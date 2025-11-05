#include "Parser.h"
#include "Tokenizer.h"
#include "Class.h"
#include "Program.h"
#include "Scope.h"
#include "Modules/ModuleID.h"
#include "Modules/IOModule.h"

static std::string g_CurrentClassName;

Parser::Parser(Program* program) :
	m_Program(program)
{
}

static char* ReadFileIntoMemoryNullTerminate(const char* path)
{
	FILE* file = fopen(path, "rb");
	fseek(file, 0, SEEK_END);
	size_t fileSize = ftell(file);
	fseek(file, 0, SEEK_SET);
	char* fileContents = (char*)malloc(fileSize + 1);
	fread(fileContents, 1, fileSize, file);
	fileContents[fileSize] = 0;
	fclose(file);
	return fileContents;
}

void Parser::Parse(const std::string& path)
{
	char* contents = ReadFileIntoMemoryNullTerminate(path.c_str());
	Tokenizer tokenizer;
	tokenizer.at = contents;

	Token token = tokenizer.GetToken();

	while (token.type != TokenTypeT::END)
	{
		if (token.type == TokenTypeT::IMPORT)
		{
			ParseImport(&tokenizer);
		}
		else if (token.type == TokenTypeT::CLASS)
		{
			ParseClass(&tokenizer);
		}

		token = tokenizer.GetToken();
	}

	free(contents);

	m_Program->Resolve();
	m_Program->EmitCode();
}

bool Parser::ParseImport(Tokenizer* tokenizer)
{
	Token token = tokenizer->GetToken();
	if (token.type == TokenTypeT::IDENTIFIER) //Built in module
	{
		std::string builtInModule(token.text, token.length);

		if (m_Program->GetModuleID(builtInModule) != INVALID_ID)
		{
			tokenizer->Expect(TokenTypeT::SEMICOLON);
			return true;
		}

		if (builtInModule == "IO") { m_Program->AddModule("IO", IO_MODULE_ID); IOModule::Init(); }

		tokenizer->Expect(TokenTypeT::SEMICOLON);
	}
	else if (token.type == TokenTypeT::STRING_LITERAL) //User defined module
	{
		
	}

	return true;
}

static void SkipStatement(Tokenizer* tokenizer)
{
	int braceDepth = 0;

	while (true)
	{
		Token t = tokenizer->GetToken();
		if (t.type == TokenTypeT::END)
			break;

		if (t.type == TokenTypeT::OPEN_BRACE)
			braceDepth++;
		else if (t.type == TokenTypeT::CLOSE_BRACE)
		{
			braceDepth--;
			if (braceDepth == 0)
				break;
		}
		else if (t.type == TokenTypeT::SEMICOLON && braceDepth == 0)
			break;
	}
}

bool Parser::ParseClass(Tokenizer* tokenizer)
{
	Token nameToken;
	if (tokenizer->Expect(TokenTypeT::IDENTIFIER, &nameToken)) return false;
	std::string className(nameToken.text, nameToken.length);

	Token openBrace;
	if (tokenizer->Expect(TokenTypeT::OPEN_BRACE, &openBrace)) return false;

	ID classScope = m_Program->CreateScope();

	Class* cls = new Class(className, classScope);
	g_CurrentClassName = className;
	m_Program->AddClass(className, cls);

	//------------------------------------------------------------
	// Pass 1: Parse all class variables (fields + static)
	//------------------------------------------------------------
	{
		Token savePos = openBrace; // remember where we started

		uint64 offset = 0;
		while (true)
		{
			Token t = tokenizer->PeekToken();
			if (t.type == TokenTypeT::CLOSE_BRACE || t.type == TokenTypeT::END)
				break;

			if (ParseClassVariable(tokenizer, cls, classScope, &offset))
				continue;

			// Skip over functions or anything we don't understand
			SkipStatement(tokenizer);
		}

		// rewind tokenizer to start of class body for second pass
		tokenizer->SetPeek(savePos);
		cls->SetSize(offset);
	}

	//------------------------------------------------------------
	// Pass 2: Parse all functions
	//------------------------------------------------------------
	bool firstIter = true;
	while (true)
	{
		Token t = firstIter ? tokenizer->GetToken() : tokenizer->PeekToken();
		firstIter = false;
		if (t.type == TokenTypeT::CLOSE_BRACE) break;
		if (t.type == TokenTypeT::END) return false;

		// Try function first
		if (ParseFunction(tokenizer, cls, classScope))
		{
			continue;
		}

		//SkipStatement(tokenizer);
		return false;
	}

	return true;
}

bool Parser::ParseFunction(Tokenizer* tokenizer, Class* cls, ID parentScope)
{
	Token t = tokenizer->GetToken();
	Function* function = new Function();

	// ---- Parse access modifier ----
	function->accessModifier = AccessModifier::PUBLIC;
	Token prev = tokenizer->PeekToken();
	if (t.type == TokenTypeT::PUBLIC)
	{
		function->accessModifier = AccessModifier::PUBLIC;
		t = tokenizer->GetToken();
	}
	else if (t.type == TokenTypeT::PRIVATE)
	{
		function->accessModifier = AccessModifier::PRIVATE;
		t = tokenizer->GetToken();
	}

	bool isDestructor = false;
	if (t.type == TokenTypeT::TILDE)
	{
		isDestructor = true;
		t = tokenizer->GetToken();
	}

	// ---- Parse static keyword ----
	function->isStatic = false;
	if (t.type == TokenTypeT::STATIC)
	{
		function->isStatic = true;
	}
	else if (t.type == TokenTypeT::VIRTUAL)
	{
		function->isVirtual = true;
	}

	// ---- Detect constructor ----
	bool isConstructor = false;
	if (!function->isStatic && !isDestructor)
	{
		Token peek = tokenizer->PeekToken();

		if (std::string(t.text, t.length) == g_CurrentClassName &&
			peek.type == TokenTypeT::OPEN_PAREN) // constructor has no return type
		{
			isConstructor = true;
		}
	}

	if (isConstructor)
	{
		function->returnInfo.type = m_Program->GetClassID(g_CurrentClassName); // constructors "return" the class
		function->name = g_CurrentClassName;
	}
	else if (isDestructor)
	{
		function->returnInfo.type = 0;
		function->name = "~" + g_CurrentClassName;
	}
	else
	{
		// ---- Normal function: parse return type + name ----
		if (function->isStatic || function->isVirtual)
			t = tokenizer->GetToken();
		else
			t = prev;

		function->returnInfo.type = ParseType(t);
		if (function->returnInfo.type == INVALID_ID)
		{
			delete function;
			return false;
		}

		uint8 pointerLevel = ParsePointerLevel(tokenizer);
		function->returnInfo.pointerLevel = pointerLevel;

		t = tokenizer->GetToken();
		if (t.type == TokenTypeT::OPERATOR)
		{
			t = tokenizer->GetToken();
			if (t.type == TokenTypeT::EQUALS)
			{
				function->name = "operator=";
			}
		}
		else if (t.type == TokenTypeT::IDENTIFIER)
		{
			function->name = std::string(t.text, t.length);
		}
		else
		{
			delete function;
			return false;
		}
	}

	if (function->name == "Main")
	{
		m_Program->SetClassWithMainFunction(m_Program->GetClassID(g_CurrentClassName));
	}

	ID functionScope = m_Program->CreateScope(parentScope);
	function->scope = functionScope;

	if(tokenizer->Expect(TokenTypeT::OPEN_PAREN, &t)) return false;

	while (true)
	{
		FunctionParameter param;
		param.type.pointerLevel = 0;

		Token typeToken = tokenizer->GetToken();
		if (typeToken.type == TokenTypeT::CLOSE_PAREN)
			break;

		param.type.type = ParseType(typeToken);
		param.type.pointerLevel = ParsePointerLevel(tokenizer);

		t = tokenizer->GetToken();
		if (t.type != TokenTypeT::IDENTIFIER) { delete function; return false; }

		std::string paramName(t.text, t.length);
		param.variableID = m_Program->GetScope(functionScope)->GetVariableID(paramName, true);
		m_Program->GetScope(functionScope)->DeclareVariable(param.variableID, param.type);

		function->parameters.push_back(param);

		t = tokenizer->GetToken();
		if (t.type == TokenTypeT::COMMA) continue;
		else if (t.type == TokenTypeT::CLOSE_PAREN) break;
		else return false;
	}

	if (tokenizer->Expect(TokenTypeT::OPEN_BRACE))
	{
		delete function; return false;
	}

	while (true)
	{
		Token peek = tokenizer->GetToken();
		if (peek.type == TokenTypeT::CLOSE_BRACE)
			break;
		tokenizer->at = peek.text; // rewind

		if (!ParseStatement(function, tokenizer, functionScope))
		{
			delete function;
			return false;
		}
	}

	cls->AddFunction(function);
}

static AccessModifier ParseAccessModifier(Tokenizer* tokenizer)
{
	Token accessModifier = tokenizer->PeekToken();
	if (accessModifier.type == TokenTypeT::PUBLIC)
	{
		tokenizer->GetToken();
		return AccessModifier::PUBLIC;
	}
	else if (accessModifier.type == TokenTypeT::PRIVATE)
	{
		tokenizer->GetToken();
		return AccessModifier::PRIVATE;
	}
	else
	{
		return AccessModifier::PUBLIC;
	}
}

bool Parser::ParseClassVariable(Tokenizer* tokenizer, Class* cls, ID parentScope, uint64* offset)
{
	AccessModifier accessModifier = ParseAccessModifier(tokenizer);

	bool isStatic = false;
	if (tokenizer->PeekToken().type == TokenTypeT::STATIC)
	{
		tokenizer->Expect(TokenTypeT::STATIC); // consume 'static'
		isStatic = true;
	}

	Token typeToken = tokenizer->GetToken();
	std::string typeName(typeToken.text, typeToken.length);
	uint16 type = ParseType(typeToken);
	if (type == INVALID_ID)
	{
		return false;
	}
	uint64 typeSize = m_Program->GetTypeSize(type);

	uint8 pointerLevel = ParsePointerLevel(tokenizer);

	if (pointerLevel > 0) typeSize = sizeof(void*);

	Token nameToken;
	if (tokenizer->Expect(TokenTypeT::IDENTIFIER, &nameToken)) return false;
	std::string name(nameToken.text, nameToken.length);

	uint32 arrayLength = 0;
	Token openBracket = tokenizer->PeekToken();
	if (openBracket.type == TokenTypeT::OPEN_BRACKET)
	{
		tokenizer->Expect(TokenTypeT::OPEN_BRACKET);
		arrayLength = ParseArrayLength(tokenizer);
		typeSize *= arrayLength;
	}

	Token equals = tokenizer->PeekToken();
	ASTExpression* initializeExpr = nullptr;

	if (equals.type == TokenTypeT::EQUALS)
	{
		tokenizer->GetToken();
		initializeExpr = ParseExpression(tokenizer, parentScope);
	}
	
	if (tokenizer->Expect(TokenTypeT::SEMICOLON)) return false;

	if (isStatic)
	{
		cls->AddStaticField(m_Program, type, pointerLevel, arrayLength, typeSize, name, initializeExpr);
	}
	else
	{
		cls->AddMemberField(type, pointerLevel, arrayLength, typeSize, *offset, name);
		*offset += typeSize;
	}
}

uint16 Parser::ParseType(const Token& t) const
{
	switch (t.type)
	{
	case TokenTypeT::IDENTIFIER: {
		std::string className(t.text, t.length);
		return m_Program->GetClassID(className);
	} break;

	case TokenTypeT::VOID_T: return (uint16)ValueType::VOID_T;
	case TokenTypeT::BOOL: return (uint16)ValueType::BOOL;
	case TokenTypeT::CHAR: return (uint16)ValueType::CHAR;
	case TokenTypeT::UINT8: return (uint16)ValueType::UINT8;
	case TokenTypeT::UINT16: return (uint16)ValueType::UINT16;
	case TokenTypeT::UINT32: return (uint16)ValueType::UINT32;
	case TokenTypeT::UINT64: return (uint16)ValueType::UINT64;
	case TokenTypeT::INT8: return (uint16)ValueType::INT8;
	case TokenTypeT::INT16: return (uint16)ValueType::INT16;
	case TokenTypeT::INT32: return (uint16)ValueType::INT32;
	case TokenTypeT::INT64: return (uint16)ValueType::INT64;
	case TokenTypeT::REAL32: return (uint16)ValueType::REAL32;
	case TokenTypeT::REAL64: return (uint16)ValueType::REAL64;
	case TokenTypeT::STRING: return (uint16)ValueType::STRING;
	default: return INVALID_ID;
	}
}

uint8 Parser::ParsePointerLevel(Tokenizer* tokenizer) const
{
	Token pointer = tokenizer->PeekToken();
	uint8 pointerLevel = 0;
	while (pointer.type == TokenTypeT::ASTERISK)
	{
		pointerLevel++;
		tokenizer->Expect(TokenTypeT::ASTERISK);
		pointer = tokenizer->PeekToken();
	}

	return pointerLevel;
}

bool Parser::ParseStatement(Function* function, Tokenizer* tokenizer, ID currentScope)
{
	Token t = tokenizer->GetToken();

	bool declaringPrimitive = false;
	ValueType primitiveType = ValueType::VOID_T;
	if (t.type == TokenTypeT::IDENTIFIER)
	{
		std::string identifier(t.text, t.length);
		Token next = tokenizer->GetToken();
		if (next.type == TokenTypeT::IDENTIFIER) //Declaring user defined type
		{
			uint16 type = m_Program->GetClassID(identifier);
			std::string name(next.text, next.length);
			ID variableID = m_Program->GetScope(currentScope)->GetVariableID(name, true);
			m_Program->GetScope(currentScope)->DeclareVariable(variableID, TypeInfo(type, 0));

			next = tokenizer->GetToken();
			if (next.type == TokenTypeT::SEMICOLON)
			{
				std::vector<ASTExpression*> argExprs(0);
				ASTExpressionDeclareObject* declareObjectExpr = new ASTExpressionDeclareObject(currentScope, type, variableID, argExprs);
				function->body.push_back(declareObjectExpr);
				return true;
			}
			else if (next.type == TokenTypeT::OPEN_PAREN)
			{
				std::vector<ASTExpression*> argExprs;
				ParseArguments(tokenizer, currentScope, argExprs);
				if (!tokenizer->Expect(TokenTypeT::SEMICOLON)) return false;
				ASTExpressionDeclareObject* declareObjectExpr = new ASTExpressionDeclareObject(currentScope, type, variableID, argExprs);
				function->body.push_back(declareObjectExpr);
				return true;
			}
			else if (next.type == TokenTypeT::EQUALS)
			{
				ASTExpression* assignExpr = ParseExpression(tokenizer, currentScope);
				if (tokenizer->Expect(TokenTypeT::SEMICOLON)) return false;
				ASTExpressionDeclareObjectAssign* declareObjectExpr = new ASTExpressionDeclareObjectAssign(currentScope, type, variableID, assignExpr);
				function->body.push_back(declareObjectExpr);
				return true;
			}
			else if (next.type == TokenTypeT::OPEN_BRACKET)
			{
				uint32 arrayLength = ParseArrayLength(tokenizer);
				Token peek = tokenizer->PeekToken();
				if (peek.type == TokenTypeT::EQUALS)
				{
					tokenizer->Expect(TokenTypeT::EQUALS);
					std::vector<ASTExpression*> arrayInitializeExprs;
					ParseArrayInitializer(tokenizer, currentScope, arrayInitializeExprs);
					if (tokenizer->Expect(TokenTypeT::SEMICOLON)) return false;
					ASTExpressionStackArrayDeclare* arrayDeclareExpr = new ASTExpressionStackArrayDeclare(currentScope, type, 0, variableID, arrayLength, arrayInitializeExprs);
					function->body.push_back(arrayDeclareExpr);
					return true;
				}
				else if(peek.type == TokenTypeT::SEMICOLON)
				{
					tokenizer->Expect(TokenTypeT::SEMICOLON);
					std::vector<ASTExpression*> arrayInitializeExprs(0);
					ASTExpressionStackArrayDeclare* arrayDeclareExpr = new ASTExpressionStackArrayDeclare(currentScope, type, 0, variableID, arrayLength, arrayInitializeExprs);
					function->body.push_back(arrayDeclareExpr);
					return true;
				}
				else
				{
					return false;
				}
			}
		}
		else
		{
			tokenizer->at = t.text; // rewind
			ASTExpression* expr = ParseExpression(tokenizer, currentScope);
			expr->isStatement = true;
			tokenizer->Expect(TokenTypeT::SEMICOLON);
			function->body.push_back(expr);
			return true;
		}
	}
	else if (t.type == TokenTypeT::UINT8)
	{
		declaringPrimitive = true;
		primitiveType = ValueType::UINT8;
	}
	else if (t.type == TokenTypeT::UINT16)
	{
		declaringPrimitive = true;
		primitiveType = ValueType::UINT16;
	}
	else if (t.type == TokenTypeT::UINT32)
	{
		declaringPrimitive = true;
		primitiveType = ValueType::UINT32;
	}
	else if (t.type == TokenTypeT::UINT64)
	{
		declaringPrimitive = true;
		primitiveType = ValueType::UINT64;
	}
	else if (t.type == TokenTypeT::INT8)
	{
		declaringPrimitive = true;
		primitiveType = ValueType::INT8;
	}
	else if (t.type == TokenTypeT::INT16)
	{
		declaringPrimitive = true;
		primitiveType = ValueType::INT16;
	}
	else if (t.type == TokenTypeT::INT32)
	{
		declaringPrimitive = true;
		primitiveType = ValueType::INT32;
	}
	else if (t.type == TokenTypeT::INT64)
	{
		declaringPrimitive = true;
		primitiveType = ValueType::INT64;
	}
	else if (t.type == TokenTypeT::REAL32)
	{
		declaringPrimitive = true;
		primitiveType = ValueType::REAL32;
	}
	else if (t.type == TokenTypeT::REAL64)
	{
		declaringPrimitive = true;
		primitiveType = ValueType::REAL64;
	}
	else if (t.type == TokenTypeT::CHAR)
	{
		declaringPrimitive = true;
		primitiveType = ValueType::CHAR;
	}
	else if (t.type == TokenTypeT::BOOL)
	{
		declaringPrimitive = true;
		primitiveType = ValueType::BOOL;
	}
	else if (t.type == TokenTypeT::STRING)
	{
		declaringPrimitive = true;
		primitiveType = ValueType::STRING;
	}
	else
	{
		tokenizer->at = t.text; // rewind
		ASTExpression* expr = ParseExpression(tokenizer, currentScope);
		expr->isStatement = true;
		tokenizer->Expect(TokenTypeT::SEMICOLON);
		function->body.push_back(expr);
		return true;
	}

	if (declaringPrimitive)
	{
		uint8 pointerLevel = ParsePointerLevel(tokenizer);
		Token nameToken;
		if(tokenizer->Expect(TokenTypeT::IDENTIFIER, &nameToken)) return false;
		std::string name(nameToken.text, nameToken.length);

		ID variableID = m_Program->GetScope(currentScope)->GetVariableID(name, true);

		Token next = tokenizer->GetToken();
		if (next.type == TokenTypeT::EQUALS)
		{
			m_Program->GetScope(currentScope)->DeclareVariable(variableID, TypeInfo((uint16)primitiveType, pointerLevel));
			ASTExpression* assignExpr = ParseExpression(tokenizer, currentScope);
			tokenizer->Expect(TokenTypeT::SEMICOLON);
			if (pointerLevel > 0)
			{
				ASTExpressionDeclarePointer* declarePointerExpr = new ASTExpressionDeclarePointer(currentScope, variableID, (uint16)primitiveType, pointerLevel, assignExpr);
				function->body.push_back(declarePointerExpr);
				return true;
			}

			ASTExpressionDeclare* declareExpr = new ASTExpressionDeclare(currentScope, variableID, (uint16)primitiveType, assignExpr);
			function->body.push_back(declareExpr);
			return true;
		}
		else if (next.type == TokenTypeT::SEMICOLON)
		{
			m_Program->GetScope(currentScope)->DeclareVariable(variableID, TypeInfo((uint16)primitiveType, pointerLevel));
			if (pointerLevel > 0)
			{
				ASTExpressionDeclarePointer* declarePointerExpr = new ASTExpressionDeclarePointer(currentScope, variableID, (uint16)primitiveType, pointerLevel, nullptr);
				function->body.push_back(declarePointerExpr);
				return true;
			}

			ASTExpressionDeclare* declareExpr = new ASTExpressionDeclare(currentScope, variableID, (uint16)primitiveType, nullptr);
			function->body.push_back(declareExpr);
			return true;
		}
		else if (next.type == TokenTypeT::OPEN_BRACKET)
		{
			uint32 length = ParseArrayLength(tokenizer);
			Token token = tokenizer->GetToken();

			m_Program->GetScope(currentScope)->DeclareVariable(variableID, TypeInfo((uint16)primitiveType, 1, pointerLevel));

			if (token.type == TokenTypeT::EQUALS)
			{
				std::vector<ASTExpression*> initializeExprs;
				ParseArrayInitializer(tokenizer, currentScope, initializeExprs);
				tokenizer->Expect(TokenTypeT::SEMICOLON);
				ASTExpressionStackArrayDeclare* arrayDeclareExpr = new ASTExpressionStackArrayDeclare(currentScope, (uint16)primitiveType, pointerLevel, variableID, length, initializeExprs);
				function->body.push_back(arrayDeclareExpr);
				return true;
			}
			else if (token.type == TokenTypeT::SEMICOLON)
			{
				std::vector<ASTExpression*> initializeExprs(0);
				ASTExpressionStackArrayDeclare* arrayDeclareExpr = new ASTExpressionStackArrayDeclare(currentScope, (uint16)primitiveType, pointerLevel, variableID, length, initializeExprs);
				function->body.push_back(arrayDeclareExpr);
				return true;
			}
			else
			{
				return false;
			}
		}
		else
		{
			return false;
		}
	}
}

ASTExpression* Parser::ParseExpression(Tokenizer* tokenizer, ID currentScope)
{
	ASTExpression* lhs = ParseUnary(tokenizer, currentScope);
	if (!lhs) return nullptr;
	return ParseBinaryOpRHS(0, lhs, tokenizer, currentScope);
}

static int32 GetPrecedence(const Token& token)
{
	switch (token.type)
	{
	case TokenTypeT::ASTERISK:
	case TokenTypeT::SLASH:
	case TokenTypeT::MOD:
		return 20;

	case TokenTypeT::PLUS:
	case TokenTypeT::MINUS:
		return 10;

	case TokenTypeT::BITSHIFT_LEFT:
	case TokenTypeT::BITSHIFT_RIGHT:
		return 9;

	case TokenTypeT::LESS:
	case TokenTypeT::LESS_EQUALS:
	case TokenTypeT::GREATER:
	case TokenTypeT::GREATER_EQUALS:
		return 8;

	case TokenTypeT::EQUALS_EQUALS:
	case TokenTypeT::NOT_EQUAL:
		return 7;

	case TokenTypeT::AND:
		return 6;

	case TokenTypeT::PIPE:
		return 5;

	case TokenTypeT::LOGICAL_AND:
		return 3;

	case TokenTypeT::LOGICAL_OR:
		return 2;

	default:
		return -1;
	}
}

ASTExpression* Parser::ParseBinaryOpRHS(int32 exprPrec, ASTExpression* lhs, Tokenizer* tokenizer, ID currentScope)
{
	while (true)
	{
		Token opToken = tokenizer->PeekToken();  // lookahead, dont consume yet
		int tokenPrec = GetPrecedence(opToken);

		if (tokenPrec < exprPrec)
			return lhs;

		// Now we know it's a valid operator, consume it
		tokenizer->GetToken();

		ASTExpression* rhs = ParseUnary(tokenizer, currentScope);
		if (!rhs) return nullptr;

		// If the next operator has higher precedence, handle it first
		Token nextOp = tokenizer->PeekToken();
		int nextPrec = GetPrecedence(nextOp);

		if (tokenPrec < nextPrec)
		{
			rhs = ParseBinaryOpRHS(tokenPrec + 1, rhs, tokenizer, currentScope);
			if (!rhs) return nullptr;
		}

		ASTOperator opEnum;
		switch (opToken.type)
		{
		case TokenTypeT::PLUS:          opEnum = ASTOperator::ADD; break;
		case TokenTypeT::MINUS:         opEnum = ASTOperator::MINUS; break;
		case TokenTypeT::ASTERISK:      opEnum = ASTOperator::MULTIPLY; break;
		case TokenTypeT::SLASH:         opEnum = ASTOperator::DIVIDE; break;
		case TokenTypeT::MOD:		   opEnum = ASTOperator::MOD; break;

		case TokenTypeT::LESS:          opEnum = ASTOperator::LESS; break;
		case TokenTypeT::LESS_EQUALS:    opEnum = ASTOperator::LESS_EQUALS; break;
		case TokenTypeT::GREATER:       opEnum = ASTOperator::GREATER; break;
		case TokenTypeT::GREATER_EQUALS: opEnum = ASTOperator::GREATER_EQUALS; break;
		case TokenTypeT::EQUALS_EQUALS:   opEnum = ASTOperator::EQUALS; break;
		case TokenTypeT::NOT_EQUAL:    opEnum = ASTOperator::NOT_EQUALS; break;

		case TokenTypeT::LOGICAL_AND: opEnum = ASTOperator::LOGICAL_AND; break;
		case TokenTypeT::LOGICAL_OR: opEnum = ASTOperator::LOGICAL_OR; break;

		case TokenTypeT::AND: opEnum = ASTOperator::BITWISE_AND; break;
		case TokenTypeT::PIPE: opEnum = ASTOperator::BITWISE_OR; break;
		case TokenTypeT::BITSHIFT_LEFT: opEnum = ASTOperator::BITSHIFT_LEFT; break;
		case TokenTypeT::BITSHIFT_RIGHT: opEnum = ASTOperator::BITSHIFT_RIGHT; break;

		default: return lhs; // shouldnt happen
		}

		lhs = new ASTExpressionBinary(currentScope, lhs, rhs, opEnum);
	}
}

ASTExpression* Parser::ParseUnary(Tokenizer* tokenizer, ID currentScope)
{
	Token tok = tokenizer->PeekToken();

	switch (tok.type)
	{
	case TokenTypeT::ASTERISK: { // dereference
		tokenizer->Expect(TokenTypeT::ASTERISK);
		ASTExpression* expr = ParseExpression(tokenizer, currentScope);
		ASTExpressionDereference* dereferenceExpr = new ASTExpressionDereference(currentScope, expr);
		return dereferenceExpr;
	} break;
	case TokenTypeT::AND: { // address-of
		tokenizer->Expect(TokenTypeT::AND);
		ASTExpression* expr = ParseExpression(tokenizer, currentScope);
		ASTExpressionAddressOf* addressOfExpr = new ASTExpressionAddressOf(currentScope, expr);
		return addressOfExpr;
	} break;
	case TokenTypeT::PLUS_PLUS: {
		
	} break;
	case TokenTypeT::MINUS_MINUS: {
		
	} break;
	case TokenTypeT::NOT: { // logical not
		
	} break;
	case TokenTypeT::MINUS: { // unary negation
		
	}
	default:
		return ParsePostFix(tokenizer, currentScope);
	}
}

ASTExpression* Parser::ParsePostFix(Tokenizer* tokenizer, ID currentScope)
{
	ASTExpression* expr = ParsePrimary(tokenizer, currentScope);

	while (true)
	{
		Token tok = tokenizer->PeekToken();

		if (tok.type == TokenTypeT::PLUS_PLUS)
		{
			tokenizer->GetToken();
			
		}
		else if (tok.type == TokenTypeT::MINUS_MINUS)
		{
			tokenizer->GetToken();
			
		}
		else if (tok.type == TokenTypeT::PLUS_EQUALS)
		{
			tokenizer->GetToken();
			ASTExpression* amountExpr = ParseExpression(tokenizer, currentScope);
			
		}
		else if (tok.type == TokenTypeT::MINUS_EQUALS)
		{
			tokenizer->GetToken();
			ASTExpression* amountExpr = ParseExpression(tokenizer, currentScope);
			
		}
		else if (tok.type == TokenTypeT::TIMES_EQUALS)
		{
			tokenizer->GetToken();
			ASTExpression* amountExpr = ParseExpression(tokenizer, currentScope);
			
		}
		else if (tok.type == TokenTypeT::DIVIDE_EQUALS)
		{
			tokenizer->GetToken();
			ASTExpression* amountExpr = ParseExpression(tokenizer, currentScope);
			
		}
		else if (tok.type == TokenTypeT::MOD_EQUALS)
		{
			tokenizer->GetToken();
			ASTExpression* amountExpr = ParseExpression(tokenizer, currentScope);
		}
		else
		{
			break;
		}
	}

	return expr;
}

static ASTExpressionModuleFunctionCall* MakeModuleFunctionCall(ID scope, ID moduleID, const std::string& moduleName, const std::string& functionName, const std::vector<ASTExpression*>& args)
{
	uint32 function = 0;

	if (moduleName == "IO")
	{
		if (functionName == "Println")		function = (uint32)IOModuleFunction::PRINTLN;
		else if (functionName == "Print")	function = (uint32)IOModuleFunction::PRINT;
	}

	ASTExpressionModuleFunctionCall* functionCallExpr = new ASTExpressionModuleFunctionCall(scope, moduleID, function, args);
	return functionCallExpr;
}

static ASTExpressionModuleConstant* MakeModuleConstant(ID scope, ID moduleID, const std::string& moduleName, const std::string& constantName)
{
	uint16 constant = 0;

	if (moduleName == "IO")
	{

	}
	else if (moduleName == "Math")
	{

	}

	ASTExpressionModuleConstant* moduleConstantExpr = new ASTExpressionModuleConstant(scope, moduleID, constant);
	return moduleConstantExpr;
}

ASTExpression* Parser::ParsePrimary(Tokenizer* tokenizer, ID currentScope)
{
	Token t = tokenizer->GetToken();

	if (t.type == TokenTypeT::NUMBER_LITERAL)
	{
		std::string numStr(t.text, t.length);
		if (numStr.find('.') != std::string::npos)
		{
			ASTExpressionLiteral* literalExpr = new ASTExpressionLiteral(currentScope, Value::MakeReal64(std::stod(numStr), m_Program->GetStackAllocator()));
			return literalExpr;
		}
		else
		{
			ASTExpressionLiteral* literalExpr = new ASTExpressionLiteral(currentScope, Value::MakeInt64(std::stoll(numStr), m_Program->GetStackAllocator()));
			return literalExpr;
		}
	}
	else if (t.type == TokenTypeT::STRING_LITERAL)
	{
		std::string str(t.text, t.length);
		for (size_t i = 0; i < str.size(); ++i)
		{
			if (str[i] == '\\' && i + 1 < str.size())
			{
				switch (str[i + 1])
				{
				case 'n':  str.replace(i, 2, "\n");  break;
				case 't':  str.replace(i, 2, "\t");  break;
				case 'r':  str.replace(i, 2, "\r");  break;
				case '\\': str.replace(i, 2, "\\");  break;
				case '"':  str.replace(i, 2, "\"");  break;
				default: continue;
				}
			}
		}

		ASTExpressionLiteral* literalExpr = new ASTExpressionLiteral(currentScope, Value::MakeString(str, m_Program->GetHeapAllocator()));
		return literalExpr;
	}
	else if (t.type == TokenTypeT::CHAR_LITERAL)
	{
		char c;

		if (t.length == 1)
		{
			c = t.text[0];
		}
		else if (t.length == 2 && t.text[0] == '\\')
		{
			switch (t.text[1])
			{
			case 'n':  c = '\n'; break;
			case 't':  c = '\t'; break;
			case 'r':  c = '\r'; break;
			case '\\': c = '\\'; break;
			case '\'': c = '\''; break;
			case '"':  c = '"';  break;
			default:   c = t.text[1]; break; // unknown escape, use raw
			}
		}
		else
		{
			// fallback -> invalid char literal
			c = '?';
		}

		ASTExpressionLiteral* literalExpr = new ASTExpressionLiteral(currentScope, Value::MakeChar(c, m_Program->GetStackAllocator()));

		return literalExpr;
	}
	else if (t.type == TokenTypeT::TRUE_T)
	{
		ASTExpressionLiteral* literalExpr = new ASTExpressionLiteral(currentScope, Value::MakeBool(true, m_Program->GetStackAllocator()));
		return literalExpr;
	}
	else if (t.type == TokenTypeT::FALSE_T)
	{
		ASTExpressionLiteral* literalExpr = new ASTExpressionLiteral(currentScope, Value::MakeBool(false, m_Program->GetStackAllocator()));
		return literalExpr;
	}
	else if (t.type == TokenTypeT::NULLPTR)
	{
		ASTExpressionLiteral* literalExpr = new ASTExpressionLiteral(currentScope, Value::MakeNULL());
		return literalExpr;
	}
	else if (t.type == TokenTypeT::OPEN_PAREN)
	{
		ASTExpression* expr = ParseExpression(tokenizer, currentScope);
		tokenizer->Expect(TokenTypeT::CLOSE_PAREN);
		return expr;
	}
	else if (t.type == TokenTypeT::RETURN)
	{
		Token peek = tokenizer->PeekToken();
		if (peek.type == TokenTypeT::SEMICOLON)
		{
			ASTExpressionReturn* returnExpr = new ASTExpressionReturn(currentScope, nullptr);
			return returnExpr;
		}
		else
		{
			ASTExpression* expr = ParseExpression(tokenizer, currentScope);
			ASTExpressionReturn* returnExpr = new ASTExpressionReturn(currentScope, expr);
			return returnExpr;
		}
	}
	else if (t.type == TokenTypeT::NEW)
	{
		Token typeToken = tokenizer->GetToken();
		uint8 pointerLevel = ParsePointerLevel(tokenizer);
		Token peek = tokenizer->PeekToken();
		if (peek.type == TokenTypeT::OPEN_BRACKET)
		{
			tokenizer->Expect(TokenTypeT::OPEN_BRACKET);
			ASTExpression* sizeExpr = ParseExpression(tokenizer, currentScope);
			tokenizer->Expect(TokenTypeT::CLOSE_BRACKET);

			uint16 type = INVALID_ID;
			switch (typeToken.type)
			{
			case TokenTypeT::UINT8: type = (uint16)ValueType::UINT8; break;
			case TokenTypeT::UINT16: type = (uint16)ValueType::UINT16; break;
			case TokenTypeT::UINT32: type = (uint16)ValueType::UINT32; break;
			case TokenTypeT::UINT64: type = (uint16)ValueType::UINT64; break;
			case TokenTypeT::INT8: type = (uint16)ValueType::INT8; break;
			case TokenTypeT::INT16: type = (uint16)ValueType::INT16; break;
			case TokenTypeT::INT32: type = (uint16)ValueType::INT32; break;
			case TokenTypeT::INT64: type = (uint16)ValueType::INT64; break;
			case TokenTypeT::REAL32: type = (uint16)ValueType::REAL32; break;
			case TokenTypeT::REAL64: type = (uint16)ValueType::REAL64; break;
			case TokenTypeT::CHAR: type = (uint16)ValueType::CHAR; break;
			case TokenTypeT::BOOL: type = (uint16)ValueType::BOOL; break;
			case TokenTypeT::STRING: type = (uint16)ValueType::STRING; break;
			case TokenTypeT::IDENTIFIER: {
				std::string typeName(typeToken.text, typeToken.length);
				type = m_Program->GetClassID(typeName);
			} break;
			}

			if (type == INVALID_ID)
			{
				return nullptr;
			}

			ASTExpressionNewArray* newArrayExpr = new ASTExpressionNewArray(currentScope, type, pointerLevel, sizeExpr);
			return newArrayExpr;
		}
	}
	else if (t.type == TokenTypeT::IDENTIFIER)
	{
		Token next = tokenizer->GetToken();
		if (next.type == TokenTypeT::OPEN_PAREN) //Function call
		{
			std::string functionName(t.text, t.length);
			ID classID = m_Program->GetClassID(g_CurrentClassName);
			std::vector<ASTExpression*> argExprs;
			ParseArguments(tokenizer, currentScope, argExprs);

			ASTExpressionStaticFunctionCall* functionCall = new ASTExpressionStaticFunctionCall(currentScope, classID, functionName, argExprs);
			return functionCall;
		}
		else if (next.type == TokenTypeT::DOT)
		{
			if(tokenizer->Expect(TokenTypeT::IDENTIFIER, &next)) return nullptr;

			std::vector<std::string> members;
			members.push_back(std::string(next.text, next.length));

			Token subMember = tokenizer->PeekToken();
			while (subMember.type == TokenTypeT::DOT)
			{
				tokenizer->SetPeek(subMember);
				tokenizer->GetToken();
				subMember = tokenizer->GetToken();
				if (subMember.type != TokenTypeT::IDENTIFIER) return nullptr;
				members.push_back(std::string(subMember.text, subMember.length));
				subMember = tokenizer->PeekToken();
			}

			Token peek = tokenizer->PeekToken();
			if (peek.type != TokenTypeT::OPEN_PAREN) //Member or static variable
			{
				std::string className(t.text, t.length);
				ID classID = m_Program->GetClassID(className);
				ID moduleID = m_Program->GetModuleID(className);

				ASTExpression* indexExpr = nullptr;
				if (peek.type == TokenTypeT::OPEN_BRACKET)
				{
					tokenizer->Expect(TokenTypeT::OPEN_BRACKET);
					indexExpr = ParseExpression(tokenizer, currentScope);
					if (tokenizer->Expect(TokenTypeT::CLOSE_BRACKET)) return nullptr;
				}

				if (moduleID != INVALID_ID)
				{
					ASTExpressionModuleConstant* moduleConstantExpr = MakeModuleConstant(currentScope, moduleID, className, members.back());
					return moduleConstantExpr;
				}
				else if (classID != INVALID_ID)
				{
					Token equals = tokenizer->PeekToken();
					ASTExpression* assignExpr = nullptr;
					if (equals.type == TokenTypeT::EQUALS)
					{
						tokenizer->GetToken();
						assignExpr = ParseExpression(tokenizer, currentScope);
					}

					if (members.size() == 1)
					{
						std::string memberName = members[0];
						ID classScope = m_Program->GetClass(classID)->GetScopeID();
						ID variableID = m_Program->GetScope(classScope)->GetVariableID(memberName, false);
						if (assignExpr)
						{
							ASTExpressionVariableSet* variableSetExpr = new ASTExpressionVariableSet(classScope, variableID, assignExpr);
							return variableSetExpr;
						}
						else
						{
							ASTExpressionVariable* variableExpr = new ASTExpressionVariable(classScope, variableID);
							return variableExpr;
						}
					}
				}
				else
				{
					Scope* scope = m_Program->GetScope(currentScope);
					std::string memberName = className;
					ID variableID = scope->GetVariableID(memberName, false);
					if (variableID == INVALID_ID)
					{
						ID thisID = m_Program->GetClassID(g_CurrentClassName);
						TypeInfo memberTypeInfo;
						members.insert(members.begin(), memberName);
						uint64 offset = m_Program->GetClass(thisID)->CalculateOffset(members, &memberTypeInfo);
						if (offset == UINT64_MAX) return nullptr;

						Token equals = tokenizer->PeekToken();
						if (equals.type == TokenTypeT::EQUALS)
						{
							tokenizer->Expect(TokenTypeT::EQUALS);
							ASTExpression* assignExpr = ParseExpression(tokenizer, currentScope);
							ASTExpressionDirectMemberAccess* memberAccessExpr = new ASTExpressionDirectMemberAccess(currentScope, memberTypeInfo, offset, assignExpr);
							return memberAccessExpr;
						}
						else
						{
							ASTExpressionDirectMemberAccess* memberAccessExpr = new ASTExpressionDirectMemberAccess(currentScope, memberTypeInfo, offset, nullptr);
							return memberAccessExpr;
						}
					}

					TypeInfo variableTypeInfo = scope->GetVariableTypeInfo(variableID);
					TypeInfo memberTypeInfo;
					uint64 offset = m_Program->GetClass(variableTypeInfo.type)->CalculateOffset(members, &memberTypeInfo);

					Token equals = tokenizer->PeekToken();
					if (equals.type == TokenTypeT::EQUALS)
					{
						tokenizer->Expect(TokenTypeT::EQUALS);
						ASTExpression* assignExpr = ParseExpression(tokenizer, currentScope);
						ASTExpressionMemberSet* memberSetExpr = new ASTExpressionMemberSet(currentScope, variableID, offset, memberTypeInfo.type, memberTypeInfo.pointerLevel, assignExpr, 0);
						return memberSetExpr;
					}
					else
					{
						ASTExpressionMemberAccess* memberAccessExpr = new ASTExpressionMemberAccess(currentScope, variableID, offset, memberTypeInfo.type, memberTypeInfo.pointerLevel, 0);
						return memberAccessExpr;
					}
				}
			}

			std::vector<ASTExpression*> expressions;
			std::string functionCall = members.back();
			if (tokenizer->Expect(TokenTypeT::OPEN_PAREN, &next)) return nullptr;
			ParseArguments(tokenizer, currentScope, expressions);

			std::string className(t.text, t.length);
			ID moduleID = m_Program->GetModuleID(className);
			ID classID = m_Program->GetClassID(className);
			if (moduleID != INVALID_ID)
			{
				ASTExpressionModuleFunctionCall* functionCallExpr = MakeModuleFunctionCall(currentScope, moduleID, className, functionCall, expressions);
				return functionCallExpr;
			}
			else if (classID != INVALID_ID)
			{
				ASTExpressionStaticFunctionCall* functionCallExpr = new ASTExpressionStaticFunctionCall(currentScope, classID, functionCall, expressions);
				return functionCallExpr;
			}
			else
			{
				ASTExpression* objExpr = nullptr;
				std::string functionName = members.back();
				members.pop_back();
				if (members.empty())
				{
					std::string variableName(t.text, t.length);
					ID variableID = m_Program->GetScope(currentScope)->GetVariableID(variableName, false);
					objExpr = new ASTExpressionVariable(currentScope, variableID);
				}
				else
				{
					std::string variableName(t.text, t.length);
					ID variableID = m_Program->GetScope(currentScope)->GetVariableID(variableName, false);
					TypeInfo variableTypeInfo = m_Program->GetScope(currentScope)->GetVariableTypeInfo(variableID);
					TypeInfo memberTypeInfo;
					uint64 offset = m_Program->GetClass(variableTypeInfo.type)->CalculateOffset(members, &memberTypeInfo);
					objExpr = new ASTExpressionMemberAccess(currentScope, variableID, offset, memberTypeInfo.type, memberTypeInfo.pointerLevel, 0);
				}

				ASTExpressionMemberFunctionCall* functionCallExpr = new ASTExpressionMemberFunctionCall(currentScope, objExpr, functionName, expressions);
				return functionCallExpr;
			}
		}
		else if (next.type == TokenTypeT::EQUALS)
		{
			std::string variableName(t.text, t.length);
			ID variableID = m_Program->GetScope(currentScope)->GetVariableID(variableName, false);
			if (variableID == INVALID_ID)
			{
				ASTExpression* assignExpr = ParseExpression(tokenizer, currentScope);
				ID classID = m_Program->GetClassID(g_CurrentClassName);
				Class* thisClass = m_Program->GetClass(classID);
				std::vector<std::string> members;
				members.push_back(variableName);
				TypeInfo memberTypeInfo;
				uint64 offset = thisClass->CalculateOffset(members, &memberTypeInfo);
				ASTExpressionDirectMemberAccess* directMemberAccessExpr = new ASTExpressionDirectMemberAccess(currentScope, memberTypeInfo, offset, assignExpr);
				return directMemberAccessExpr;
			}
			else
			{
				ASTExpression* assignExpr = ParseExpression(tokenizer, currentScope);
				ASTExpressionVariableSet* variableSetExpr = new ASTExpressionVariableSet(currentScope, variableID, assignExpr);
				return variableSetExpr;
			}
		}
		else if (next.type == TokenTypeT::OPEN_BRACKET)
		{
			ASTExpression* indexExpr = ParseExpression(tokenizer, currentScope);
			tokenizer->Expect(TokenTypeT::CLOSE_BRACKET);

			Token peek = tokenizer->PeekToken();
			ASTExpression* assignExpr = nullptr;
			if (peek.type == TokenTypeT::EQUALS)
			{
				tokenizer->Expect(TokenTypeT::EQUALS);
				assignExpr = ParseExpression(tokenizer, currentScope);
			}
			else if (peek.type == TokenTypeT::DOT)
			{

			}

			std::string variableName(t.text, t.length);
			ID variableID = m_Program->GetScope(currentScope)->GetVariableID(variableName, false);
			if (variableID == INVALID_ID)
			{
				return nullptr;
			}
			else
			{
				ASTExpressionVariable* variableExpr = new ASTExpressionVariable(currentScope, variableID);
				ASTExpressionIndex* indexExpr_ = new ASTExpressionIndex(currentScope, variableExpr, indexExpr, assignExpr);
				return indexExpr_;
			}
		}
		else
		{
			std::string variableName(t.text, t.length);
			ID variableID = m_Program->GetScope(currentScope)->GetVariableID(variableName, false);
			if (variableID == INVALID_ID)
			{
				ID classID = m_Program->GetClassID(g_CurrentClassName);
				Class* thisClass = m_Program->GetClass(classID);
				std::vector<std::string> members;
				members.push_back(variableName);
				TypeInfo memberTypeInfo;
				uint64 offset = thisClass->CalculateOffset(members, &memberTypeInfo);
				if (offset == UINT64_MAX) return nullptr;

				tokenizer->at = next.text; // rewind, it's a variable
				ASTExpressionDirectMemberAccess* directMemberAccessExpr = new ASTExpressionDirectMemberAccess(currentScope, memberTypeInfo, offset, nullptr);
				return directMemberAccessExpr;
			}
			else
			{
				tokenizer->at = next.text; // rewind, it's a variable
				ASTExpressionVariable* variableExpr = new ASTExpressionVariable(currentScope, variableID);
				return variableExpr;
			}
		}
	}
}

void Parser::ParseArguments(Tokenizer* tokenizer, ID currentScope, std::vector<ASTExpression*>& args)
{
	while (true)
	{
		Token peek = tokenizer->PeekToken();
		if (peek.type == TokenTypeT::CLOSE_PAREN)
		{
			tokenizer->GetToken();
			break;
		}

		ASTExpression* argExpr = ParseExpression(tokenizer, currentScope);
		args.push_back(argExpr);
		Token next = tokenizer->GetToken();
		if (next.type == TokenTypeT::COMMA) continue;
		if (next.type == TokenTypeT::CLOSE_PAREN) break;
	}
}

uint32 Parser::ParseArrayLength(Tokenizer* tokenizer)
{
	Token dimensionToken = tokenizer->GetToken();
	std::string dimensionTokenText(dimensionToken.text, dimensionToken.length);
	uint32 dimension = std::stol(dimensionTokenText);
	tokenizer->Expect(TokenTypeT::CLOSE_BRACKET);
	return dimension;
}

void Parser::ParseArrayInitializer(Tokenizer* tokenizer, ID currentScope, std::vector<ASTExpression*>& initializeExprs)
{
	tokenizer->Expect(TokenTypeT::OPEN_BRACE);

	while (true)
	{
		Token token = tokenizer->PeekToken();
		if (token.type == TokenTypeT::CLOSE_BRACE)
			break;

		ASTExpression* initializeExpr = ParseExpression(tokenizer, currentScope);
		initializeExprs.push_back(initializeExpr);
		token = tokenizer->GetToken();
		if (token.type == TokenTypeT::COMMA) continue;
		if (token.type == TokenTypeT::CLOSE_BRACE) break;
	}
}
