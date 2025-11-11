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

	m_Program->BuildVTables();
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

	TemplateDefinition templateDefinition;
	Token arrowToken = tokenizer->PeekToken();
	Class* baseClass = nullptr;
	while (arrowToken.type == TokenTypeT::ARROW)
	{
		tokenizer->Expect(TokenTypeT::ARROW);
		Token classExtensionToken = tokenizer->GetToken();
		if (classExtensionToken.type == TokenTypeT::TEMPLATE)
		{
			if (tokenizer->Expect(TokenTypeT::OPEN_BRACKET)) return false;
			while (true)
			{
				TemplateParameter param;
				Token templateTypeToken = tokenizer->GetToken();
				if (templateTypeToken.type == TokenTypeT::CLASS)
				{
					Token templateTypeNameToken;
					if (tokenizer->Expect(TokenTypeT::IDENTIFIER, &templateTypeNameToken)) return false;

					std::string templateTypeName(templateTypeNameToken.text, templateTypeNameToken.length);
					param.type = TemplateParameterType::TYPE;
					param.name = templateTypeName;
				}
				else if (templateTypeToken.type == TokenTypeT::UINT32)
				{
					Token templateIntNameToken;
					if (tokenizer->Expect(TokenTypeT::IDENTIFIER, &templateIntNameToken)) return false;

					std::string templateIntName(templateIntNameToken.text, templateIntNameToken.length);
					param.type = TemplateParameterType::INT;
					param.name = templateIntName;
				}
				else
				{
					return false;
				}

				templateDefinition.parameters.push_back(param);

				Token commaToken = tokenizer->GetToken();
				if (commaToken.type == TokenTypeT::CLOSE_BRACKET)
					break;
			}
		}
		else if (classExtensionToken.type == TokenTypeT::INHERIT)
		{
			if (tokenizer->Expect(TokenTypeT::OPEN_BRACKET)) return false;
			Token inheritToken;
			if (tokenizer->Expect(TokenTypeT::IDENTIFIER, &inheritToken)) return false;
			std::string inheritClassName(inheritToken.text, inheritToken.length);

			baseClass = m_Program->GetClass(m_Program->GetClassID(inheritClassName));

			Token closeBracket = tokenizer->GetToken();
			if (closeBracket.type != TokenTypeT::CLOSE_BRACKET) return false;
		}

		arrowToken = tokenizer->PeekToken();
	}

	Token openBrace;
	if (tokenizer->Expect(TokenTypeT::OPEN_BRACE, &openBrace)) return false;

	ID classScope = m_Program->CreateScope();

	Class* cls = new Class(className, classScope, baseClass);
	g_CurrentClassName = className;
	ID classID = m_Program->AddClass(className, cls);
	cls->SetTemplateDefinition(templateDefinition);

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

	if (!cls->HasAssignSTFunction())
	{
		Function* function = new Function();
		function->accessModifier = AccessModifier::PUBLIC;
		function->isStatic = false;
		function->isVirtual = false;
		function->name = "operator=";
		function->returnInfo = TypeInfo((uint16)ValueType::VOID_T, 0);
		function->scope = m_Program->CreateScope(classScope);
		
		FunctionParameter parameter;
		parameter.isReference = true;
		parameter.type = TypeInfo(classID, 0);
		
		std::string paramName = "#TLS_Other";
		Scope* scope = m_Program->GetScope(function->scope);
		ID variableID = scope->GetVariableID(paramName, true);
		scope->DeclareVariable(variableID, TypeInfo(classID, 0));

		parameter.variableID = variableID;
		function->parameters.push_back(parameter);

		const std::vector<ClassField>& members = cls->GetMemberFields();
		for (uint32 i = 0; i < members.size(); i++)
		{
			const ClassField& member = members[i];
			ASTExpressionVariable* variableExpr = new ASTExpressionVariable(function->scope, parameter.variableID);
			ASTExpressionDirectMemberAccess* memberAssignExpr = new ASTExpressionDirectMemberAccess(function->scope, member.type, member.offset, variableExpr);
			function->body.push_back(memberAssignExpr);
		}

		cls->AddFunction(function);
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
		function->returnInfo.type = (uint16)ValueType::VOID_T;
		function->name = g_CurrentClassName;
	}
	else if (isDestructor)
	{
		function->returnInfo.type = (uint16)ValueType::VOID_T;
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
			if (!cls->IsTemplateClass())
			{
				delete function;
				return false;
			}

			std::string templateTypeName(t.text, t.length);
			if (cls->InstantiateTemplateGetIndex(m_Program, templateTypeName) == -1)
			{
				delete function;
				return false;
			}

			function->returnInfo.type = (uint16)ValueType::TEMPLATE_TYPE;
			function->returnTemplateTypeName = templateTypeName;
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
			else if (t.type == TokenTypeT::PLUS)
			{
				function->name = "operator+";
			}
			else if (t.type == TokenTypeT::MINUS)
			{
				function->name = "operator-";
			}
			else if (t.type == TokenTypeT::ASTERISK)
			{
				function->name = "operator*";
			}
			else if (t.type == TokenTypeT::SLASH)
			{
				function->name = "operator/";
			}
			else if (t.type == TokenTypeT::MOD)
			{
				function->name = "operator%";
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

		param.isReference = false;
		Token peek = tokenizer->PeekToken();
		if (peek.type == TokenTypeT::AND)
		{
			tokenizer->Expect(TokenTypeT::AND);
			param.isReference = true;
		}

		std::string templateTypeName = "";
		if (param.type.type == INVALID_ID)
		{
			if (!cls->IsTemplateClass())
				return false;

			templateTypeName = std::string(typeToken.text, typeToken.length);
			if (cls->InstantiateTemplateGetIndex(m_Program, templateTypeName) == -1)
				return false;

			param.type.type = (uint16)ValueType::TEMPLATE_TYPE;
			param.templateTypeName = templateTypeName;
		}

		t = tokenizer->GetToken();
		if (t.type != TokenTypeT::IDENTIFIER) { delete function; return false; }

		std::string paramName(t.text, t.length);
		param.variableID = m_Program->GetScope(functionScope)->GetVariableID(paramName, true);
		m_Program->GetScope(functionScope)->DeclareVariable(param.variableID, param.type, templateTypeName);

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
	std::string templateTypeName = "";
	if (type == INVALID_ID)
	{
		bool foundTemplateType = false;
		const TemplateDefinition& definition = cls->GetTemplateDefinition();
		for (uint32 i = 0; i < definition.parameters.size(); i++)
		{
			if (definition.parameters[i].type == TemplateParameterType::TYPE &&
				definition.parameters[i].name == typeName)
			{
				templateTypeName = typeName;
				foundTemplateType = true;
				break;
			}
		}

		if (!foundTemplateType) return false;
		type = (uint16)ValueType::TEMPLATE_TYPE;
	}

	Token openAngle = tokenizer->PeekToken();
	bool addedInstantiationCommand = false;
	if (openAngle.type == TokenTypeT::LESS)
	{
		tokenizer->Expect(TokenTypeT::LESS);
		bool templatedType = false;
		TemplateInstantiationCommand* command = new TemplateInstantiationCommand();
		TemplateInstantiation instantiation = ParseTemplateInstantiation(tokenizer, m_Program->GetClass(type), command, &templatedType);
		command->type = m_Program->GetClassID(typeName);

		if (!templatedType)
		{
			Class* baseClass = m_Program->GetClass(type);
			type = baseClass->InstantiateTemplate(m_Program, instantiation);
		}
		else
		{
			type = m_Program->GetClass(m_Program->GetClassID(g_CurrentClassName))->AddInstantiationCommand(command);
			addedInstantiationCommand = true;
		}
	}

	uint64 typeSize = addedInstantiationCommand ? 0 : m_Program->GetTypeSize(type);
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
		cls->AddMemberField(type, pointerLevel, arrayLength, typeSize, *offset, name, templateTypeName);
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
				if (tokenizer->Expect(TokenTypeT::SEMICOLON)) return false;
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
		else if (next.type == TokenTypeT::ASTERISK)
		{
			uint8 pointerLevel = ParsePointerLevel(tokenizer) + 1;//already consumed 1 '*'
			uint16 type = m_Program->GetClassID(identifier);
			Token nameToken;
			if (tokenizer->Expect(TokenTypeT::IDENTIFIER, &nameToken))return false;
			std::string name(nameToken.text, nameToken.length);
			ID variableID = m_Program->GetScope(currentScope)->GetVariableID(name, true);

			next = tokenizer->GetToken();
			ASTExpression* assignExpr = nullptr;
			if (next.type == TokenTypeT::SEMICOLON)
			{
				assignExpr = nullptr;
			}
			else if (next.type == TokenTypeT::EQUALS)
			{
				assignExpr = ParseExpression(tokenizer, currentScope);
				if (tokenizer->Expect(TokenTypeT::SEMICOLON)) return false;
			}
			else
			{
				return false;
			}

			uint16 derivedType = type;
			if (assignExpr)
			{
				derivedType = assignExpr->GetTypeInfo(m_Program).type;
			}

			m_Program->GetScope(currentScope)->DeclareVariable(variableID, TypeInfo(type, pointerLevel), "", derivedType);
			ASTExpressionDeclarePointer* declarePointer = new ASTExpressionDeclarePointer(currentScope, variableID, type, pointerLevel, assignExpr);
			function->body.push_back(declarePointer);
			return true;
		}
		else if (next.type == TokenTypeT::LESS)
		{
			TemplateInstantiationCommand* command = new TemplateInstantiationCommand();
			bool templatedType = false;
			TemplateInstantiation instantiation = ParseTemplateInstantiation(tokenizer, m_Program->GetClass(m_Program->GetClassID(identifier)), command, &templatedType);
			command->type = m_Program->GetClassID(identifier);
			ID classID = INVALID_ID;
			if (!templatedType)
			{
				ID baseClassID = m_Program->GetClassID(identifier);
				Class* baseClass = m_Program->GetClass(baseClassID);
				// Instantiate the template type (compile-time class creation)
				classID = baseClass->InstantiateTemplate(m_Program, instantiation);
				delete command;
			}
			else
			{
				classID = m_Program->GetClass(m_Program->GetClassID(g_CurrentClassName))->AddInstantiationCommand(command);
			}

			Token nameTok = tokenizer->GetToken(); // variable name
			std::string varName(nameTok.text, nameTok.length);

			Token openParen = tokenizer->PeekToken();
			std::vector<ASTExpression*> args;
			if (openParen.type == TokenTypeT::OPEN_PAREN)
			{
				tokenizer->Expect(TokenTypeT::OPEN_PAREN);
				ParseArguments(tokenizer, currentScope, args);
			}

			if (tokenizer->Expect(TokenTypeT::SEMICOLON)) return false;

			Scope* scope = m_Program->GetScope(currentScope);
			ID variableID = scope->GetVariableID(varName);

			scope->DeclareVariable(variableID, TypeInfo(classID, 0));

			ASTExpressionDeclareObject* declareObjectExpr = new ASTExpressionDeclareObject(currentScope, classID, variableID, args);
			function->body.push_back(declareObjectExpr);
			return true;
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
	else if (t.type == TokenTypeT::IF)
	{
		if (tokenizer->Expect(TokenTypeT::OPEN_PAREN)) return false;
		ASTExpression* conditionExpr = ParseExpression(tokenizer, currentScope);
		if (tokenizer->Expect(TokenTypeT::CLOSE_PAREN)) return false;

		ID ifScope = INVALID_ID;
		ID elseScope = INVALID_ID;
		std::vector<ASTExpression*> ifExprs;
		std::vector<ASTExpression*> elseExprs;

		Token next = tokenizer->PeekToken();
		if (next.type == TokenTypeT::OPEN_BRACE)
		{
			tokenizer->Expect(TokenTypeT::OPEN_BRACE);
			ifScope = m_Program->CreateScope(currentScope);
			while (true)
			{
				Token peekBody = tokenizer->PeekToken();
				if (peekBody.type == TokenTypeT::CLOSE_BRACE) { tokenizer->GetToken(); break; }
				if (!ParseStatement(function, tokenizer, ifScope)) return false;
				ASTExpression* statement = function->body.back();
				function->body.pop_back();
				ifExprs.push_back(statement);
			}
		}
		else
		{
			if (!ParseStatement(function, tokenizer, currentScope))
				return false;

			ASTExpression* statement = function->body.back();
			function->body.pop_back();
			ifExprs.push_back(statement);
		}

		Token maybeElse = tokenizer->PeekToken();
		if (maybeElse.type == TokenTypeT::ELSE)
		{
			tokenizer->GetToken();
			Token braceElse = tokenizer->PeekToken();
			if (braceElse.type == TokenTypeT::OPEN_BRACE)
			{
				tokenizer->GetToken();
				elseScope = m_Program->CreateScope(currentScope);
				while (true)
				{
					Token peekBody = tokenizer->PeekToken();
					if (peekBody.type == TokenTypeT::CLOSE_BRACE) { tokenizer->GetToken(); break; }
					if (!ParseStatement(function, tokenizer, elseScope)) return false;
					ASTExpression* statement = function->body.back();
					function->body.pop_back();
					elseExprs.push_back(statement);
				}
			}
			else
			{
				if (!ParseStatement(function, tokenizer, currentScope)) return false;
				ASTExpression* statement = function->body.back();
				function->body.pop_back();
				elseExprs.push_back(statement);
			}
		}

		ASTExpressionIfElse* ifElseExpr = new ASTExpressionIfElse(currentScope, ifScope, elseScope, conditionExpr, ifExprs, elseExprs);
		function->body.push_back(ifElseExpr);
		return true;
	}
	else if (t.type == TokenTypeT::FOR)
	{
		if (tokenizer->Expect(TokenTypeT::OPEN_PAREN)) return false;

		ASTExpression* declareExpr = nullptr;
		ASTExpression* conditionExpr = nullptr;
		ASTExpression* incrementExpr = nullptr;
		std::vector<ASTExpression*> body;
		ID bodyScope = INVALID_ID;

		// init
		Token peek = tokenizer->PeekToken();
		if (peek.type != TokenTypeT::SEMICOLON)
		{
			if (!ParseStatement(function, tokenizer, currentScope)) return false;
			declareExpr = function->body.back();
			function->body.pop_back();
		}
		else 
		{ 
			tokenizer->GetToken();
		}

		// condition
		peek = tokenizer->PeekToken();
		if (peek.type != TokenTypeT::SEMICOLON)
			conditionExpr = ParseExpression(tokenizer, currentScope);

		if (tokenizer->Expect(TokenTypeT::SEMICOLON)) return false;

		// increment
		peek = tokenizer->PeekToken();
		if (peek.type != TokenTypeT::CLOSE_PAREN)
		{
			if (!ParseStatement(function, tokenizer, currentScope)) return false;
			incrementExpr = function->body.back();
			function->body.pop_back();
		}

		// body
		Token brace = tokenizer->PeekToken();
		if (brace.type == TokenTypeT::OPEN_BRACE)
		{
			tokenizer->GetToken(); // consume '{'
			ID loopScope = m_Program->CreateScope(currentScope);
			bodyScope = loopScope;
			while (true)
			{
				Token peekBody = tokenizer->PeekToken();
				if (peekBody.type == TokenTypeT::CLOSE_BRACE) { tokenizer->GetToken(); break; }
				if (!ParseStatement(function, tokenizer, loopScope)) return false;
				ASTExpression* statement = function->body.back();
				function->body.pop_back();
				body.push_back(statement);
			}
		}
		else
		{
			if (!ParseStatement(function, tokenizer, currentScope)) return false;
			ASTExpression* statement = function->body.back();
			function->body.pop_back();
			body.push_back(statement);
		}

		ASTExpressionFor* forExpr = new ASTExpressionFor(currentScope, bodyScope, declareExpr, conditionExpr, incrementExpr, body);
		function->body.push_back(forExpr);
		return true;
	}
	else if (t.type == TokenTypeT::WHILE)
	{
		std::vector<ASTExpression*> body;
		ID bodyScope = INVALID_ID;

		if (tokenizer->Expect(TokenTypeT::OPEN_PAREN)) return false;
		ASTExpression* conditionExpr = ParseExpression(tokenizer, currentScope);
		if (tokenizer->Expect(TokenTypeT::CLOSE_PAREN)) return false;

		Token brace = tokenizer->PeekToken();
		if (brace.type == TokenTypeT::OPEN_BRACE)
		{
			tokenizer->Expect(TokenTypeT::OPEN_BRACE);
			ID loopScope = m_Program->CreateScope(currentScope);
			bodyScope = loopScope;
			while (true)
			{
				Token peekBody = tokenizer->PeekToken();
				if (peekBody.type == TokenTypeT::CLOSE_BRACE) { tokenizer->GetToken(); break; }
				if (!ParseStatement(function, tokenizer, loopScope)) return false;
				ASTExpression* statement = function->body.back();
				function->body.pop_back();
				body.push_back(statement);
			}
		}
		else
		{
			if (!ParseStatement(function, tokenizer, currentScope)) return false;
			ASTExpression* statement = function->body.back();
			function->body.pop_back();
			body.push_back(statement);
		}

		ASTExpressionWhile* whileExpr = new ASTExpressionWhile(currentScope, bodyScope, conditionExpr, body);
		function->body.push_back(whileExpr);
		return true;
	}
	else if (t.type == TokenTypeT::BREAK)
	{
		if (tokenizer->Expect(TokenTypeT::SEMICOLON)) return false;
		ASTExpressionBreak* breakExpr = new ASTExpressionBreak(currentScope);
		function->body.push_back(breakExpr);
		return true;
	}
	else if (t.type == TokenTypeT::CONTINUE)
	{
		if (tokenizer->Expect(TokenTypeT::SEMICOLON)) return false;
		ASTExpressionContinue* continueExpr = new ASTExpressionContinue(currentScope);
		function->body.push_back(continueExpr);
		return true;
	}
	else if (t.type == TokenTypeT::DELETE_T)
	{
		Token peek = tokenizer->PeekToken();
		bool deleteArray = false;
		if (peek.type == TokenTypeT::OPEN_BRACKET)
		{
			tokenizer->Expect(TokenTypeT::OPEN_BRACKET);
			if (tokenizer->Expect(TokenTypeT::CLOSE_BRACKET)) return false;
			deleteArray = true;
		}

		ASTExpression* expr = ParseExpression(tokenizer, currentScope);

		if (tokenizer->Expect(TokenTypeT::SEMICOLON)) return false;

		ASTExpressionDelete* deleteExpr = new ASTExpressionDelete(currentScope, expr, deleteArray);
		function->body.push_back(deleteExpr);
		return true;
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
		tokenizer->Expect(TokenTypeT::PLUS_PLUS);
		ASTExpression* expr = ParseExpression(tokenizer, currentScope);
		ASTExpressionUnaryUpdate* unaryExpr = new ASTExpressionUnaryUpdate(currentScope, expr, ASTUnaryUpdateOp::PRE_INC);
		return unaryExpr;
	} break;
	case TokenTypeT::MINUS_MINUS: {
		tokenizer->Expect(TokenTypeT::MINUS_MINUS);
		ASTExpression* expr = ParseExpression(tokenizer, currentScope);
		ASTExpressionUnaryUpdate* unaryExpr = new ASTExpressionUnaryUpdate(currentScope, expr, ASTUnaryUpdateOp::PRE_DEC);
		return unaryExpr;
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
			ASTExpressionUnaryUpdate* unaryExpr = new ASTExpressionUnaryUpdate(currentScope, expr, ASTUnaryUpdateOp::POST_INC);
			return unaryExpr;
		}
		else if (tok.type == TokenTypeT::MINUS_MINUS)
		{
			tokenizer->GetToken();
			ASTExpressionUnaryUpdate* unaryExpr = new ASTExpressionUnaryUpdate(currentScope, expr, ASTUnaryUpdateOp::POST_DEC);
			return unaryExpr;
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
		uint16 type = ParseType(typeToken);
		uint8 pointerLevel = ParsePointerLevel(tokenizer);

		std::string templateTypeName = "";
		if (type == INVALID_ID)
		{
			Class* cls = m_Program->GetClass(m_Program->GetClassID(g_CurrentClassName));
			if (!cls->IsTemplateClass())
				return nullptr;

			templateTypeName = std::string(typeToken.text, typeToken.length);
			const TemplateDefinition& definition = cls->GetTemplateDefinition();
			bool foundParam = false;
			for (uint32 i = 0; i < definition.parameters.size(); i++)
			{
				if (templateTypeName == definition.parameters[i].name)
				{
					foundParam = true;
					break;
				}
			}

			if (!foundParam) return nullptr;
			type = (uint16)ValueType::TEMPLATE_TYPE;
		}

		Token peek = tokenizer->PeekToken();
		if (peek.type == TokenTypeT::OPEN_BRACKET)
		{
			tokenizer->Expect(TokenTypeT::OPEN_BRACKET);
			ASTExpression* sizeExpr = ParseExpression(tokenizer, currentScope);
			tokenizer->Expect(TokenTypeT::CLOSE_BRACKET);

			ASTExpressionNewArray* newArrayExpr = new ASTExpressionNewArray(currentScope, type, pointerLevel, sizeExpr);
			newArrayExpr->templateTypeName = templateTypeName;
			return newArrayExpr;
		}
		else if(peek.type == TokenTypeT::OPEN_PAREN)
		{
			tokenizer->Expect(TokenTypeT::OPEN_PAREN);
			std::vector<ASTExpression*> argExprs;
			ParseArguments(tokenizer, currentScope, argExprs);
			ASTExpressionNew* newExpr = new ASTExpressionNew(currentScope, type, argExprs);
			newExpr->templateTypeName = templateTypeName;
			return newExpr;
		}
		else
		{
			return nullptr;
		}
	}
	else if (t.type == TokenTypeT::IDENTIFIER || t.type == TokenTypeT::THIS)
	{
		Token next = tokenizer->GetToken();

		if (t.type == TokenTypeT::THIS && next.type != TokenTypeT::DOT)
		{
			ASTExpressionThis* thisExpr = new ASTExpressionThis(currentScope, m_Program->GetClassID(g_CurrentClassName));
			return thisExpr;
		}

		if (next.type == TokenTypeT::OPEN_PAREN) //Function call
		{
			std::vector<ASTExpression*> argExprs;
			ParseArguments(tokenizer, currentScope, argExprs);

			std::string functionName(t.text, t.length);
			ID classID = m_Program->GetClassID(g_CurrentClassName);
			Class* constructorClass = m_Program->GetClassByName(functionName);
			if (constructorClass)
			{
				ASTExpressionConstructorCall* constructorCall = new ASTExpressionConstructorCall(currentScope, m_Program->GetClassID(functionName), argExprs);
				return constructorCall;
			}

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

			ASTExpression* indexExpr = nullptr;
			if (peek.type == TokenTypeT::OPEN_BRACKET)
			{
				tokenizer->Expect(TokenTypeT::OPEN_BRACKET);
				indexExpr = ParseExpression(tokenizer, currentScope);
				if (tokenizer->Expect(TokenTypeT::CLOSE_BRACKET)) return nullptr;
			}

			if (peek.type != TokenTypeT::OPEN_PAREN) //Member or static variable
			{
				std::string className(t.text, t.length);
				ID classID = m_Program->GetClassID(className);
				ID moduleID = m_Program->GetModuleID(className);

				

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
							if (indexExpr != nullptr)
							{
								ASTExpressionVariableSet* variableSetExpr = new ASTExpressionVariableSet(classScope, variableID, nullptr);
								ASTExpressionIndex* indexExpr_ = new ASTExpressionIndex(currentScope, variableSetExpr, indexExpr, assignExpr);
								return indexExpr_;
							}
							else
							{
								ASTExpressionVariableSet* variableSetExpr = new ASTExpressionVariableSet(classScope, variableID, assignExpr);
								return variableSetExpr;
							}
						}
						else
						{
							if (indexExpr != nullptr)
							{
								ASTExpressionVariable* variableExpr = new ASTExpressionVariable(classScope, variableID);
								ASTExpressionIndex* indexExpr_ = new ASTExpressionIndex(currentScope, variableExpr, indexExpr, nullptr);
								return indexExpr_;
							}
							else
							{
								ASTExpressionVariable* variableExpr = new ASTExpressionVariable(classScope, variableID);
								return variableExpr;
							}
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
						if (t.type == TokenTypeT::THIS)
						{
							members.erase(members.begin());
						}

						bool templatedType = false;
						uint64 offset = m_Program->GetClass(thisID)->CalculateOffset(members, &memberTypeInfo, &templatedType);
						if (offset == UINT64_MAX) return nullptr;

						Token equals = tokenizer->PeekToken();
						if (equals.type == TokenTypeT::EQUALS)
						{
							tokenizer->Expect(TokenTypeT::EQUALS);
							ASTExpression* assignExpr = ParseExpression(tokenizer, currentScope);
							ASTExpressionDirectMemberAccess* memberAccessExpr = new ASTExpressionDirectMemberAccess(currentScope, memberTypeInfo, offset, assignExpr);
							if (templatedType)
								memberAccessExpr->members = members;
							return memberAccessExpr;
						}
						else
						{
							ASTExpressionDirectMemberAccess* memberAccessExpr = new ASTExpressionDirectMemberAccess(currentScope, memberTypeInfo, offset, nullptr);
							if (templatedType)
								memberAccessExpr->members = members;
							return memberAccessExpr;
						}
					}

					TypeInfo variableTypeInfo = scope->GetVariableDerivedTypeInfo(variableID);
					TypeInfo memberTypeInfo;
					bool templatedType = false;
					Class* cls = m_Program->GetClass(variableTypeInfo.type);
					uint64 offset = cls ? cls->CalculateOffset(members, &memberTypeInfo, &templatedType) : 0;

					Token equals = tokenizer->PeekToken();
					if (equals.type == TokenTypeT::EQUALS)
					{
						tokenizer->Expect(TokenTypeT::EQUALS);
						ASTExpression* assignExpr = ParseExpression(tokenizer, currentScope);
						ASTExpressionMemberSet* memberSetExpr = new ASTExpressionMemberSet(currentScope, variableID, offset, memberTypeInfo.type, memberTypeInfo.pointerLevel, assignExpr, indexExpr);
						if (templatedType)
							memberSetExpr->members = members;
						return memberSetExpr;
					}
					else if (equals.type == TokenTypeT::DOT)
					{
						tokenizer->Expect(TokenTypeT::DOT);
						std::vector<std::string> identifiers;
						while (true)
						{
							Token identifierToken = tokenizer->GetToken();
							if (identifierToken.type != TokenTypeT::IDENTIFIER) return nullptr;
							identifiers.push_back(std::string(identifierToken.text, identifierToken.length));
							Token peek = tokenizer->PeekToken();
							if (peek.type != TokenTypeT::DOT) break;
							tokenizer->GetToken();
						}

						std::vector<std::string> updatedMembers;
						for (uint32 i = 0; i < members.size(); i++)
							updatedMembers.push_back(members[i]);
						for (uint32 i = 0; i < identifiers.size(); i++)
							updatedMembers.push_back(identifiers[i]);

						

						peek = tokenizer->PeekToken();
						if (peek.type == TokenTypeT::OPEN_PAREN)
						{
							tokenizer->Expect(TokenTypeT::OPEN_PAREN);
							std::vector<ASTExpression*> argExprs;
							ParseArguments(tokenizer, currentScope, argExprs);
							std::string functionName = updatedMembers.back();
							updatedMembers.pop_back();
							bool templatedType = false;
							offset = m_Program->GetClass(variableTypeInfo.type)->CalculateOffset(updatedMembers, &memberTypeInfo, &templatedType);

							Class* cls = m_Program->GetClass(memberTypeInfo.type);
							uint16 functionID = cls->GetFunctionID(functionName, argExprs);

							ASTExpressionMemberAccess* memberAccessExpr = new ASTExpressionMemberAccess(currentScope, variableID, offset, memberTypeInfo.type, memberTypeInfo.pointerLevel, indexExpr);
							if (templatedType)
								memberAccessExpr->members = updatedMembers;

							if (cls->GetFunction(functionID)->isVirtual)
							{
								ASTExpressionVirtualFunctionCall* virtualFunctionCall = new ASTExpressionVirtualFunctionCall(currentScope, memberAccessExpr, functionName, argExprs);
								return virtualFunctionCall;
							}

							ASTExpressionMemberFunctionCall* memberFunctionCall = new ASTExpressionMemberFunctionCall(currentScope, memberAccessExpr, functionName, argExprs);
							return memberFunctionCall;
						}
						else if (peek.type == TokenTypeT::EQUALS)
						{
							bool templatedType = false;
							offset = m_Program->GetClass(variableTypeInfo.type)->CalculateOffset(updatedMembers, &memberTypeInfo, &templatedType);
							tokenizer->Expect(TokenTypeT::EQUALS);
							ASTExpression* assignExpr = ParseExpression(tokenizer, currentScope);
							ASTExpressionMemberSet* memberSetExpr = new ASTExpressionMemberSet(currentScope, variableID, offset, memberTypeInfo.type, memberTypeInfo.pointerLevel, assignExpr, indexExpr);
							if (templatedType)
								memberSetExpr->members = updatedMembers;
							return memberSetExpr;
						}
						else
						{
							bool templatedType = false;
							offset = m_Program->GetClass(variableTypeInfo.type)->CalculateOffset(updatedMembers, &memberTypeInfo, &templatedType);
							ASTExpressionMemberAccess* memberAccessExpr = new ASTExpressionMemberAccess(currentScope, variableID, offset, memberTypeInfo.type, memberTypeInfo.pointerLevel, indexExpr);
							if (templatedType)
								memberAccessExpr->members = updatedMembers;
							return memberAccessExpr;
						}

						uint32 bp = 0;
					}
					else
					{
						ASTExpressionMemberAccess* memberAccessExpr = new ASTExpressionMemberAccess(currentScope, variableID, offset, memberTypeInfo.type, memberTypeInfo.pointerLevel, indexExpr);
						if (templatedType)
							memberAccessExpr->members = members;
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
					if (variableID == INVALID_ID)
					{
						TypeInfo memberTypeInfo;
						members.push_back(variableName);
						bool templatedType = false;
						uint64 offset = m_Program->GetClass(m_Program->GetClassID(g_CurrentClassName))->CalculateOffset(members, &memberTypeInfo, &templatedType);
						if (offset == UINT64_MAX) return nullptr;
						objExpr = new ASTExpressionDirectMemberAccess(currentScope, memberTypeInfo, offset, nullptr);
						if (templatedType)
							((ASTExpressionDirectMemberAccess*)objExpr)->members = members;

						if (indexExpr != nullptr)
						{
							objExpr = new ASTExpressionIndex(currentScope, objExpr, indexExpr, nullptr);
						}
					}
					else
					{
						objExpr = new ASTExpressionVariable(currentScope, variableID);
						if (indexExpr != nullptr)
						{
							objExpr = new ASTExpressionIndex(currentScope, objExpr, indexExpr, nullptr);
						}
					}
				}
				else
				{
					std::string variableName(t.text, t.length);
					ID variableID = m_Program->GetScope(currentScope)->GetVariableID(variableName, false);
					if (variableID == INVALID_ID)
					{
						TypeInfo memberTypeInfo;
						members.insert(members.begin(), variableName);
						bool templatedType = false;
						uint64 offset = m_Program->GetClass(m_Program->GetClassID(g_CurrentClassName))->CalculateOffset(members, &memberTypeInfo, &templatedType);
						if (offset == UINT64_MAX) return nullptr;
						objExpr = new ASTExpressionDirectMemberAccess(currentScope, memberTypeInfo, offset, nullptr);
						if (templatedType)
							((ASTExpressionDirectMemberAccess*)objExpr)->members = members;
						if (indexExpr)
							objExpr = new ASTExpressionIndex(currentScope, objExpr, indexExpr, nullptr);
					}
					else
					{
						TypeInfo variableTypeInfo = m_Program->GetScope(currentScope)->GetVariableDerivedTypeInfo(variableID);
						TypeInfo memberTypeInfo;
						bool templatedType = false;
						uint64 offset = m_Program->GetClass(variableTypeInfo.type)->CalculateOffset(members, &memberTypeInfo, &templatedType);
						if (offset == UINT64_MAX) return nullptr;
						objExpr = new ASTExpressionMemberAccess(currentScope, variableID, offset, memberTypeInfo.type, memberTypeInfo.pointerLevel, indexExpr);
						if (templatedType)
							((ASTExpressionMemberAccess*)objExpr)->members = members;
					}
				}

				Class* cls = m_Program->GetClass(objExpr->GetTypeInfo(m_Program).type);
				uint16 functionID = cls->GetFunctionID(functionName, expressions);
				Function* function = cls->GetFunction(functionID);
				if (function->isVirtual)
				{
					ASTExpressionVirtualFunctionCall* virtualFunctionCall = new ASTExpressionVirtualFunctionCall(currentScope, objExpr, functionName, expressions);
					return virtualFunctionCall;
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
				bool templatedType = false;
				uint64 offset = thisClass->CalculateOffset(members, &memberTypeInfo, &templatedType);
				ASTExpressionDirectMemberAccess* directMemberAccessExpr = new ASTExpressionDirectMemberAccess(currentScope, memberTypeInfo, offset, assignExpr);
				if (templatedType)
					directMemberAccessExpr->members = members;
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
				tokenizer->Expect(TokenTypeT::DOT);
				std::vector<std::string> identifiers;
				while (true)
				{
					Token identifierToken = tokenizer->GetToken();
					if (identifierToken.type != TokenTypeT::IDENTIFIER) return nullptr;
					identifiers.push_back(std::string(identifierToken.text, identifierToken.length));
					Token peek = tokenizer->PeekToken();
					if (peek.type != TokenTypeT::DOT) break;
					tokenizer->GetToken();
				}

				peek = tokenizer->PeekToken();
				ASTExpression* assignExpr = nullptr;
				if (peek.type == TokenTypeT::EQUALS)
				{
					tokenizer->Expect(TokenTypeT::EQUALS);
					assignExpr = ParseExpression(tokenizer, currentScope);
				}
				else if (peek.type == TokenTypeT::OPEN_PAREN)
				{
					tokenizer->Expect(TokenTypeT::OPEN_PAREN);
					std::vector<ASTExpression*> argExprs;
					ParseArguments(tokenizer, currentScope, argExprs);
					std::string functionName = identifiers.back();
					identifiers.pop_back();
					ASTExpression* objExpr = nullptr;
					std::string variableName(t.text, t.length);
					ID variableID = m_Program->GetScope(currentScope)->GetVariableID(variableName, false);
					if (identifiers.empty())
					{
						objExpr = new ASTExpressionVariable(currentScope, variableID);
						objExpr = new ASTExpressionIndex(currentScope, objExpr, indexExpr, nullptr);
					}
					else
					{
						uint64 variableType = m_Program->GetScope(currentScope)->GetVariableDerivedTypeInfo(variableID).type;
						TypeInfo memberTypeInfo;
						bool templatedType = false;
						uint64 offset = m_Program->GetClass(variableType)->CalculateOffset(identifiers, &memberTypeInfo, &templatedType);
						objExpr = new ASTExpressionMemberAccess(currentScope, variableID, offset, memberTypeInfo.type, memberTypeInfo.pointerLevel, indexExpr);
						if (templatedType)
							((ASTExpressionMemberAccess*)objExpr)->members = identifiers;
					}

					Class* cls = m_Program->GetClass(objExpr->GetTypeInfo(m_Program).type);
					uint16 functionID = cls->GetFunctionID(functionName, argExprs);
					Function* function = cls->GetFunction(functionID);
					if (function->isVirtual)
					{
						ASTExpressionVirtualFunctionCall* virtualFunctionCall = new ASTExpressionVirtualFunctionCall(currentScope, objExpr, functionName, argExprs);
						return virtualFunctionCall;
					}

					ASTExpressionMemberFunctionCall* memberFunctionCallExpr = new ASTExpressionMemberFunctionCall(currentScope, objExpr, functionName, argExprs);
					return memberFunctionCallExpr;
				}

				std::string variableName(t.text, t.length);
				ID variableID = m_Program->GetScope(currentScope)->GetVariableID(variableName, false);
				uint64 variableType = m_Program->GetScope(currentScope)->GetVariableDerivedTypeInfo(variableID).type;
				TypeInfo memberTypeInfo;
				bool templatedType = false;
				uint64 offset = m_Program->GetClass(variableType)->CalculateOffset(identifiers, &memberTypeInfo, &templatedType);

				if (assignExpr)
				{
					ASTExpressionMemberSet* memberSetExpr = new ASTExpressionMemberSet(currentScope, variableID, offset, memberTypeInfo.type, memberTypeInfo.pointerLevel, assignExpr, indexExpr);
					if (templatedType)
						memberSetExpr->members = identifiers;
					return memberSetExpr;
				}
				else
				{
					ASTExpressionMemberAccess* memberAccessExpr = new ASTExpressionMemberAccess(currentScope, variableID, offset, memberTypeInfo.type, memberTypeInfo.pointerLevel, indexExpr);
					if (templatedType)
						memberAccessExpr->members = identifiers;
					return memberAccessExpr;
				}
			}

			std::string variableName(t.text, t.length);
			ID variableID = m_Program->GetScope(currentScope)->GetVariableID(variableName, false);
			if (variableID == INVALID_ID)
			{
				Class* cls = m_Program->GetClass(m_Program->GetClassID(g_CurrentClassName));
				TypeInfo memberTypeInfo;
				bool templatedType;
				std::vector<std::string> members;
				members.push_back(variableName);
				uint64 offset = cls->CalculateOffset(members, &memberTypeInfo, &templatedType);
				if (offset == UINT64_MAX) return nullptr;

				ASTExpressionDirectMemberAccess* memberAccessExpr = new ASTExpressionDirectMemberAccess(currentScope, memberTypeInfo, offset, nullptr);
				if (templatedType)
					memberAccessExpr->members = members;
				ASTExpressionIndex* indexExpr_ = new ASTExpressionIndex(currentScope, memberAccessExpr, indexExpr, assignExpr);
				return indexExpr_;
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
				bool templatedType = false;
				uint64 offset = thisClass->CalculateOffset(members, &memberTypeInfo, &templatedType);
				if (offset == UINT64_MAX) return nullptr;

				tokenizer->at = next.text; // rewind, it's a variable
				ASTExpressionDirectMemberAccess* directMemberAccessExpr = new ASTExpressionDirectMemberAccess(currentScope, memberTypeInfo, offset, nullptr);
				if (templatedType)
					directMemberAccessExpr->members = members;
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

TemplateInstantiation Parser::ParseTemplateInstantiation(Tokenizer* tokenizer, Class* parentClass, TemplateInstantiationCommand* command, bool* templatedType)
{
	TemplateInstantiation instantiation;
	uint32 angleDepth = 1;

	while (true)
	{
		Token t = tokenizer->PeekToken();
		if (t.type == TokenTypeT::END)
			break;

		if (t.type == TokenTypeT::IDENTIFIER || Tokenizer::IsTokenPrimitiveType(t))
		{
			tokenizer->Expect(TokenTypeT::IDENTIFIER);
			std::string typeName(t.text, t.length);
			Token next = tokenizer->PeekToken();

			if (next.type == TokenTypeT::LESS)
			{
				tokenizer->Expect(TokenTypeT::LESS);
				TemplateInstantiationCommand* subCmd = new TemplateInstantiationCommand();
				subCmd->type = m_Program->GetClassID(typeName);
				TemplateInstantiation nested = ParseTemplateInstantiation(tokenizer, m_Program->GetClass(m_Program->GetTypeID(typeName)), subCmd, templatedType);

				TemplateInstantiationCommandArg arg;
				arg.type = 1;
				arg.command = subCmd;
				command->args.push_back(arg);

				bool isTemplate = false;
				for (uint32 i = 0; i < nested.args.size(); i++)
				{
					if (nested.args[i].value == (uint16)ValueType::TEMPLATE_TYPE)
					{
						isTemplate = true;
						break;
					}
				}

				if (!isTemplate)
				{
					TemplateArgument arg;
					arg.type = TemplateParameterType::TYPE;
					arg.value = AddTemplateInstantiationType(typeName, nested);
					instantiation.args.push_back(arg);
				}

			}
			else
			{
				uint8 pointerLevel = ParsePointerLevel(tokenizer);

				const TemplateDefinition& definition = parentClass->GetTemplateDefinition();
				TemplateParameterType paramType = TemplateParameterType::TYPE;
				for (uint32 i = 0; i < definition.parameters.size(); i++)
				{
					if (definition.parameters[i].name == typeName)
					{
						paramType = definition.parameters[i].type;
						break;
					}
				}

				TemplateArgument arg;
				arg.type = paramType;
				arg.value = m_Program->GetTypeID(typeName);
				arg.pointerLevel = pointerLevel;

				if (arg.value == INVALID_ID)
				{
					if (arg.type != TemplateParameterType::INT)
						arg.type = TemplateParameterType::TEMPLATE_TYPE;

					arg.value = (uint16)ValueType::TEMPLATE_TYPE;
					arg.templateTypeName = typeName;
					*templatedType = true;
				}

				TemplateInstantiationCommandArg cmdArg;
				cmdArg.type = 0;
				cmdArg.arg = arg;
				command->args.push_back(cmdArg);

				instantiation.args.push_back(arg);
			}
		}
		else if (t.type == TokenTypeT::NUMBER_LITERAL)
		{
			tokenizer->Expect(TokenTypeT::NUMBER_LITERAL);
			std::string numberString(t.text, t.length);
			TemplateArgument arg;
			arg.type = TemplateParameterType::INT;
			arg.value = std::stol(numberString);
			instantiation.args.push_back(arg);
		}

		t = tokenizer->PeekToken();

		if (t.type == TokenTypeT::COMMA)
		{
			tokenizer->Expect(TokenTypeT::COMMA);
			continue;
		}
		else if (t.type == TokenTypeT::GREATER)
		{
			tokenizer->Expect(TokenTypeT::GREATER);
			break;
		}
		else
		{
			// Unexpected token -> syntax error
			break;
		}
	}

	return instantiation;
}

static bool IsPrimitiveTypeName(const std::string& name)
{
	static const std::unordered_set<std::string> primitiveNames = {
		"uint8", "uint16", "uint32", "uint64",
		"int8", "int16", "int32", "int64",
		"real32", "real64", "bool", "char", "string", "void"
	};
	return primitiveNames.find(name) != primitiveNames.end();
}

static ValueType PrimitiveTypeFromName(const std::string& name)
{
	if (name == "uint8")   return ValueType::UINT8;
	if (name == "uint16")  return ValueType::UINT16;
	if (name == "uint32")  return ValueType::UINT32;
	if (name == "uint64")  return ValueType::UINT64;
	if (name == "int8")    return ValueType::INT8;
	if (name == "int16")   return ValueType::INT16;
	if (name == "int32")   return ValueType::INT32;
	if (name == "int64")   return ValueType::INT64;
	if (name == "real32")  return ValueType::REAL32;
	if (name == "real64")  return ValueType::REAL64;
	if (name == "bool")    return ValueType::BOOL;
	if (name == "char")    return ValueType::CHAR;
	if (name == "string")  return ValueType::STRING;
	if (name == "void")    return ValueType::VOID_T;
	return ValueType::LAST_TYPE;
}

uint32 Parser::AddTemplateInstantiationType(const std::string& baseName, const TemplateInstantiation& nested)
{
	if (IsPrimitiveTypeName(baseName))
	{
		ValueType type = PrimitiveTypeFromName(baseName);
		return static_cast<uint32>(type);
	}

	ID id = m_Program->GetClassID(baseName);
	if (id == INVALID_ID)
		return INVALID_ID;

	Class* cls = m_Program->GetClass(id);
	return cls->InstantiateTemplate(m_Program, nested);
}
