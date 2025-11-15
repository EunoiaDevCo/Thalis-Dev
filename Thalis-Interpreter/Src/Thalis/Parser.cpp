#include "Parser.h"
#include "Tokenizer.h"
#include "Class.h"
#include "Program.h"
#include "Scope.h"
#include "Modules/ModuleID.h"
#include "Modules/IOModule.h"
#include "Modules/MathModule.h"
#include "Modules/WindowModule.h"
#include "Modules/GLModule.h"
#include "Modules/MemModule.h"
#include "Modules/FSModule.h"
#include <filesystem>

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
		else if (builtInModule == "Math") { m_Program->AddModule("Math", MATH_MODULE_ID); MathModule::Init(); }
		else if (builtInModule == "Window") { m_Program->AddModule("Window", WINDOW_MODULE_ID); WindowModule::Init(); }
		else if (builtInModule == "GL") { m_Program->AddModule("GL", OPENGL_MODULE_ID); GLModule::Init(); }
		else if (builtInModule == "Mem") { m_Program->AddModule("Mem", MEM_MODULE_ID); MemModule::Init(); }
		else if (builtInModule == "FS") { m_Program->AddModule("FS", FS_MODULE_ID); FSModule::Init(m_Program); }

		tokenizer->Expect(TokenTypeT::SEMICOLON);
	}
	else if (token.type == TokenTypeT::STRING_LITERAL) //User defined module
	{
		std::string path(token.text, token.length);
		if (!WasFileAlreadyParsed(path))
		{
			Parse(path);
			m_ParsedFiles.push_back(std::filesystem::absolute(path).generic_string());
		}
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

		if (ParseFunction(tokenizer, cls, classScope))
		{
			continue;
		}

		//SkipStatement(tokenizer);
		break;
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

	if (!cls->HasCopyConstructor())
	{
		Function* function = new Function();
		function->accessModifier = AccessModifier::PUBLIC;
		function->isStatic = false;
		function->isVirtual = false;
		function->name = cls->GetName();
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

	if(tokenizer->Expect(TokenTypeT::OPEN_PAREN, &t))
	{
		return false;
	}

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
			{
				return false;
			}

			templateTypeName = std::string(typeToken.text, typeToken.length);
			if (cls->InstantiateTemplateGetIndex(m_Program, templateTypeName) == -1)
			{
				return false;
			}

			param.type.type = (uint16)ValueType::TEMPLATE_TYPE;
			param.templateTypeName = templateTypeName;
		}

		t = tokenizer->GetToken();
		if (t.type != TokenTypeT::IDENTIFIER)
		{ 
			delete function;
			return false; 
		}

		std::string paramName(t.text, t.length);
		param.variableID = m_Program->GetScope(functionScope)->GetVariableID(paramName, true);
		m_Program->GetScope(functionScope)->DeclareVariable(param.variableID, param.type, templateTypeName);

		function->parameters.push_back(param);

		t = tokenizer->GetToken();
		if (t.type == TokenTypeT::COMMA) continue;
		else if (t.type == TokenTypeT::CLOSE_PAREN) break;
		else 
		{
			return false;
		}
	}

	if (tokenizer->Expect(TokenTypeT::OPEN_BRACE))
	{
		delete function; return false;
	}

	while (true)
	{
		Token peek = tokenizer->PeekToken();
		if (peek.type == TokenTypeT::CLOSE_BRACE)
		{
			tokenizer->Expect(TokenTypeT::CLOSE_BRACE);
			break;
		}

		if (!ParseStatement(function, tokenizer, functionScope))
		{
			delete function;
			return false;
		}
	}

	cls->AddFunction(function);
	return true;
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
		typeSize += sizeof(ArrayHeader);
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
		uint32 finalOffset = *offset;
		if (arrayLength > 0)
			finalOffset += sizeof(ArrayHeader);

		cls->AddMemberField(type, pointerLevel, arrayLength, typeSize, *offset, name, templateTypeName);
		*offset += typeSize;
	}

	return true;
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
			std::string templateTypeName = "";
			if (type == INVALID_ID)
			{
				Class* cls = m_Program->GetClass(m_Program->GetClassID(g_CurrentClassName));
				const TemplateDefinition& tdef = cls->GetTemplateDefinition();
				bool foundTemplateParam = false;
				for (uint32 i = 0; i < tdef.parameters.size(); i++)
				{
					if (tdef.parameters[i].type == TemplateParameterType::TYPE &&
						tdef.parameters[i].name == identifier)
					{
						type = (uint16)ValueType::TEMPLATE_TYPE;
						templateTypeName = identifier;
						foundTemplateParam = true;
						break;
					}
				}

				if (!foundTemplateParam)
					return false;
			}

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

			m_Program->GetScope(currentScope)->DeclareVariable(variableID, TypeInfo(type, pointerLevel), templateTypeName, derivedType);
			ASTExpressionDeclarePointer* declarePointer = new ASTExpressionDeclarePointer(currentScope, variableID, type, pointerLevel, assignExpr);
			declarePointer->templateTypeName = templateTypeName;

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
			ASTExpression* assignExpr = nullptr;
			if (openParen.type == TokenTypeT::OPEN_PAREN)
			{
				tokenizer->Expect(TokenTypeT::OPEN_PAREN);
				ParseArguments(tokenizer, currentScope, args);
			}
			else if (openParen.type == TokenTypeT::EQUALS)
			{
				tokenizer->Expect(TokenTypeT::EQUALS);
				assignExpr = ParseExpression(tokenizer, currentScope);
			}

			if (tokenizer->Expect(TokenTypeT::SEMICOLON)) return false;

			Scope* scope = m_Program->GetScope(currentScope);
			ID variableID = scope->GetVariableID(varName);

			scope->DeclareVariable(variableID, TypeInfo(classID, 0));

			if (assignExpr != nullptr)
			{
				ASTExpressionDeclareObjectAssign* declareObjectExpr = new ASTExpressionDeclareObjectAssign(currentScope, classID, variableID, assignExpr);
				function->body.push_back(declareObjectExpr);
				return true;
			}
			else
			{
				ASTExpressionDeclareObject* declareObjectExpr = new ASTExpressionDeclareObject(currentScope, classID, variableID, args);
				function->body.push_back(declareObjectExpr);
				return true;
			}
		}
		else
		{
			tokenizer->SetPeek(t);
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
	else if (t.type == TokenTypeT::VOID_T)
	{
		declaringPrimitive = true;
		primitiveType = ValueType::VOID_T;
	}
	else
	{
		tokenizer->SetPeek(t);
		ASTExpression* expr = ParseExpression(tokenizer, currentScope);
		expr->isStatement = true;
		tokenizer->Expect(TokenTypeT::SEMICOLON);
		function->body.push_back(expr);
		return true;
	}

	if (declaringPrimitive)
	{
		uint8 pointerLevel = ParsePointerLevel(tokenizer);
		if (primitiveType == ValueType::VOID_T && pointerLevel == 0)
			return false;


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

	return false;
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
		tokenizer->Expect(TokenTypeT::NOT);
		ASTExpression* expr = ParseExpression(tokenizer, currentScope);
		ASTExpressionNot* notExpr = new ASTExpressionNot(currentScope, expr);
		return notExpr;
	} break;
	case TokenTypeT::MINUS: { // unary negation
		tokenizer->Expect(TokenTypeT::MINUS);
		ASTExpression* expr = ParseExpression(tokenizer, currentScope);
		ASTExpressionNegate* negateExpr = new ASTExpressionNegate(currentScope, expr);
		return negateExpr;
	} break;
	case TokenTypeT::OPEN_PAREN: {
		tokenizer->Expect(TokenTypeT::OPEN_PAREN);
		Token identifier = tokenizer->GetToken();
		bool isPointerCast = false;
		uint16 castToType = INVALID_ID;
		if (identifier.type == TokenTypeT::IDENTIFIER || Tokenizer::IsTokenPrimitiveType(identifier))
		{
			if (tokenizer->GetToken().type == TokenTypeT::ASTERISK)
			{
				std::string castToName(identifier.text, identifier.length);
				if (identifier.type == TokenTypeT::IDENTIFIER)
				{
					castToType = m_Program->GetClassID(castToName);
					if (castToType != INVALID_ID)
					{
						if (tokenizer->GetToken().type == TokenTypeT::CLOSE_PAREN)
						{
							isPointerCast = true;
						}
					}
				}
				else
				{
					if (tokenizer->GetToken().type == TokenTypeT::CLOSE_PAREN)
					{
						castToType = ParseType(identifier);
						isPointerCast = true;
					}
				}
			}
		}

		if (isPointerCast)
		{
			ASTExpression* expr = ParseExpression(tokenizer, currentScope);
			ASTExpressionPointerCast* pointerCastExpr = new ASTExpressionPointerCast(currentScope, castToType, expr);
			return pointerCastExpr;
		}
		else
		{
			tokenizer->SetPeek(tok);
			return ParsePostFix(tokenizer, currentScope);
		}
	}
	default:
		return ParsePostFix(tokenizer, currentScope);
	}

	return ParsePostFix(tokenizer, currentScope);
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
			ASTExpressionArithmaticEquals* aritmaticPlusEqualsExpr = new ASTExpressionArithmaticEquals(currentScope, expr, amountExpr, ASTOperator::ADD);
			return aritmaticPlusEqualsExpr;
		}
		else if (tok.type == TokenTypeT::MINUS_EQUALS)
		{
			tokenizer->GetToken();
			ASTExpression* amountExpr = ParseExpression(tokenizer, currentScope);
			ASTExpressionArithmaticEquals* aritmaticMinusEqualsExpr = new ASTExpressionArithmaticEquals(currentScope, expr, amountExpr, ASTOperator::MINUS);
			return aritmaticMinusEqualsExpr;
		}
		else if (tok.type == TokenTypeT::TIMES_EQUALS)
		{
			tokenizer->GetToken();
			ASTExpression* amountExpr = ParseExpression(tokenizer, currentScope);
			ASTExpressionArithmaticEquals* aritmaticTimesEqualsExpr = new ASTExpressionArithmaticEquals(currentScope, expr, amountExpr, ASTOperator::MULTIPLY);
			return aritmaticTimesEqualsExpr;
		}
		else if (tok.type == TokenTypeT::DIVIDE_EQUALS)
		{
			tokenizer->GetToken();
			ASTExpression* amountExpr = ParseExpression(tokenizer, currentScope);
			ASTExpressionArithmaticEquals* aritmaticDivideEqualsExpr = new ASTExpressionArithmaticEquals(currentScope, expr, amountExpr, ASTOperator::DIVIDE);
			return aritmaticDivideEqualsExpr;
		}
		else if (tok.type == TokenTypeT::MOD_EQUALS)
		{
			tokenizer->GetToken();
			ASTExpression* amountExpr = ParseExpression(tokenizer, currentScope);
			ASTExpressionArithmaticEquals* aritmaticPlusEqualsExpr = new ASTExpressionArithmaticEquals(currentScope, expr, amountExpr, ASTOperator::MOD);
			return aritmaticPlusEqualsExpr;
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
	else if (moduleName == "Math")
	{
		if (functionName == "Cos")				function = (uint32)MathModuleFunction::COS;
		else if (functionName == "Sin")			function = (uint32)MathModuleFunction::SIN;
		else if (functionName == "Tan")			function = (uint32)MathModuleFunction::TAN;
		else if (functionName == "ACos")		function = (uint32)MathModuleFunction::ACOS;
		else if (functionName == "ASin")		function = (uint32)MathModuleFunction::ASIN;
		else if (functionName == "ATan")		function = (uint32)MathModuleFunction::ATAN;
		else if (functionName == "ATan2")		function = (uint32)MathModuleFunction::ATAN2;
		else if (functionName == "Cosh")		function = (uint32)MathModuleFunction::COSH;
		else if (functionName == "Sinh")		function = (uint32)MathModuleFunction::SINH;
		else if (functionName == "Tanh")		function = (uint32)MathModuleFunction::TANH;
		else if (functionName == "ACosh")		function = (uint32)MathModuleFunction::ACOSH;
		else if (functionName == "ASinh")		function = (uint32)MathModuleFunction::ASINH;
		else if (functionName == "ATanh")		function = (uint32)MathModuleFunction::ATANH;
		else if (functionName == "DegToRad")	function = (uint32)MathModuleFunction::DEGTORAD;
		else if (functionName == "RadToDeg")	function = (uint32)MathModuleFunction::RADTODEG;
		else if (functionName == "Floor")		function = (uint32)MathModuleFunction::FLOOR;
		else if (functionName == "Ceil")		function = (uint32)MathModuleFunction::CEIL;
		else if (functionName == "Round")		function = (uint32)MathModuleFunction::ROUND;
		else if (functionName == "Min")			function = (uint32)MathModuleFunction::MIN;
		else if (functionName == "Max")			function = (uint32)MathModuleFunction::MAX;
		else if (functionName == "Clamp")		function = (uint32)MathModuleFunction::CLAMP;
		else if (functionName == "Lerp")		function = (uint32)MathModuleFunction::LERP;
		else if (functionName == "Abs")			function = (uint32)MathModuleFunction::ABS;
		else if (functionName == "Sqrt")		function = (uint32)MathModuleFunction::SQRT;
		else if (functionName == "Pow")			function = (uint32)MathModuleFunction::POW;
		else if (functionName == "Exp")			function = (uint32)MathModuleFunction::EXP;
		else if (functionName == "Log")			function = (uint32)MathModuleFunction::LOG;
		else if (functionName == "Log10")		function = (uint32)MathModuleFunction::LOG10;
		else if (functionName == "Log2")		function = (uint32)MathModuleFunction::LOG2;
		else if (functionName == "Mod")			function = (uint32)MathModuleFunction::MOD;
		else if (functionName == "Modf")		function = (uint32)MathModuleFunction::MODF;
	}
	else if (moduleName == "Window")
	{
		if (functionName == "Create")				function = (uint32)WindowModuleFunction::CREATE;
		else if (functionName == "Destroy")			function = (uint32)WindowModuleFunction::DESTROY;
		else if (functionName == "Update")			function = (uint32)WindowModuleFunction::UPDATE;
		else if (functionName == "Present")			function = (uint32)WindowModuleFunction::PRESENT;
		else if (functionName == "CheckForEvent")	function = (uint32)WindowModuleFunction::CHECK_FOR_EVENT;
		else if (functionName == "GetSize")			function = (uint32)WindowModuleFunction::GET_SIZE;
	}
	else if (moduleName == "Mem")
	{
		if (functionName == "Copy") function = (uint32)MemModuleFunction::COPY;
		else if (functionName == "Alloc") function = (uint32)MemModuleFunction::ALLOC;
	}
	else if (moduleName == "FS")
	{
		if (functionName == "ReadTextFile") function = (uint32)FSModuleFunction::READ_TEXT_FILE;
		else if (functionName == "ReadBinaryFile") function = (uint32)FSModuleFunction::READ_BINARY_FILE;
		else if (functionName == "OpenFile") function = (uint32)FSModuleFunction::OPEN_FILE;
		else if (functionName == "CloseFile") function = (uint32)FSModuleFunction::CLOSE_FILE;
		else if (functionName == "ReadLine") function = (uint32)FSModuleFunction::READ_LINE;
	}
	else if (moduleName == "GL")
	{
		if (functionName == "glInit") function = (uint32)GLModuleFunction::TGL_INIT;

		// Buffer Objects
		else if (functionName == "glGenBuffers") function = (uint32)GLModuleFunction::TGL_GEN_BUFFERS;
		else if (functionName == "glDeleteBuffers") function = (uint32)GLModuleFunction::TGL_DELETE_BUFFERS;
		else if (functionName == "glBindBuffer") function = (uint32)GLModuleFunction::TGL_BIND_BUFFER;
		else if (functionName == "glBufferData") function = (uint32)GLModuleFunction::TGL_BUFFER_DATA;
		else if (functionName == "glBufferSubData") function = (uint32)GLModuleFunction::TGL_BUFFER_SUB_DATA;
		else if (functionName == "glMapBuffer") function = (uint32)GLModuleFunction::TGL_MAP_BUFFER;
		else if (functionName == "glUnmapBuffer") function = (uint32)GLModuleFunction::TGL_UNMAP_BUFFER;

		// Vertex Arrays
		if (functionName == "glGenVertexArrays") function = (uint32)GLModuleFunction::TGL_GEN_VERTEX_ARRAYS;
		else if (functionName == "glDeleteVertexArrays") function = (uint32)GLModuleFunction::TGL_DELETE_VERTEX_ARRAYS;
		else if (functionName == "glBindVertexArray") function = (uint32)GLModuleFunction::TGL_BIND_VERTEX_ARRAY;
		else if (functionName == "glEnableVertexAttribArray") function = (uint32)GLModuleFunction::TGL_ENABLE_VERTEX_ATTRIB_ARRAY;
		else if (functionName == "glDisableVertexAttribArray") function = (uint32)GLModuleFunction::TGL_DISABLE_VERTEX_ATTRIB_ARRAY;
		else if (functionName == "glVertexAttribPointer") function = (uint32)GLModuleFunction::TGL_VERTEX_ATTRIB_POINTER;
		else if (functionName == "glVertexAttribIPointer") function = (uint32)GLModuleFunction::TGL_VERTEX_ATTRIB_IPOINTER;
		else if (functionName == "glVertexAttribDivisor") function = (uint32)GLModuleFunction::TGL_VERTEX_ATTRIB_DIVISOR;
		else if (functionName == "glBindVertexBuffer") function = (uint32)GLModuleFunction::TGL_BIND_VERTEX_BUFFER;
		else if (functionName == "glVertexAttribFormat") function = (uint32)GLModuleFunction::TGL_VERTEX_ATTRIB_FORMAT;
		else if (functionName == "glVertexAttribBinding") function = (uint32)GLModuleFunction::TGL_VERTEX_ATTRIB_BINDING;

		else if (functionName == "glDisable") function = (uint32)GLModuleFunction::TGL_DISABLE;
		else if (functionName == "glEnable") function = (uint32)GLModuleFunction::TGL_ENABLE;

		// Drawing Commands
		if (functionName == "glDrawArrays") function = (uint32)GLModuleFunction::TGL_DRAW_ARRAYS;
		else if (functionName == "glDrawElements") function = (uint32)GLModuleFunction::TGL_DRAW_ELEMENTS;
		else if (functionName == "glDrawElementsBaseVertex") function = (uint32)GLModuleFunction::TGL_DRAW_ELEMENTS_BASE_VERTEX;
		else if (functionName == "glDrawElementsInstanced") function = (uint32)GLModuleFunction::TGL_DRAW_ELEMENTS_INSTANCED;
		else if (functionName == "glDrawArraysInstanced") function = (uint32)GLModuleFunction::TGL_DRAW_ARRAYS_INSTANCED;
		else if (functionName == "glDrawRangeElements") function = (uint32)GLModuleFunction::TGL_DRAW_RANGE_ELEMENTS;
		else if (functionName == "glDrawBuffer") function = (uint32)GLModuleFunction::TGL_DRAW_BUFFER;
		else if (functionName == "glDrawBuffers") function = (uint32)GLModuleFunction::TGL_DRAW_BUFFERS;
		else if (functionName == "glClear") function = (uint32)GLModuleFunction::TGL_CLEAR;
		else if (functionName == "glClearColor") function = (uint32)GLModuleFunction::TGL_CLEAR_COLOR;
		else if (functionName == "glClearDepth") function = (uint32)GLModuleFunction::TGL_CLEAR_DEPTH;
		else if (functionName == "glClearStencil") function = (uint32)GLModuleFunction::TGL_CLEAR_STENCIL;
		else if (functionName == "glPolygonMode") function = (uint32)GLModuleFunction::TGL_POLYGON_MODE;
		else if (functionName == "glLineWidth") function = (uint32)GLModuleFunction::TGL_LINE_WIDTH;
		else if (functionName == "glPointSize") function = (uint32)GLModuleFunction::TGL_POINT_SIZE;
		else if (functionName == "glCullFace") function = (uint32)GLModuleFunction::TGL_CULL_FACE;
		else if (functionName == "glFrontFace") function = (uint32)GLModuleFunction::TGL_FRONT_FACE;
		else if (functionName == "glPolygonOffset") function = (uint32)GLModuleFunction::TGL_POLYGON_OFFSET;
		else if (functionName == "glScissor") function = (uint32)GLModuleFunction::TGL_SCISSOR;
		else if (functionName == "glViewport") function = (uint32)GLModuleFunction::TGL_VIEWPORT;

		// Framebuffers / Renderbuffers
		if (functionName == "glGenFramebuffers") function = (uint32)GLModuleFunction::TGL_GEN_FRAMEBUFFERS;
		else if (functionName == "glDeleteFramebuffers") function = (uint32)GLModuleFunction::TGL_DELETE_FRAMEBUFFERS;
		else if (functionName == "glBindFramebuffer") function = (uint32)GLModuleFunction::TGL_BIND_FRAMEBUFFER;
		else if (functionName == "glFramebufferTexture") function = (uint32)GLModuleFunction::TGL_FRAMEBUFFER_TEXTURE;
		else if (functionName == "glFramebufferTexture2D") function = (uint32)GLModuleFunction::TGL_FRAMEBUFFER_TEXTURE_2D;
		else if (functionName == "glFramebufferTextureLayer") function = (uint32)GLModuleFunction::TGL_FRAMEBUFFER_TEXTURE_LAYER;
		else if (functionName == "glFramebufferRenderbuffer") function = (uint32)GLModuleFunction::TGL_FRAMEBUFFER_RENDERBUFFER;
		else if (functionName == "glCheckFramebufferStatus") function = (uint32)GLModuleFunction::TGL_CHECK_FRAMEBUFFER_STATUS;
		else if (functionName == "glGenRenderbuffers") function = (uint32)GLModuleFunction::TGL_GEN_RENDERBUFFERS;
		else if (functionName == "glDeleteRenderbuffers") function = (uint32)GLModuleFunction::TGL_DELETE_RENDERBUFFERS;
		else if (functionName == "glBindRenderbuffer") function = (uint32)GLModuleFunction::TGL_BIND_RENDERBUFFER;
		else if (functionName == "glRenderbufferStorage") function = (uint32)GLModuleFunction::TGL_RENDERBUFFER_STORAGE;
		else if (functionName == "glRenderbufferStorageMultisample") function = (uint32)GLModuleFunction::TGL_RENDERBUFFER_STORAGE_MULTISAMPLE;
		else if (functionName == "glBlitFramebuffer") function = (uint32)GLModuleFunction::TGL_BLIT_FRAMEBUFFER;
		else if (functionName == "glReadBuffer") function = (uint32)GLModuleFunction::TGL_READ_BUFFER;
		else if (functionName == "glReadPixels") function = (uint32)GLModuleFunction::TGL_READ_PIXELS;
		else if (functionName == "glInvalidateFramebuffer") function = (uint32)GLModuleFunction::TGL_INVALIDATE_FRAMEBUFFER;
		else if (functionName == "glInvalidateSubFramebuffer") function = (uint32)GLModuleFunction::TGL_INVALIDATE_SUB_FRAMEBUFFER;


		// Shaders and Programs
		if (functionName == "glCreateShader") function = (uint32)GLModuleFunction::TGL_CREATE_SHADER;
		else if (functionName == "glShaderSource") function = (uint32)GLModuleFunction::TGL_SHADER_SOURCE;
		else if (functionName == "glCompileShader") function = (uint32)GLModuleFunction::TGL_COMPILE_SHADER;
		else if (functionName == "glDeleteShader") function = (uint32)GLModuleFunction::TGL_DELETE_SHADER;
		else if (functionName == "glCreateProgram") function = (uint32)GLModuleFunction::TGL_CREATE_PROGRAM;
		else if (functionName == "glAttachShader") function = (uint32)GLModuleFunction::TGL_ATTACH_SHADER;
		else if (functionName == "glDetachShader") function = (uint32)GLModuleFunction::TGL_DETACH_SHADER;
		else if (functionName == "glLinkProgram") function = (uint32)GLModuleFunction::TGL_LINK_PROGRAM;
		else if (functionName == "glValidateProgram") function = (uint32)GLModuleFunction::TGL_VALIDATE_PROGRAM;
		else if (functionName == "glDeleteProgram") function = (uint32)GLModuleFunction::TGL_DELETE_PROGRAM;
		else if (functionName == "glUseProgram") function = (uint32)GLModuleFunction::TGL_USE_PROGRAM;
		else if (functionName == "glGetShaderiv") function = (uint32)GLModuleFunction::TGL_GET_SHADERIV;
		else if (functionName == "glGetShaderInfoLog") function = (uint32)GLModuleFunction::TGL_GET_SHADER_INFO_LOG;
		else if (functionName == "glGetProgramiv") function = (uint32)GLModuleFunction::TGL_GET_PROGRAMIV;
		else if (functionName == "glGetProgramInfoLog") function = (uint32)GLModuleFunction::TGL_GET_PROGRAM_INFO_LOG;
		else if (functionName == "glGetActiveUniform") function = (uint32)GLModuleFunction::TGL_GET_ACTIVE_UNIFORM;
		else if (functionName == "glGetActiveAttrib") function = (uint32)GLModuleFunction::TGL_GET_ACTIVE_ATTRIBUTE;
		else if (functionName == "glGetUniformLocation") function = (uint32)GLModuleFunction::TGL_GET_UNIFORM_LOCATION;
		else if (functionName == "glGetAttribLocation") function = (uint32)GLModuleFunction::TGL_GET_ATTRIB_LOCATION;
		else if (functionName == "glUniform1i") function = (uint32)GLModuleFunction::TGL_UNIFORM_1I;
		else if (functionName == "glUniform1f") function = (uint32)GLModuleFunction::TGL_UNIFORM_1F;
		else if (functionName == "glUniform2f") function = (uint32)GLModuleFunction::TGL_UNIFORM_2F;
		else if (functionName == "glUniform3f") function = (uint32)GLModuleFunction::TGL_UNIFORM_3F;
		else if (functionName == "glUniform4f") function = (uint32)GLModuleFunction::TGL_UNIFORM_4F;
		else if (functionName == "glUniformMatrix4fv") function = (uint32)GLModuleFunction::TGL_UNIFORM_MATRIX4FV;
		else if (functionName == "glGetUniformfv") function = (uint32)GLModuleFunction::TGL_GET_UNIFORMFV;
		else if (functionName == "glGetUniformiv") function = (uint32)GLModuleFunction::TGL_GET_UNIFORMIV;
		else if (functionName == "glBindAttribLocation") function = (uint32)GLModuleFunction::TGL_BIND_ATTRIB_LOCATION;
		else if (functionName == "glGetProgramBinary") function = (uint32)GLModuleFunction::TGL_GET_PROGRAM_BINARY;
		else if (functionName == "glProgramBinary") function = (uint32)GLModuleFunction::TGL_PROGRAM_BINARY;
		else if (functionName == "glProgramParameteri") function = (uint32)GLModuleFunction::TGL_PROGRAM_PARAMETERI;
		else if (functionName == "glGetActiveUniformBlockiv") function = (uint32)GLModuleFunction::TGL_GET_ACTIVE_UNIFORM_BLOCKIV;
		else if (functionName == "glGetUniformBlockIndex") function = (uint32)GLModuleFunction::TGL_GET_UNIFORM_BLOCK_INDEX;
		else if (functionName == "glUniformBlockBinding") function = (uint32)GLModuleFunction::TGL_UNIFORM_BLOCK_BINDING;
		else if (functionName == "glDispatchCompute") function = (uint32)GLModuleFunction::TGL_DISPATCH_COMPUTE;
		else if (functionName == "glDispatchComputeIndirect") function = (uint32)GLModuleFunction::TGL_DISPATCH_COMPUTE_INDIRECT;

		// Textures
		if (functionName == "glGenTextures") function = (uint32)GLModuleFunction::TGL_GEN_TEXTURES;
		else if (functionName == "glDeleteTextures") function = (uint32)GLModuleFunction::TGL_DELETE_TEXTURES;
		else if (functionName == "glBindTexture") function = (uint32)GLModuleFunction::TGL_BIND_TEXTURE;
		else if (functionName == "glActiveTexture") function = (uint32)GLModuleFunction::TGL_ACTIVE_TEXTURE;
		else if (functionName == "glTexImage1D") function = (uint32)GLModuleFunction::TGL_TEX_IMAGE_1D;
		else if (functionName == "glTexImage2D") function = (uint32)GLModuleFunction::TGL_TEX_IMAGE_2D;
		else if (functionName == "glTexImage3D") function = (uint32)GLModuleFunction::TGL_TEX_IMAGE_3D;
		else if (functionName == "glTexSubImage1D") function = (uint32)GLModuleFunction::TGL_TEX_SUB_IMAGE_1D;
		else if (functionName == "glTexSubImage2D") function = (uint32)GLModuleFunction::TGL_TEX_SUB_IMAGE_2D;
		else if (functionName == "glTexSubImage3D") function = (uint32)GLModuleFunction::TGL_TEX_SUB_IMAGE_3D;
		//else if (functionName == "glCopyTexImage2D") function = (uint32)GLModuleFunction::TGL_COPY_TEX_IMAGE_2D;
		else if (functionName == "glCopyTexSubImage2D") function = (uint32)GLModuleFunction::TGL_COPY_TEX_SUB_IMAGE_2D;
		else if (functionName == "glCompressedTexImage2D") function = (uint32)GLModuleFunction::TGL_COMPRESSED_TEX_IMAGE_2D;
		else if (functionName == "glCompressedTexSubImage2D") function = (uint32)GLModuleFunction::TGL_COMPRESSED_TEX_SUB_IMAGE_2D;
		else if (functionName == "glGenerateMipmap") function = (uint32)GLModuleFunction::TGL_GENERATE_MIPMAP;
		else if (functionName == "glTexParameteri") function = (uint32)GLModuleFunction::TGL_TEX_PARAMETERI;
		else if (functionName == "glTexParameterf") function = (uint32)GLModuleFunction::TGL_TEX_PARAMETERF;
		else if (functionName == "glTexParameteriv") function = (uint32)GLModuleFunction::TGL_TEX_PARAMETERIV;
		else if (functionName == "glTexParameterfv") function = (uint32)GLModuleFunction::TGL_TEX_PARAMETERFV;
		else if (functionName == "glGetTexLevelParameteriv") function = (uint32)GLModuleFunction::TGL_GET_TEX_LEVEL_PARAMETERIV;
		//else if (functionName == "glGetTexLevelParameterfv") function = (uint32)GLModuleFunction::TGL_GET_TEX_LEVEL_PARAMETERFV;
		else if (functionName == "glGetTexImage") function = (uint32)GLModuleFunction::TGL_GET_TEX_IMAGE;
		else if (functionName == "glBindImageTexture") function = (uint32)GLModuleFunction::TGL_BIND_IMAGE_TEXTURE;
		else if (functionName == "glGetTexParameteriv") function = (uint32)GLModuleFunction::TGL_GET_TEX_PARAMETERIV;
		//else if (functionName == "glGetTexParameterfv") function = (uint32)GLModuleFunction::TGL_GET_TEX_PARAMETERFV;
		//else if (functionName == "glTextureView") function = (uint32)GLModuleFunction::TGL_TEXTURE_VIEW;
		else if (functionName == "glTexStorage1D") function = (uint32)GLModuleFunction::TGL_TEX_STORAGE_1D;
		else if (functionName == "glTexStorage2D") function = (uint32)GLModuleFunction::TGL_TEX_STORAGE_2D;
		else if (functionName == "glTexStorage3D") function = (uint32)GLModuleFunction::TGL_TEX_STORAGE_3D;
		//else if (functionName == "glTexBuffer") function = (uint32)GLModuleFunction::TGL_TEX_BUFFER;
		//else if (functionName == "glTexBufferRange") function = (uint32)GLModuleFunction::TGL_TEX_BUFFER_RANGE;
		//else if (functionName == "glBindTextureUnit") function = (uint32)GLModuleFunction::TGL_BIND_TEXTURE_UNIT;

	}

	ASTExpressionModuleFunctionCall* functionCallExpr = new ASTExpressionModuleFunctionCall(scope, moduleID, function, args);
	return functionCallExpr;
}

static ASTExpressionModuleConstant* MakeModuleConstant(ID scope, ID moduleID, const std::string& moduleName, const std::string& variableName)
{
	uint16 constant = 0;

	if (moduleName == "IO")
	{

	}
	else if (moduleName == "Math")
	{
		if (variableName == "PI") constant = (uint32)MathModuleConstant::PI;
		else if (variableName == "E") constant = (uint32)MathModuleConstant::E;
		else if (variableName == "TAU") constant = (uint32)MathModuleConstant::TAU;
	}
	else if (moduleName == "Window")
	{
		if (variableName == "CB_CREATE") constant = (uint32)WindowModuleConstant::CB_CREATE;
		if (variableName == "CB_CLOSE") constant = (uint32)WindowModuleConstant::CB_CLOSE;
		if (variableName == "CB_RESIZE") constant = (uint32)WindowModuleConstant::CB_RESIZE;
	}
	else if (moduleName == "GL")
	{
		if (variableName == "GL_ZERO") constant = (uint32)GLModuleConstant::TGL_ZERO;
		else if (variableName == "GL_ONE") constant = (uint32)GLModuleConstant::TGL_ONE;
		else if (variableName == "GL_FALSE") constant = (uint32)GLModuleConstant::TGL_FALSE;
		else if (variableName == "GL_TRUE") constant = (uint32)GLModuleConstant::TGL_TRUE;

		// -- Primitives / modes --
		if (variableName == "GL_POINTS") constant = (uint32)GLModuleConstant::TGL_POINTS;
		else if (variableName == "GL_LINES") constant = (uint32)GLModuleConstant::TGL_LINES;
		else if (variableName == "GL_LINE_LOOP") constant = (uint32)GLModuleConstant::TGL_LINE_LOOP;
		else if (variableName == "GL_LINE_STRIP") constant = (uint32)GLModuleConstant::TGL_LINE_STRIP;
		else if (variableName == "GL_TRIANGLES") constant = (uint32)GLModuleConstant::TGL_TRIANGLES;
		else if (variableName == "GL_TRIANGLE_STRIP") constant = (uint32)GLModuleConstant::TGL_TRIANGLE_STRIP;
		else if (variableName == "GL_TRIANGLE_FAN") constant = (uint32)GLModuleConstant::TGL_TRIANGLE_FAN;
		else if (variableName == "GL_LINES_ADJACENCY") constant = (uint32)GLModuleConstant::TGL_LINES_ADJACENCY;
		else if (variableName == "GL_LINE_STRIP_ADJACENCY") constant = (uint32)GLModuleConstant::TGL_LINE_STRIP_ADJACENCY;
		else if (variableName == "GL_TRIANGLES_ADJACENCY") constant = (uint32)GLModuleConstant::TGL_TRIANGLES_ADJACENCY;
		else if (variableName == "GL_TRIANGLE_STRIP_ADJACENCY") constant = (uint32)GLModuleConstant::TGL_TRIANGLE_STRIP_ADJACENCY;
		else if (variableName == "GL_PATCHES") constant = (uint32)GLModuleConstant::TGL_PATCHES;

		// -- Buffer binding targets --
		if (variableName == "GL_ARRAY_BUFFER") constant = (uint32)GLModuleConstant::TGL_ARRAY_BUFFER;
		else if (variableName == "GL_ELEMENT_ARRAY_BUFFER") constant = (uint32)GLModuleConstant::TGL_ELEMENT_ARRAY_BUFFER;
		else if (variableName == "GL_COPY_READ_BUFFER") constant = (uint32)GLModuleConstant::TGL_COPY_READ_BUFFER;
		else if (variableName == "GL_COPY_WRITE_BUFFER") constant = (uint32)GLModuleConstant::TGL_COPY_WRITE_BUFFER;
		else if (variableName == "GL_PIXEL_PACK_BUFFER") constant = (uint32)GLModuleConstant::TGL_PIXEL_PACK_BUFFER;
		else if (variableName == "GL_PIXEL_UNPACK_BUFFER") constant = (uint32)GLModuleConstant::TGL_PIXEL_UNPACK_BUFFER;
		else if (variableName == "GL_TRANSFORM_FEEDBACK_BUFFER") constant = (uint32)GLModuleConstant::TGL_TRANSFORM_FEEDBACK_BUFFER;
		else if (variableName == "GL_UNIFORM_BUFFER") constant = (uint32)GLModuleConstant::TGL_UNIFORM_BUFFER;
		else if (variableName == "GL_SHADER_STORAGE_BUFFER") constant = (uint32)GLModuleConstant::TGL_SHADER_STORAGE_BUFFER;
		else if (variableName == "GL_DISPATCH_INDIRECT_BUFFER") constant = (uint32)GLModuleConstant::TGL_DISPATCH_INDIRECT_BUFFER;
		else if (variableName == "GL_DRAW_INDIRECT_BUFFER") constant = (uint32)GLModuleConstant::TGL_DRAW_INDIRECT_BUFFER;
		else if (variableName == "GL_ATOMIC_COUNTER_BUFFER") constant = (uint32)GLModuleConstant::TGL_ATOMIC_COUNTER_BUFFER;
		else if (variableName == "GL_QUERY_BUFFER") constant = (uint32)GLModuleConstant::TGL_QUERY_BUFFER;
		else if (variableName == "GL_ACCELERATION_STRUCTURE_READ_ONLY_NV") constant = (uint32)GLModuleConstant::TGL_ACCELERATION_STRUCTURE_READ_ONLY_NV;
		else if (variableName == "GL_ACCELERATION_STRUCTURE_WRITE_ONLY_NV") constant = (uint32)GLModuleConstant::TGL_ACCELERATION_STRUCTURE_WRITE_ONLY_NV;

		// -- Usage hints --
		if (variableName == "GL_STATIC_DRAW") constant = (uint32)GLModuleConstant::TGL_STATIC_DRAW;
		else if (variableName == "GL_DYNAMIC_DRAW") constant = (uint32)GLModuleConstant::TGL_DYNAMIC_DRAW;
		else if (variableName == "GL_STREAM_DRAW") constant = (uint32)GLModuleConstant::TGL_STREAM_DRAW;
		else if (variableName == "GL_STATIC_READ") constant = (uint32)GLModuleConstant::TGL_STATIC_READ;
		else if (variableName == "GL_DYNAMIC_READ") constant = (uint32)GLModuleConstant::TGL_DYNAMIC_READ;
		else if (variableName == "GL_STREAM_READ") constant = (uint32)GLModuleConstant::TGL_STREAM_READ;
		else if (variableName == "GL_STATIC_COPY") constant = (uint32)GLModuleConstant::TGL_STATIC_COPY;
		else if (variableName == "GL_DYNAMIC_COPY") constant = (uint32)GLModuleConstant::TGL_DYNAMIC_COPY;
		else if (variableName == "GL_STREAM_COPY") constant = (uint32)GLModuleConstant::TGL_STREAM_COPY;
		else if (variableName == "GL_READ_ONLY") constant = (uint32)GLModuleConstant::TGL_READ_ONLY;
		else if (variableName == "GL_WRITE_ONLY") constant = (uint32)GLModuleConstant::TGL_WRITE_ONLY;
		else if (variableName == "GL_READ_WRITE") constant = (uint32)GLModuleConstant::TGL_READ_WRITE;

		// -- Texture targets / types --
		if (variableName == "GL_TEXTURE_1D") constant = (uint32)GLModuleConstant::TGL_TEXTURE_1D;
		else if (variableName == "GL_TEXTURE_2D") constant = (uint32)GLModuleConstant::TGL_TEXTURE_2D;
		else if (variableName == "GL_TEXTURE_3D") constant = (uint32)GLModuleConstant::TGL_TEXTURE_3D;
		else if (variableName == "GL_TEXTURE_1D_ARRAY") constant = (uint32)GLModuleConstant::TGL_TEXTURE_1D_ARRAY;
		else if (variableName == "GL_TEXTURE_2D_ARRAY") constant = (uint32)GLModuleConstant::TGL_TEXTURE_2D_ARRAY;
		else if (variableName == "GL_TEXTURE_RECTANGLE") constant = (uint32)GLModuleConstant::TGL_TEXTURE_RECTANGLE;
		else if (variableName == "GL_TEXTURE_CUBE_MAP") constant = (uint32)GLModuleConstant::TGL_TEXTURE_CUBE_MAP;
		else if (variableName == "GL_TEXTURE_CUBE_MAP_ARRAY") constant = (uint32)GLModuleConstant::TGL_TEXTURE_CUBE_MAP_ARRAY;
		else if (variableName == "GL_TEXTURE_BUFFER") constant = (uint32)GLModuleConstant::TGL_TEXTURE_BUFFER;
		else if (variableName == "GL_TEXTURE_2D_MULTISAMPLE") constant = (uint32)GLModuleConstant::TGL_TEXTURE_2D_MULTISAMPLE;
		else if (variableName == "GL_TEXTURE_2D_MULTISAMPLE_ARRAY") constant = (uint32)GLModuleConstant::TGL_TEXTURE_2D_MULTISAMPLE_ARRAY;

		// -- Texture parameters / filtering / wrapping --
		if (variableName == "GL_NEAREST") constant = (uint32)GLModuleConstant::TGL_NEAREST;
		else if (variableName == "GL_LINEAR") constant = (uint32)GLModuleConstant::TGL_LINEAR;
		else if (variableName == "GL_NEAREST_MIPMAP_NEAREST") constant = (uint32)GLModuleConstant::TGL_NEAREST_MIPMAP_NEAREST;
		else if (variableName == "GL_LINEAR_MIPMAP_NEAREST") constant = (uint32)GLModuleConstant::TGL_LINEAR_MIPMAP_NEAREST;
		else if (variableName == "GL_NEAREST_MIPMAP_LINEAR") constant = (uint32)GLModuleConstant::TGL_NEAREST_MIPMAP_LINEAR;
		else if (variableName == "GL_LINEAR_MIPMAP_LINEAR") constant = (uint32)GLModuleConstant::TGL_LINEAR_MIPMAP_LINEAR;
		else if (variableName == "GL_TEXTURE_MAG_FILTER") constant = (uint32)GLModuleConstant::TGL_TEXTURE_MAG_FILTER;
		else if (variableName == "GL_TEXTURE_MIN_FILTER") constant = (uint32)GLModuleConstant::TGL_TEXTURE_MIN_FILTER;
		else if (variableName == "GL_TEXTURE_WRAP_S") constant = (uint32)GLModuleConstant::TGL_TEXTURE_WRAP_S;
		else if (variableName == "GL_TEXTURE_WRAP_T") constant = (uint32)GLModuleConstant::TGL_TEXTURE_WRAP_T;
		else if (variableName == "GL_TEXTURE_WRAP_R") constant = (uint32)GLModuleConstant::TGL_TEXTURE_WRAP_R;
		else if (variableName == "GL_REPEAT") constant = (uint32)GLModuleConstant::TGL_REPEAT;
		else if (variableName == "GL_CLAMP_TO_EDGE") constant = (uint32)GLModuleConstant::TGL_CLAMP_TO_EDGE;
		else if (variableName == "GL_MIRRORED_REPEAT") constant = (uint32)GLModuleConstant::TGL_MIRRORED_REPEAT;
		else if (variableName == "GL_CLAMP_TO_BORDER") constant = (uint32)GLModuleConstant::TGL_CLAMP_TO_BORDER;

		// -- Internal formats, base formats --
		if (variableName == "GL_R8") constant = (uint32)GLModuleConstant::TGL_R8;
		else if (variableName == "GL_R16") constant = (uint32)GLModuleConstant::TGL_R16;
		else if (variableName == "GL_RG8") constant = (uint32)GLModuleConstant::TGL_RG8;
		else if (variableName == "GL_RG16") constant = (uint32)GLModuleConstant::TGL_RG16;
		else if (variableName == "GL_R16F") constant = (uint32)GLModuleConstant::TGL_R16F;
		else if (variableName == "GL_R32F") constant = (uint32)GLModuleConstant::TGL_R32F;
		else if (variableName == "GL_RG16F") constant = (uint32)GLModuleConstant::TGL_RG16F;
		else if (variableName == "GL_RG32F") constant = (uint32)GLModuleConstant::TGL_RG32F;
		else if (variableName == "GL_RGBA8") constant = (uint32)GLModuleConstant::TGL_RGBA8;
		else if (variableName == "GL_RGBA16") constant = (uint32)GLModuleConstant::TGL_RGBA16;
		else if (variableName == "GL_RGBA16F") constant = (uint32)GLModuleConstant::TGL_RGBA16F;
		else if (variableName == "GL_RGBA32F") constant = (uint32)GLModuleConstant::TGL_RGBA32F;
		else if (variableName == "GL_SRGB8_ALPHA8") constant = (uint32)GLModuleConstant::TGL_SRGB8_ALPHA8;
		else if (variableName == "GL_DEPTH_COMPONENT16") constant = (uint32)GLModuleConstant::TGL_DEPTH_COMPONENT16;
		else if (variableName == "GL_DEPTH_COMPONENT24") constant = (uint32)GLModuleConstant::TGL_DEPTH_COMPONENT24;
		else if (variableName == "GL_DEPTH_COMPONENT32F") constant = (uint32)GLModuleConstant::TGL_DEPTH_COMPONENT32F;
		else if (variableName == "GL_DEPTH24_STENCIL8") constant = (uint32)GLModuleConstant::TGL_DEPTH24_STENCIL8;
		else if (variableName == "GL_DEPTH32F_STENCIL8") constant = (uint32)GLModuleConstant::TGL_DEPTH32F_STENCIL8;
		else if (variableName == "GL_RGBA") constant = (uint32)GLModuleConstant::TGL_RGBA;

		// -- Pixel data types / formats --
		if (variableName == "GL_UNSIGNED_BYTE") constant = (uint32)GLModuleConstant::TGL_UNSIGNED_BYTE;
		else if (variableName == "GL_UNSIGNED_SHORT") constant = (uint32)GLModuleConstant::TGL_UNSIGNED_SHORT;
		else if (variableName == "GL_UNSIGNED_INT") constant = (uint32)GLModuleConstant::TGL_UNSIGNED_INT;
		else if (variableName == "GL_UNSIGNED_INT_24_8") constant = (uint32)GLModuleConstant::TGL_UNSIGNED_INT_24_8;
		else if (variableName == "GL_UNSIGNED_INT_2_10_10_10_REV") constant = (uint32)GLModuleConstant::TGL_UNSIGNED_INT_2_10_10_10_REV;
		else if (variableName == "GL_FLOAT") constant = (uint32)GLModuleConstant::TGL_FLOAT;
		else if (variableName == "GL_HALF_FLOAT") constant = (uint32)GLModuleConstant::TGL_HALF_FLOAT;
		else if (variableName == "GL_INT") constant = (uint32)GLModuleConstant::TGL_INT;
		else if (variableName == "GL_SHORT") constant = (uint32)GLModuleConstant::TGL_SHORT;
		else if (variableName == "GL_BYTE") constant = (uint32)GLModuleConstant::TGL_BYTE;
		else if (variableName == "GL_UNSIGNED_BYTE_3_3_2") constant = (uint32)GLModuleConstant::TGL_UNSIGNED_BYTE_3_3_2;
		else if (variableName == "GL_UNSIGNED_BYTE_2_3_3_REV") constant = (uint32)GLModuleConstant::TGL_UNSIGNED_BYTE_2_3_3_REV;

		else if (variableName == "GL_CW") constant = (uint32)GLModuleConstant::TGL_CW;
		else if (variableName == "GL_CCW") constant = (uint32)GLModuleConstant::TGL_CCW;

		// -- Shader / program / pipeline constants --
		if (variableName == "GL_VERTEX_SHADER") constant = (uint32)GLModuleConstant::TGL_VERTEX_SHADER;
		else if (variableName == "GL_FRAGMENT_SHADER") constant = (uint32)GLModuleConstant::TGL_FRAGMENT_SHADER;
		else if (variableName == "GL_GEOMETRY_SHADER") constant = (uint32)GLModuleConstant::TGL_GEOMETRY_SHADER;
		else if (variableName == "GL_TESS_CONTROL_SHADER") constant = (uint32)GLModuleConstant::TGL_TESS_CONTROL_SHADER;
		else if (variableName == "GL_TESS_EVALUATION_SHADER") constant = (uint32)GLModuleConstant::TGL_TESS_EVALUATION_SHADER;
		else if (variableName == "GL_COMPUTE_SHADER") constant = (uint32)GLModuleConstant::TGL_COMPUTE_SHADER;
		else if (variableName == "GL_PROGRAM") constant = (uint32)GLModuleConstant::TGL_PROGRAM;
		else if (variableName == "GL_PROGRAM_PIPELINE") constant = (uint32)GLModuleConstant::TGL_PROGRAM_PIPELINE;
		else if (variableName == "GL_COMPILE_STATUS") constant = (uint32)GLModuleConstant::TGL_COMPILE_STATUS;
		else if (variableName == "GL_LINK_STATUS") constant = (uint32)GLModuleConstant::TGL_LINK_STATUS;
		else if (variableName == "GL_VALIDATE_STATUS") constant = (uint32)GLModuleConstant::TGL_VALIDATE_STATUS;
		else if (variableName == "GL_INFO_LOG_LENGTH") constant = (uint32)GLModuleConstant::TGL_INFO_LOG_LENGTH;
		else if (variableName == "GL_ATTACHED_SHADERS") constant = (uint32)GLModuleConstant::TGL_ATTACHED_SHADERS;
		else if (variableName == "GL_ACTIVE_UNIFORMS") constant = (uint32)GLModuleConstant::TGL_ACTIVE_UNIFORMS;
		else if (variableName == "GL_ACTIVE_ATTRIBUTES") constant = (uint32)GLModuleConstant::TGL_ACTIVE_ATTRIBUTES;
		else if (variableName == "GL_ACTIVE_UNIFORM_BLOCKS") constant = (uint32)GLModuleConstant::TGL_ACTIVE_UNIFORM_BLOCKS;
		else if (variableName == "GL_ACTIVE_UNIFORM_MAX_LENGTH") constant = (uint32)GLModuleConstant::TGL_ACTIVE_UNIFORM_MAX_LENGTH;
		else if (variableName == "GL_ACTIVE_ATTRIBUTE_MAX_LENGTH") constant = (uint32)GLModuleConstant::TGL_ACTIVE_ATTRIBUTE_MAX_LENGTH;
		else if (variableName == "GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH") constant = (uint32)GLModuleConstant::TGL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH;

		// -- Uniform / attribute types & sampler types --
		if (variableName == "GL_INT_VEC2") constant = (uint32)GLModuleConstant::TGL_INT_VEC2;
		else if (variableName == "GL_INT_VEC3") constant = (uint32)GLModuleConstant::TGL_INT_VEC3;
		else if (variableName == "GL_INT_VEC4") constant = (uint32)GLModuleConstant::TGL_INT_VEC4;
		else if (variableName == "GL_BOOL") constant = (uint32)GLModuleConstant::TGL_BOOL;
		else if (variableName == "GL_BOOL_VEC2") constant = (uint32)GLModuleConstant::TGL_BOOL_VEC2;
		else if (variableName == "GL_BOOL_VEC3") constant = (uint32)GLModuleConstant::TGL_BOOL_VEC3;
		else if (variableName == "GL_BOOL_VEC4") constant = (uint32)GLModuleConstant::TGL_BOOL_VEC4;
		else if (variableName == "GL_FLOAT_VEC2") constant = (uint32)GLModuleConstant::TGL_FLOAT_VEC2;
		else if (variableName == "GL_FLOAT_VEC3") constant = (uint32)GLModuleConstant::TGL_FLOAT_VEC3;
		else if (variableName == "GL_FLOAT_VEC4") constant = (uint32)GLModuleConstant::TGL_FLOAT_VEC4;
		else if (variableName == "GL_FLOAT_MAT2") constant = (uint32)GLModuleConstant::TGL_FLOAT_MAT2;
		else if (variableName == "GL_FLOAT_MAT3") constant = (uint32)GLModuleConstant::TGL_FLOAT_MAT3;
		else if (variableName == "GL_FLOAT_MAT4") constant = (uint32)GLModuleConstant::TGL_FLOAT_MAT4;
		else if (variableName == "GL_SAMPLER_2D") constant = (uint32)GLModuleConstant::TGL_SAMPLER_2D;
		else if (variableName == "GL_SAMPLER_3D") constant = (uint32)GLModuleConstant::TGL_SAMPLER_3D;
		else if (variableName == "GL_SAMPLER_CUBE") constant = (uint32)GLModuleConstant::TGL_SAMPLER_CUBE;
		else if (variableName == "GL_SAMPLER_2D_ARRAY") constant = (uint32)GLModuleConstant::TGL_SAMPLER_2D_ARRAY;
		else if (variableName == "GL_SAMPLER_CUBE_MAP_ARRAY") constant = (uint32)GLModuleConstant::TGL_SAMPLER_CUBE_MAP_ARRAY;
		else if (variableName == "GL_SAMPLER_BUFFER") constant = (uint32)GLModuleConstant::TGL_SAMPLER_BUFFER;
		else if (variableName == "GL_SAMPLER_2D_MULTISAMPLE") constant = (uint32)GLModuleConstant::TGL_SAMPLER_2D_MULTISAMPLE;
		else if (variableName == "GL_SAMPLER_2D_MULTISAMPLE_ARRAY") constant = (uint32)GLModuleConstant::TGL_SAMPLER_2D_MULTISAMPLE_ARRAY;
		else if (variableName == "GL_SAMPLER_2D_SHADOW") constant = (uint32)GLModuleConstant::TGL_SAMPLER_2D_SHADOW;
		else if (variableName == "GL_SAMPLER_2D_ARRAY_SHADOW") constant = (uint32)GLModuleConstant::TGL_SAMPLER_2D_ARRAY_SHADOW;

		// -- Framebuffer / renderbuffer / attachments --
		if (variableName == "GL_FRAMEBUFFER") constant = (uint32)GLModuleConstant::TGL_FRAMEBUFFER;
		else if (variableName == "GL_READ_FRAMEBUFFER") constant = (uint32)GLModuleConstant::TGL_READ_FRAMEBUFFER;
		else if (variableName == "GL_DRAW_FRAMEBUFFER") constant = (uint32)GLModuleConstant::TGL_DRAW_FRAMEBUFFER;
		else if (variableName == "GL_RENDERBUFFER") constant = (uint32)GLModuleConstant::TGL_RENDERBUFFER;
		else if (variableName == "GL_COLOR_ATTACHMENT0") constant = (uint32)GLModuleConstant::TGL_COLOR_ATTACHMENT0;
		else if (variableName == "GL_COLOR_ATTACHMENT1") constant = (uint32)GLModuleConstant::TGL_COLOR_ATTACHMENT1;
		else if (variableName == "GL_COLOR_ATTACHMENT2") constant = (uint32)GLModuleConstant::TGL_COLOR_ATTACHMENT2;
		else if (variableName == "GL_COLOR_ATTACHMENT3") constant = (uint32)GLModuleConstant::TGL_COLOR_ATTACHMENT3;
		else if (variableName == "GL_DEPTH_ATTACHMENT") constant = (uint32)GLModuleConstant::TGL_DEPTH_ATTACHMENT;
		else if (variableName == "GL_STENCIL_ATTACHMENT") constant = (uint32)GLModuleConstant::TGL_STENCIL_ATTACHMENT;
		else if (variableName == "GL_DEPTH_STENCIL_ATTACHMENT") constant = (uint32)GLModuleConstant::TGL_DEPTH_STENCIL_ATTACHMENT;
		else if (variableName == "GL_FRAMEBUFFER_COMPLETE") constant = (uint32)GLModuleConstant::TGL_FRAMEBUFFER_COMPLETE;
		else if (variableName == "GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT") constant = (uint32)GLModuleConstant::TGL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT;
		else if (variableName == "GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT") constant = (uint32)GLModuleConstant::TGL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT;
		else if (variableName == "GL_FRAMEBUFFER_UNSUPPORTED") constant = (uint32)GLModuleConstant::TGL_FRAMEBUFFER_UNSUPPORTED;

		// -- State / capability flags --
		if (variableName == "GL_BLEND") constant = (uint32)GLModuleConstant::TGL_BLEND;
		else if (variableName == "GL_DEPTH_TEST") constant = (uint32)GLModuleConstant::TGL_DEPTH_TEST;
		else if (variableName == "GL_CULL_FACE") constant = (uint32)GLModuleConstant::TGL_CULL_FACE;
		else if (variableName == "GL_SCISSOR_TEST") constant = (uint32)GLModuleConstant::TGL_SCISSOR_TEST;
		else if (variableName == "GL_STENCIL_TEST") constant = (uint32)GLModuleConstant::TGL_STENCIL_TEST;
		else if (variableName == "GL_POLYGON_OFFSET_FILL") constant = (uint32)GLModuleConstant::TGL_POLYGON_OFFSET_FILL;
		else if (variableName == "GL_POLYGON_OFFSET_LINE") constant = (uint32)GLModuleConstant::TGL_POLYGON_OFFSET_LINE;
		else if (variableName == "GL_POLYGON_OFFSET_POINT") constant = (uint32)GLModuleConstant::TGL_POLYGON_OFFSET_POINT;
		else if (variableName == "GL_SAMPLE_ALPHA_TO_COVERAGE") constant = (uint32)GLModuleConstant::TGL_SAMPLE_ALPHA_TO_COVERAGE;
		else if (variableName == "GL_SAMPLE_COVERAGE") constant = (uint32)GLModuleConstant::TGL_SAMPLE_COVERAGE;
		else if (variableName == "GL_SAMPLE_SHADING") constant = (uint32)GLModuleConstant::TGL_SAMPLE_SHADING;
		else if (variableName == "GL_MULTISAMPLE") constant = (uint32)GLModuleConstant::TGL_MULTISAMPLE;
		else if (variableName == "GL_SAMPLE_MASK") constant = (uint32)GLModuleConstant::TGL_SAMPLE_MASK;
		else if (variableName == "GL_RASTERIZER_DISCARD") constant = (uint32)GLModuleConstant::TGL_RASTERIZER_DISCARD;

		// -- Blend / depth / stencil / logic ops --
		if (variableName == "GL_BLEND_SRC_RGB") constant = (uint32)GLModuleConstant::TGL_BLEND_SRC_RGB;
		else if (variableName == "GL_BLEND_DST_RGB") constant = (uint32)GLModuleConstant::TGL_BLEND_DST_RGB;
		else if (variableName == "GL_BLEND_SRC_ALPHA") constant = (uint32)GLModuleConstant::TGL_BLEND_SRC_ALPHA;
		else if (variableName == "GL_BLEND_DST_ALPHA") constant = (uint32)GLModuleConstant::TGL_BLEND_DST_ALPHA;
		else if (variableName == "GL_BLEND_EQUATION_RGB") constant = (uint32)GLModuleConstant::TGL_BLEND_EQUATION_RGB;
		else if (variableName == "GL_BLEND_EQUATION_ALPHA") constant = (uint32)GLModuleConstant::TGL_BLEND_EQUATION_ALPHA;
		else if (variableName == "GL_FUNC_ADD") constant = (uint32)GLModuleConstant::TGL_FUNC_ADD;
		else if (variableName == "GL_FUNC_SUBTRACT") constant = (uint32)GLModuleConstant::TGL_FUNC_SUBTRACT;
		else if (variableName == "GL_FUNC_REVERSE_SUBTRACT") constant = (uint32)GLModuleConstant::TGL_FUNC_REVERSE_SUBTRACT;
		else if (variableName == "GL_MIN") constant = (uint32)GLModuleConstant::TGL_MIN;
		else if (variableName == "GL_MAX") constant = (uint32)GLModuleConstant::TGL_MAX;
		else if (variableName == "GL_ONE_MINUS_SRC_ALPHA") constant = (uint32)GLModuleConstant::TGL_ONE_MINUS_SRC_ALPHA;
		else if (variableName == "GL_ONE_MINUS_DST_ALPHA") constant = (uint32)GLModuleConstant::TGL_ONE_MINUS_DST_ALPHA;
		else if (variableName == "GL_ONE_MINUS_SRC_COLOR") constant = (uint32)GLModuleConstant::TGL_ONE_MINUS_SRC_COLOR;
		else if (variableName == "GL_ONE_MINUS_DST_COLOR") constant = (uint32)GLModuleConstant::TGL_ONE_MINUS_DST_COLOR;

		// -- Query / timer / sync / occlusion etc. --
		if (variableName == "GL_QUERY_COUNTER_BITS") constant = (uint32)GLModuleConstant::TGL_QUERY_COUNTER_BITS;
		else if (variableName == "GL_CURRENT_QUERY") constant = (uint32)GLModuleConstant::TGL_CURRENT_QUERY;
		else if (variableName == "GL_QUERY_RESULT") constant = (uint32)GLModuleConstant::TGL_QUERY_RESULT;
		else if (variableName == "GL_QUERY_RESULT_AVAILABLE") constant = (uint32)GLModuleConstant::TGL_QUERY_RESULT_AVAILABLE;
		else if (variableName == "GL_SAMPLES_PASSED") constant = (uint32)GLModuleConstant::TGL_SAMPLES_PASSED;
		else if (variableName == "GL_PRIMITIVES_GENERATED") constant = (uint32)GLModuleConstant::TGL_PRIMITIVES_GENERATED;
		else if (variableName == "GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN") constant = (uint32)GLModuleConstant::TGL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN;
		else if (variableName == "GL_TIME_ELAPSED") constant = (uint32)GLModuleConstant::TGL_TIME_ELAPSED;
		else if (variableName == "GL_TIMESTAMP") constant = (uint32)GLModuleConstant::TGL_TIMESTAMP;

		// -- Misc / Other constants --
		if (variableName == "GL_VIEWPORT") constant = (uint32)GLModuleConstant::TGL_VIEWPORT;
		else if (variableName == "GL_SCISSOR_BOX") constant = (uint32)GLModuleConstant::TGL_SCISSOR_BOX;
		else if (variableName == "GL_COLOR_CLEAR_VALUE") constant = (uint32)GLModuleConstant::TGL_COLOR_CLEAR_VALUE;
		else if (variableName == "GL_DEPTH_CLEAR_VALUE") constant = (uint32)GLModuleConstant::TGL_DEPTH_CLEAR_VALUE;
		else if (variableName == "GL_STENCIL_CLEAR_VALUE") constant = (uint32)GLModuleConstant::TGL_STENCIL_CLEAR_VALUE;
		else if (variableName == "GL_COLOR_WRITEMASK") constant = (uint32)GLModuleConstant::TGL_COLOR_WRITEMASK;
		else if (variableName == "GL_DEPTH_WRITEMASK") constant = (uint32)GLModuleConstant::TGL_DEPTH_WRITEMASK;
		else if (variableName == "GL_STENCIL_WRITEMASK") constant = (uint32)GLModuleConstant::TGL_STENCIL_WRITEMASK;
		else if (variableName == "GL_STENCIL_BACK_WRITEMASK") constant = (uint32)GLModuleConstant::TGL_STENCIL_BACK_WRITEMASK;
		else if (variableName == "GL_MAX_VIEWPORT_DIMS") constant = (uint32)GLModuleConstant::TGL_MAX_VIEWPORT_DIMS;
		else if (variableName == "GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS") constant = (uint32)GLModuleConstant::TGL_MAX_COMBINED_TEXTURE_IMAGE_UNITS;
		else if (variableName == "GL_MAX_TEXTURE_IMAGE_UNITS") constant = (uint32)GLModuleConstant::TGL_MAX_TEXTURE_IMAGE_UNITS;
		else if (variableName == "GL_MAX_VERTEX_ATTRIBS") constant = (uint32)GLModuleConstant::TGL_MAX_VERTEX_ATTRIBS;
		else if (variableName == "GL_MAX_VERTEX_UNIFORM_COMPONENTS") constant = (uint32)GLModuleConstant::TGL_MAX_VERTEX_UNIFORM_COMPONENTS;
		else if (variableName == "GL_MAX_FRAGMENT_UNIFORM_COMPONENTS") constant = (uint32)GLModuleConstant::TGL_MAX_FRAGMENT_UNIFORM_COMPONENTS;
		else if (variableName == "GL_MAX_UNIFORM_BLOCK_SIZE") constant = (uint32)GLModuleConstant::TGL_MAX_UNIFORM_BLOCK_SIZE;
		else if (variableName == "GL_MAX_DRAW_BUFFERS") constant = (uint32)GLModuleConstant::TGL_MAX_DRAW_BUFFERS;
		else if (variableName == "GL_MAX_COLOR_ATTACHMENTS") constant = (uint32)GLModuleConstant::TGL_MAX_COLOR_ATTACHMENTS;
		else if (variableName == "GL_MAX_ARRAY_TEXTURE_LAYERS") constant = (uint32)GLModuleConstant::TGL_MAX_ARRAY_TEXTURE_LAYERS;
		else if (variableName == "GL_MAX_FRAMEBUFFER_WIDTH") constant = (uint32)GLModuleConstant::TGL_MAX_FRAMEBUFFER_WIDTH;
		else if (variableName == "GL_MAX_FRAMEBUFFER_HEIGHT") constant = (uint32)GLModuleConstant::TGL_MAX_FRAMEBUFFER_HEIGHT;
		else if (variableName == "GL_MAX_FRAMEBUFFER_LAYERS") constant = (uint32)GLModuleConstant::TGL_MAX_FRAMEBUFFER_LAYERS;
		else if (variableName == "GL_COLOR_BUFFER_BIT") constant = (uint32)GLModuleConstant::TGL_COLOR_BUFFER_BIT;
		else if (variableName == "GL_DEPTH_BUFFER_BIT") constant = (uint32)GLModuleConstant::TGL_DEPTH_BUFFER_BIT;
		else if (variableName == "GL_STENCIL_BUFFER_BIT") constant = (uint32)GLModuleConstant::TGL_STENCIL_BUFFER_BIT;

		if (variableName == "GL_DEBUG_OUTPUT") constant = (uint32)GLModuleConstant::TGL_DEBUG_OUTPUT;
		else if (variableName == "GL_DEBUG_OUTPUT_SYNCHRONOUS") constant = (uint32)GLModuleConstant::TGL_DEBUG_OUTPUT_SYNCHRONOUS;
		else if (variableName == "GL_DEBUG_SOURCE_API") constant = (uint32)GLModuleConstant::TGL_DEBUG_SOURCE_API;
		else if (variableName == "GL_DEBUG_SOURCE_WINDOW_SYSTEM") constant = (uint32)GLModuleConstant::TGL_DEBUG_SOURCE_WINDOW_SYSTEM;
		else if (variableName == "GL_DEBUG_SOURCE_SHADER_COMPILER") constant = (uint32)GLModuleConstant::TGL_DEBUG_SOURCE_SHADER_COMPILER;
		else if (variableName == "GL_DEBUG_SOURCE_THIRD_PARTY") constant = (uint32)GLModuleConstant::TGL_DEBUG_SOURCE_THIRD_PARTY;
		else if (variableName == "GL_DEBUG_SOURCE_APPLICATION") constant = (uint32)GLModuleConstant::TGL_DEBUG_SOURCE_APPLICATION;
		else if (variableName == "GL_DEBUG_SOURCE_OTHER") constant = (uint32)GLModuleConstant::TGL_DEBUG_SOURCE_OTHER;
		else if (variableName == "GL_DEBUG_TYPE_ERROR") constant = (uint32)GLModuleConstant::TGL_DEBUG_TYPE_ERROR;
		else if (variableName == "GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR") constant = (uint32)GLModuleConstant::TGL_DEBUG_TYPE_DEPRECATED_BEHAVIOR;
		else if (variableName == "GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR") constant = (uint32)GLModuleConstant::TGL_DEBUG_TYPE_UNDEFINED_BEHAVIOR;
		else if (variableName == "GL_DEBUG_TYPE_PORTABILITY") constant = (uint32)GLModuleConstant::TGL_DEBUG_TYPE_PORTABILITY;
		else if (variableName == "GL_DEBUG_TYPE_PERFORMANCE") constant = (uint32)GLModuleConstant::TGL_DEBUG_TYPE_PERFORMANCE;
		else if (variableName == "GL_DEBUG_TYPE_MARKER") constant = (uint32)GLModuleConstant::TGL_DEBUG_TYPE_MARKER;
		else if (variableName == "GL_DEBUG_TYPE_PUSH_GROUP") constant = (uint32)GLModuleConstant::TGL_DEBUG_TYPE_PUSH_GROUP;
		else if (variableName == "GL_DEBUG_TYPE_POP_GROUP") constant = (uint32)GLModuleConstant::TGL_DEBUG_TYPE_POP_GROUP;
		else if (variableName == "GL_DEBUG_TYPE_OTHER") constant = (uint32)GLModuleConstant::TGL_DEBUG_TYPE_OTHER;
		else if (variableName == "GL_DEBUG_SEVERITY_HIGH") constant = (uint32)GLModuleConstant::TGL_DEBUG_SEVERITY_HIGH;
		else if (variableName == "GL_DEBUG_SEVERITY_MEDIUM") constant = (uint32)GLModuleConstant::TGL_DEBUG_SEVERITY_MEDIUM;
		else if (variableName == "GL_DEBUG_SEVERITY_LOW") constant = (uint32)GLModuleConstant::TGL_DEBUG_SEVERITY_LOW;
		else if (variableName == "GL_DEBUG_SEVERITY_NOTIFICATION") constant = (uint32)GLModuleConstant::TGL_DEBUG_SEVERITY_NOTIFICATION;

		if (variableName == "GL_TEXTURE0") constant = (uint32)GLModuleConstant::TGL_TEXTURE0;
		else if (variableName == "GL_TEXTURE1") constant = (uint32)GLModuleConstant::TGL_TEXTURE1;
		else if (variableName == "GL_TEXTURE2") constant = (uint32)GLModuleConstant::TGL_TEXTURE2;
		else if (variableName == "GL_TEXTURE3") constant = (uint32)GLModuleConstant::TGL_TEXTURE3;
		else if (variableName == "GL_TEXTURE4") constant = (uint32)GLModuleConstant::TGL_TEXTURE4;
		else if (variableName == "GL_TEXTURE5") constant = (uint32)GLModuleConstant::TGL_TEXTURE5;
		else if (variableName == "GL_TEXTURE6") constant = (uint32)GLModuleConstant::TGL_TEXTURE6;
		else if (variableName == "GL_TEXTURE7") constant = (uint32)GLModuleConstant::TGL_TEXTURE7;
		else if (variableName == "GL_TEXTURE8") constant = (uint32)GLModuleConstant::TGL_TEXTURE8;
		else if (variableName == "GL_TEXTURE9") constant = (uint32)GLModuleConstant::TGL_TEXTURE9;
		else if (variableName == "GL_TEXTURE10") constant = (uint32)GLModuleConstant::TGL_TEXTURE10;
		else if (variableName == "GL_TEXTURE11") constant = (uint32)GLModuleConstant::TGL_TEXTURE11;
		else if (variableName == "GL_TEXTURE12") constant = (uint32)GLModuleConstant::TGL_TEXTURE12;
		else if (variableName == "GL_TEXTURE13") constant = (uint32)GLModuleConstant::TGL_TEXTURE13;
		else if (variableName == "GL_TEXTURE14") constant = (uint32)GLModuleConstant::TGL_TEXTURE14;
		else if (variableName == "GL_TEXTURE15") constant = (uint32)GLModuleConstant::TGL_TEXTURE15;
		else if (variableName == "GL_TEXTURE16") constant = (uint32)GLModuleConstant::TGL_TEXTURE16;
		else if (variableName == "GL_TEXTURE17") constant = (uint32)GLModuleConstant::TGL_TEXTURE17;
		else if (variableName == "GL_TEXTURE18") constant = (uint32)GLModuleConstant::TGL_TEXTURE18;
		else if (variableName == "GL_TEXTURE19") constant = (uint32)GLModuleConstant::TGL_TEXTURE19;
		else if (variableName == "GL_TEXTURE20") constant = (uint32)GLModuleConstant::TGL_TEXTURE20;
		else if (variableName == "GL_TEXTURE21") constant = (uint32)GLModuleConstant::TGL_TEXTURE21;
		else if (variableName == "GL_TEXTURE22") constant = (uint32)GLModuleConstant::TGL_TEXTURE22;
		else if (variableName == "GL_TEXTURE23") constant = (uint32)GLModuleConstant::TGL_TEXTURE23;
		else if (variableName == "GL_TEXTURE24") constant = (uint32)GLModuleConstant::TGL_TEXTURE24;
		else if (variableName == "GL_TEXTURE25") constant = (uint32)GLModuleConstant::TGL_TEXTURE25;
		else if (variableName == "GL_TEXTURE26") constant = (uint32)GLModuleConstant::TGL_TEXTURE26;
		else if (variableName == "GL_TEXTURE27") constant = (uint32)GLModuleConstant::TGL_TEXTURE27;
		else if (variableName == "GL_TEXTURE28") constant = (uint32)GLModuleConstant::TGL_TEXTURE28;
		else if (variableName == "GL_TEXTURE29") constant = (uint32)GLModuleConstant::TGL_TEXTURE29;
		else if (variableName == "GL_TEXTURE30") constant = (uint32)GLModuleConstant::TGL_TEXTURE30;
		else if (variableName == "GL_TEXTURE31") constant = (uint32)GLModuleConstant::TGL_TEXTURE31;
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
		ASTExpressionLiteral* literalExpr = new ASTExpressionLiteral(currentScope, Value::MakeNULL(INVALID_ID, 0));
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
	else if (t.type == TokenTypeT::STRLEN)
	{
		if (tokenizer->Expect(TokenTypeT::OPEN_PAREN)) return nullptr;
		ASTExpression* expr = ParseExpression(tokenizer, currentScope);
		if (tokenizer->Expect(TokenTypeT::CLOSE_PAREN)) return nullptr;
		ASTExpressionStrlen* strlenExpr = new ASTExpressionStrlen(currentScope, expr);
		return strlenExpr;
	}
	else if (t.type == TokenTypeT::SIZE_OF)
	{
		if (tokenizer->Expect(TokenTypeT::OPEN_PAREN)) return nullptr;
		Token typeToken = tokenizer->GetToken();
		uint16 type = ParseType(typeToken);
		if (tokenizer->Expect(TokenTypeT::CLOSE_PAREN)) return nullptr;
		ASTExpressionSizeOfStatic* sizeofExpr = new ASTExpressionSizeOfStatic(currentScope, type);
		return sizeofExpr;
	}
	else if (t.type == TokenTypeT::OFFSETOF)//TODO: Allow for deeper access
	{
		if (tokenizer->Expect(TokenTypeT::OPEN_PAREN)) return nullptr;
		Token typeToken = tokenizer->GetToken();
		uint16 type = ParseType(typeToken);
		if (tokenizer->Expect(TokenTypeT::DOT)) return nullptr;
		Token memberToken;
		if (tokenizer->Expect(TokenTypeT::IDENTIFIER, &memberToken)) return nullptr;
		if (tokenizer->Expect(TokenTypeT::CLOSE_PAREN)) return nullptr;
		
		Class* cls = m_Program->GetClass(type);
		TypeInfo typeInfo; bool templatedType = false;
		std::vector<std::string> members;
		members.push_back(std::string(memberToken.text, memberToken.length));
		uint64 offset = cls->CalculateOffset(members, &typeInfo, &templatedType);
		ASTExpressionOffsetOf* offsetofExpr = new ASTExpressionOffsetOf(currentScope, offset);
		return offsetofExpr;
	}
	else if (t.type == TokenTypeT::IDENTIFIER || t.type == TokenTypeT::THIS)
	{
		Token next = tokenizer->GetToken();

		if (t.type == TokenTypeT::THIS && next.type != TokenTypeT::DOT)
		{
			tokenizer->SetPeek(next);
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

				TypeInfo objTypeInfo = objExpr->GetTypeInfo(m_Program);

				Class* cls = m_Program->GetClass(objTypeInfo.type);

				if (functionName == "TrimBeginning")
				{
					uint32 bp = 0;
				}

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

				if (variableID == INVALID_ID)
				{
					Class* thisClass = m_Program->GetClass(m_Program->GetClassID(g_CurrentClassName));
					
					
					TypeInfo memberTypeInfo;
					bool templatedType = false;
					std::vector<std::string> members;
					members.push_back(variableName);
					uint64 offset = thisClass->CalculateOffset(members, &memberTypeInfo, &templatedType);
					if (offset == UINT64_MAX) return nullptr;

					ASTExpressionDirectMemberAccess* memberAccess = new ASTExpressionDirectMemberAccess(currentScope, memberTypeInfo, offset, nullptr);
					

					if (!identifiers.empty())
					{
						ASTExpressionIndex* indexExpr_ = new ASTExpressionIndex(currentScope, memberAccess, indexExpr, nullptr);

						Class* memberClass = m_Program->GetClass(memberTypeInfo.type);
						TypeInfo subMemberTypeInfo;
						bool subTemplatedType = false;
						uint64 subOffset = memberClass->CalculateOffset(identifiers, &subMemberTypeInfo, &subTemplatedType);
						ASTExpressionAccessMemberFromStack* accessExpr = new ASTExpressionAccessMemberFromStack(currentScope, indexExpr_, subMemberTypeInfo, subOffset, assignExpr);
						if (subTemplatedType)
						{
							accessExpr->members = identifiers;
						}

						return accessExpr;
					}
					else
					{
						ASTExpressionIndex* indexExpr_ = new ASTExpressionIndex(currentScope, memberAccess, indexExpr, assignExpr);
						return indexExpr_;
					}
				}

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
		else if (next.type == TokenTypeT::LESS)
		{
			std::string baseTypeName(t.text, t.length);

			ID classID = m_Program->GetClassID(baseTypeName);
			if (classID == INVALID_ID)
			{
				std::string variableName(t.text, t.length);
				ID variableID = m_Program->GetScope(currentScope)->GetVariableID(variableName, false);
				if ((variableID == INVALID_ID))
				{
					tokenizer->at = next.text; // rewind, it's a variable
					ID classID = m_Program->GetClassID(g_CurrentClassName);
					Class* typeClass = m_Program->GetClass(classID);
					std::vector<std::string> members;
					members.push_back(variableName);
					TypeInfo memberTypeInfo;
					bool templatedType = false;
					uint64 offset = typeClass->CalculateOffset(members, &memberTypeInfo, &templatedType);
					if (offset == UINT64_MAX) return nullptr;

					if (variableID != INVALID_ID)
					{
						ASTExpressionDirectMemberAccess* memberAccessExpr = new ASTExpressionDirectMemberAccess(currentScope, memberTypeInfo, offset, nullptr);
						return memberAccessExpr;
					}

					ASTExpressionTemplateLiteral* templateLiteralExpr = new ASTExpressionTemplateLiteral(currentScope, variableName);
					return templateLiteralExpr;
				}
				else
				{
					tokenizer->SetPeek(next);
					ASTExpressionVariable* variable = new ASTExpressionVariable(currentScope, variableID);
					return variable;
				}
			}
			TemplateInstantiationCommand* command = new TemplateInstantiationCommand();
			bool templatedType = false;
			Class* cls = m_Program->GetClass(m_Program->GetClassID(baseTypeName));

			TemplateInstantiation instantiation = ParseTemplateInstantiation(tokenizer, cls, command, &templatedType);
			if (!templatedType)
				delete command;

			if (tokenizer->Expect(TokenTypeT::OPEN_PAREN)) return false;
			std::vector<ASTExpression*> argExprs;
			ParseArguments(tokenizer, currentScope, argExprs);

			uint16 type = cls->InstantiateTemplate(m_Program, instantiation);
			ASTExpressionConstructorCall* constructorCallExpr = new ASTExpressionConstructorCall(currentScope, type, argExprs);
			return constructorCallExpr;
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

				tokenizer->SetPeek(next); // rewind, it's a variable
				ASTExpressionDirectMemberAccess* directMemberAccessExpr = new ASTExpressionDirectMemberAccess(currentScope, memberTypeInfo, offset, nullptr);
				if (templatedType)
					directMemberAccessExpr->members = members;
				return directMemberAccessExpr;
			}
			else
			{
				tokenizer->SetPeek(next); // rewind, it's a variable
				ASTExpressionVariable* variableExpr = new ASTExpressionVariable(currentScope, variableID);
				return variableExpr;
			}
		}
	}

	return nullptr;
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

bool Parser::WasFileAlreadyParsed(const std::string& file)
{
	std::string absPath = std::filesystem::absolute(file).generic_string();
	for (uint32 i = 0; i < m_ParsedFiles.size(); i++)
		if (absPath == m_ParsedFiles[i])
			return true;

	return false;
}
