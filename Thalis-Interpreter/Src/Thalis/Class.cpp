#include "Class.h"
#include "Program.h"
#include "Scope.h"

void Class::AddFunction(Function* function)
{
	m_Functions[function->name].push_back(function);

	std::string signature = function->GenerateSignature();

	if (m_FunctionDefinitionMap.find(signature) == m_FunctionDefinitionMap.end())
	{
		uint32 functionID = m_NextFunctionID++;
		m_FunctionDefinitionMap[signature] = functionID;
		m_FunctionMap.push_back(function);
		function->id = functionID;
	}

	if (function->name.find("~") != std::string::npos)
	{
		m_Destructor = function;
	}

	if ((function->name == m_Name) && (function->parameters.size() == 1))
	{
		if ((function->parameters[0].type.pointerLevel == 0) &&
			(function->parameters[0].type.type == m_ID))
		{
			m_CopyConstructor = function;
		}
	}
}

void Class::AddStaticField(Program* program, uint16 type, uint8 pointerLevel, uint32 arrayLength, uint64 size, const std::string& name, ASTExpression* initializeExpr)
{
	ClassField field;
	field.type.type = type;
	field.type.pointerLevel =			arrayLength > 0 ? 1 : pointerLevel;
	field.type.elementPointerLevel =	arrayLength > 0 ? pointerLevel : 0;
	field.arrayLength = arrayLength;
	field.size = size;
	field.offset = 0;
	field.name = name;
	field.initializeExpr = initializeExpr;
	program->GetScope(m_ScopeID)->GetVariableID(name, true);

	m_StaticFields.push_back(field);
}

void Class::AddMemberField(uint16 type, uint8 pointerLevel, uint32 arrayLength, uint64 size, uint64 offset, const std::string& name, const std::string& templateTypeName)
{
	ClassField field;
	field.type.type = type;
	field.type.pointerLevel = arrayLength > 0 ? 1 : pointerLevel;
	field.type.elementPointerLevel = arrayLength > 0 ? pointerLevel : 0;
	field.arrayLength = arrayLength;
	field.size = size;
	field.offset = offset;
	field.name = name;
	field.initializeExpr = nullptr;
	field.templateTypeName = templateTypeName;

	m_MemberFields.push_back(field);
}

ClassField Class::GetMemberFieldByOffset(uint64 offset) const
{
	for (uint32 i = 0; i < m_MemberFields.size(); i++)
	{
		if (m_MemberFields[i].offset == offset)
			return m_MemberFields[i];
	}

	return ClassField();
}

static int32 GetConversionScore(Program* program, const TypeInfo& from, const TypeInfo& to)
{
	if (from.pointerLevel != to.pointerLevel)
		return -1; // pointer mismatch not allowed

	if (from.type == to.type)
		return 0; // exact match

	if (!Value::IsPrimitiveType(from.type) && !Value::IsPrimitiveType(to.type))
	{
		Class* fromClass = program->GetClass(from.type);
		//TODO: check for inheritance
	}

	bool fromIsInt = Value::IsIntegerType(from.type);
	bool toIsInt = Value::IsIntegerType(to.type);
	bool fromIsReal = Value::IsRealType(from.type);
	bool toIsReal = Value::IsRealType(to.type);

	// integer -> integer (safe or narrowing)
	if (fromIsInt && toIsInt)
	{
		// Slightly penalize narrowing (e.g. int64 -> int32)
		if (program->GetTypeSize(from.type) > program->GetTypeSize(to.type))
			return 2; // narrowing
		else
			return 1; // widening or same range
	}

	// float -> float (e.g. real64 -> real32)
	if (fromIsReal && toIsReal)
	{
		if (program->GetTypeSize(from.type) > program->GetTypeSize(to.type))
			return 2; // narrowing float
		else
			return 1; // widening float
	}

	// int -> float
	if (fromIsInt && toIsReal)
		return 3; // less ideal but allowed

	// float -> int
	if (fromIsReal && toIsInt)
		return 4; // least ideal primitive cast

	return -1; // incompatible
}

uint32 Class::GetFunctionID(const std::string& name, const std::vector<ASTExpression*>& args)
{
	Program* program = Program::GetCompiledProgram();

	// First try exact signature match
	std::string exactSignature = Function::GenerateSignatureFromArgs(program, name, args);
	auto it = m_FunctionDefinitionMap.find(exactSignature);
	if (it != m_FunctionDefinitionMap.end())
		return it->second;

	Function* bestFunc = nullptr;
	uint32 bestID = INVALID_ID;
	int bestScore = INT_MAX;

	// Now try to find the best compatible function
	for (const auto& [sig, id] : m_FunctionDefinitionMap)
	{
		// Check if signature starts with "<name>-"
		if (sig.rfind(name + "-", 0) != 0)
			continue;

		Function* func = FindFunctionBySignature(sig);
		if (!func || func->parameters.size() != args.size())
			continue;

		int totalScore = 0;
		bool compatible = true;

		for (size_t i = 0; i < args.size(); ++i)
		{
			ASTExpression* arg = args[i];
			TypeInfo argType = arg->GetTypeInfo(program);
			const FunctionParameter& param = func->parameters[i];

			int score = GetConversionScore(program, argType, param.type);
			if (score < 0) { compatible = false; break; }
			totalScore += score;
		}

		if (compatible && totalScore < bestScore)
		{
			bestFunc = func;
			bestID = id;
			bestScore = totalScore;
		}
	}

	if (bestFunc)
		return bestID;

	// No match found
	return INVALID_ID;
}

Function* Class::FindFunctionBySignature(const std::string& signature)
{
	// Extract the function name before the first '-'
	size_t dashPos = signature.find('-');
	std::string name = (dashPos == std::string::npos) ? signature : signature.substr(0, dashPos);

	// Look up all overloads for this function name
	const auto it = m_Functions.find(name);
	if (it == m_Functions.end())
		return nullptr;

	// Search for a function with an exact matching signature
	for (Function* func : it->second)
	{
		std::string funcSig = func->GenerateSignature();
		if (funcSig == signature)
			return func;
	}

	return nullptr;
}

void Class::EmitCode(Program* program)
{
	for (uint32 i = 0; i < m_FunctionMap.size(); i++)
	{
		Function* function = m_FunctionMap[i];
		function->pc = program->GetCodeSize();
		for (uint32 i = 0; i < function->body.size(); i++)
		{
			function->body[i]->EmitCode(program);
		}
		if (function->returnInfo.type == (uint16)ValueType::VOID_T)
		{
			program->AddReturnCommand(false);
		}
	}
}

void Class::InitStatics(Program* program)
{
	Scope* scope = program->GetScope(m_ScopeID);
	for (uint32 i = 0; i < m_StaticFields.size(); i++)
	{
		const ClassField& field = m_StaticFields[i];
		ID variableID = scope->GetVariableID(field.name, true);

		if (field.initializeExpr)
		{
			if (field.arrayLength > 0)
			{
				std::cerr << "Cannot initialize static arrays yet" << std::endl;
			}
			else
			{
				field.initializeExpr->EmitCode(program);
				program->AddVariableSetCommand(m_ScopeID, variableID, INVALID_ID, INVALID_ID);
			}
		}

		if (field.arrayLength > 0)
		{
			scope->AddVariable(variableID, Value::MakeArray(program, field.type.type, field.type.pointerLevel, field.arrayLength, program->GetStackAllocator()));
		}
		else if (field.type.pointerLevel > 0)
		{
			scope->AddVariable(variableID, Value::MakeNULL(field.type.type, field.type.pointerLevel));
		}
		else
		{
			scope->AddVariable(variableID, Value::MakeDefaultValue(field.type.type, program->GetStackAllocator()));
		}
		
	}
}

uint64 Class::CalculateOffset(const std::vector<std::string>& members, TypeInfo* memberTypeInfo, bool* isTemplated, uint32 currentMember, uint64 currentOffset)
{
	for (uint32 i = currentMember; i < members.size(); i++)
	{
		for (uint32 j = 0; j < m_MemberFields.size(); j++)
		{
			const ClassField& member = m_MemberFields[j];
			if (members[i] == member.name)
			{
				uint64 memberOffset = member.offset + currentOffset;

				// If it's primitive or the last field, return the offset
				if (Value::IsPrimitiveType(member.type.type) || (i + 1) == members.size())
				{
					*memberTypeInfo = member.type;
					if (member.type.type == (uint16)ValueType::TEMPLATE_TYPE)
						*isTemplated = true;
					return memberOffset;
				}

				// Recurse into sub-class
				Class* subClass = Program::GetCompiledProgram()->GetClass(member.type.type);
				if (!subClass)
					throw std::runtime_error("Invalid class type for member: " + member.name);

				return subClass->CalculateOffset(members, memberTypeInfo, isTemplated, i + 1, memberOffset);
			}
		}
	}

	// No member found
	return UINT64_MAX;
}

ID Class::InstantiateTemplate(Program* program, const TemplateInstantiation& instantiation, ID generatedID)
{
	if (!IsTemplateClass()) return INVALID_ID;
	if (m_TemplateDefinition.parameters.size() != instantiation.args.size()) return INVALID_ID;

	ExecuteInstantiationCommands(program, instantiation);

	std::string name = GenerateTemplateClassName(program, m_Name, instantiation);
	ID classID = program->GetClassID(name);
	if (classID != INVALID_ID) 
	{
		if(generatedID != INVALID_ID)
		{
			Class* cls = program->GetClass(classID);
			program->AddClass2(cls, generatedID);
		}
		return classID;
	}

	Class* cls = new Class(name, m_ScopeID);
	classID = program->AddClass(name, cls, generatedID);
	cls->m_IsTemplateInstance = true;

	uint64 memberOffset = 0;
	for (uint32 i = 0; i < m_MemberFields.size(); i++)
	{
		const ClassField& member = m_MemberFields[i];
		TypeInfo typeInfo = member.type;

		if (typeInfo.type == (uint16)ValueType::TEMPLATE_TYPE)
		{
			uint32 index = InstantiateTemplateGetIndex(program, member.templateTypeName);
			typeInfo.type = instantiation.args[index].value;
			typeInfo.pointerLevel += instantiation.args[index].pointerLevel;
		}

		uint32 arrayLength = member.arrayLength;
		if (!member.arrayDimensionTemplateSize.empty())
		{
			uint32 index = InstantiateTemplateGetIndex(program, member.arrayDimensionTemplateSize);
			arrayLength = instantiation.args[index].value;
		}

		uint64 typeSize = (typeInfo.pointerLevel > 0) ? sizeof(void*) : program->GetTypeSize(typeInfo.type);
		cls->AddMemberField(typeInfo.type, typeInfo.pointerLevel, arrayLength, typeSize, memberOffset, member.name, "");

		memberOffset += typeSize;
	}

	cls->SetSize(memberOffset);

	for (uint32 i = 0; i < m_FunctionMap.size(); i++)
	{
		Function* function = m_FunctionMap[i];
		Function* injectedFunction = InstantiateTemplateInjectFunction(program, function, classID, name, instantiation, cls);
		cls->AddFunction(injectedFunction);
	}

	return classID;
}

ID Class::AddInstantiationCommand(TemplateInstantiationCommand* command)
{
	ID id = GenClassID();
	m_InstantiationCommands.push_back(std::make_pair(command, id));
	return id;
}

void Class::ExecuteInstantiationCommands(Program* program, const TemplateInstantiation& instantiation)
{
	for (uint32 i = 0; i < m_InstantiationCommands.size(); i++)
		ExecuteInstantiationCommand(program, m_InstantiationCommands[i].first, instantiation, m_InstantiationCommands[i].second);
}

uint32 Class::ExecuteInstantiationCommand(Program* program, TemplateInstantiationCommand* command, const TemplateInstantiation& instantiation, ID generatedID)
{
	TemplateInstantiation result;
	for (uint32 i = 0; i < command->args.size(); i++)
	{
		const TemplateInstantiationCommandArg& arg = command->args[i];
		if (arg.type == 0)
		{
			if (arg.arg.type == TemplateParameterType::TEMPLATE_TYPE)
			{
				TemplateArgument injectedArg;
				injectedArg.type = TemplateParameterType::TYPE;
				uint32 index = InstantiateTemplateGetIndex(program, arg.arg.templateTypeName);
				injectedArg.value = instantiation.args[index].value;
				result.args.push_back(injectedArg);
			}
			else if (arg.arg.type == TemplateParameterType::INT)
			{
				if (!arg.arg.templateTypeName.empty())
				{
					TemplateArgument injectedArg;
					injectedArg.type = TemplateParameterType::INT;
					uint32 index = InstantiateTemplateGetIndex(program, arg.arg.templateTypeName);
					injectedArg.value = instantiation.args[index].value;
					result.args.push_back(injectedArg);
				}
				else
				{
					result.args.push_back(arg.arg);
				}
			}
			else
			{
				result.args.push_back(arg.arg);
			}
		}
		else if (arg.type == 1)
		{
			TemplateArgument injectedArg;
			injectedArg.type = TemplateParameterType::TYPE;
			injectedArg.value = ExecuteInstantiationCommand(program, arg.command, instantiation, INVALID_ID);
			result.args.push_back(injectedArg);
		}
	}

	Class* cls = program->GetClass(command->type);
	uint16 type = cls->InstantiateTemplate(program, result, generatedID);
	return type;
}

int32 Class::InstantiateTemplateGetIndex(Program* program, const std::string& templateTypeName)
{
	for (uint32 i = 0; i < m_TemplateDefinition.parameters.size(); i++)
	{
		const TemplateParameter& param = m_TemplateDefinition.parameters[i];
		if (param.name == templateTypeName)
			return i;
	}

	return -1;
}

Function* Class::InstantiateTemplateInjectFunction(Program* program, Function* templatedFunction, ID templatedTypeID,
	const std::string& templatedTypeName, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	Scope* scope = program->GetScope(templatedFunction->scope);
	const std::unordered_map<ID, std::pair<TypeInfo, std::string>>& declaredVariables = scope->GetDeclaredVariables();

	Function* injectedFunction = new Function();
	injectedFunction->accessModifier = templatedFunction->accessModifier;
	injectedFunction->isStatic = templatedFunction->isStatic;
	injectedFunction->name = templatedFunction->name;
	injectedFunction->returnInfo = templatedFunction->returnInfo;
	injectedFunction->scope = program->CreateScope(m_ScopeID);

	if (templatedFunction->returnInfo.type == m_ID)
		injectedFunction->returnInfo.type = templatedTypeID;

	if (templatedFunction->name == m_Name) //Constructor so set new templated name
		injectedFunction->name = templatedTypeName;
	else
		injectedFunction->name = templatedFunction->name;

	Scope* injectedScope = program->GetScope(injectedFunction->scope);
	for (const auto& it : declaredVariables)
	{
		ID variableID = it.first;
		TypeInfo typeInfo = it.second.first;
		if (typeInfo.type == (uint16)ValueType::TEMPLATE_TYPE)
		{
			std::string templateTypeName = it.second.second;
			uint32 index = InstantiateTemplateGetIndex(program, templateTypeName);
			typeInfo.type = instantiation.args[index].value;
		}

		injectedScope->DeclareVariable(variableID, typeInfo, "");
	}

	if (injectedFunction->returnInfo.type == (uint16)ValueType::TEMPLATE_TYPE)
	{
		uint32 index = InstantiateTemplateGetIndex(program, templatedFunction->returnTemplateTypeName);
		injectedFunction->returnInfo.type = instantiation.args[index].value;
	}

	for (uint32 j = 0; j < templatedFunction->parameters.size(); j++)
	{
		FunctionParameter param = templatedFunction->parameters[j];
		if (param.type.type == (uint16)ValueType::TEMPLATE_TYPE)
		{
			uint32 index = InstantiateTemplateGetIndex(program, param.templateTypeName);
			param.type = instantiation.args[index].value;
			param.type.pointerLevel += instantiation.args[index].pointerLevel;
		}

		injectedFunction->parameters.push_back(param);
	}

	for (uint32 j = 0; j < templatedFunction->body.size(); j++)
	{
		ASTExpression* injectedExpr = templatedFunction->body[j]->InjectTemplateType(program, this, injectedFunction->scope, instantiation, templatedClass);
		injectedFunction->body.push_back(injectedExpr);
	}

	return injectedFunction;
}

static std::string GetPrimitiveTypeName(ValueType type)
{
	switch (type)
	{
	case ValueType::UINT8:   return "uint8";
	case ValueType::UINT16:  return "uint16";
	case ValueType::UINT32:  return "uint32";
	case ValueType::UINT64:  return "uint64";

	case ValueType::INT8:    return "int8";
	case ValueType::INT16:   return "int16";
	case ValueType::INT32:   return "int32";
	case ValueType::INT64:   return "int64";

	case ValueType::REAL32:  return "real32";
	case ValueType::REAL64:  return "real64";

	case ValueType::BOOL:    return "bool";
	case ValueType::CHAR:    return "char";
	case ValueType::STRING:  return "string";
	case ValueType::VOID_T:  return "void";

	default:                             return "unknown";
	}
}

std::string Class::GenerateTemplateClassName(Program* program, const std::string& className, const TemplateInstantiation& instantiation)
{
	std::string name = className + "<";
	for (uint32 i = 0; i < instantiation.args.size(); i++)
	{
		const TemplateArgument& arg = instantiation.args[i];
		if (arg.type == TemplateParameterType::TYPE)
		{
			name += "Type=";
			if (Value::IsPrimitiveType(arg.value))
				name += GetPrimitiveTypeName((ValueType)arg.value);
			else
				name += program->GetClass(arg.value)->GetName();
		}
		else if (arg.type == TemplateParameterType::INT)
		{
			name += "Int=";
			name += std::to_string(arg.value);
		}

		if ((i + 1) < instantiation.args.size())
		{
			name += ",";
		}
	}

	name += ">";

	return name;
}

std::string Function::GenerateSignature() const
{
	Program* program = Program::GetCompiledProgram();

	std::string signature = name + "-";

	for (uint32 i = 0; i < parameters.size(); i++)
	{
		const FunctionParameter& param = parameters[i];
		std::string typeString = program->GetTypeName(param.type.type);
		if (param.type.pointerLevel > 0)
			typeString += std::to_string(param.type.pointerLevel);

		signature += typeString;
		if (i + 1 < parameters.size())
			signature += "_";
	}

	return signature;
}

std::string Function::GenerateSignatureFromArgs(Program* program, const std::string& name, const std::vector<ASTExpression*>& args)
{
	std::string signature = name + "-";

	for (uint32 i = 0; i < args.size(); i++)
	{
		TypeInfo typeInfo = args[i]->GetTypeInfo(program);
		std::string typeName = program->GetTypeName(typeInfo.type);

		if (typeInfo.pointerLevel > 0)
			typeName += std::to_string(typeInfo.pointerLevel);

		signature += typeName;
		if (i + 1 < args.size())
			signature += "_";
	}

	return signature;
}

bool TemplateInstantiation::HasTemplatedType() const
{
	for (uint32 i = 0; i < args.size(); i++)
		if (args[i].value == (uint32)ValueType::TEMPLATE_TYPE)
			return true;

	return false;
}
