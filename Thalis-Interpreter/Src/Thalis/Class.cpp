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

void Class::AddMemberField(uint16 type, uint8 pointerLevel, uint32 arrayLength, uint64 size, uint64 offset, const std::string& name)
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
				program->AddVariableSetCommand(m_ScopeID, variableID);
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

uint64 Class::CalculateOffset(const std::vector<std::string>& members, TypeInfo* memberTypeInfo, uint32 currentMember, uint64 currentOffset)
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
				if (Value::IsPrimitiveType(member.type.type) || i + 1 == members.size())
				{
					*memberTypeInfo = member.type;
					return memberOffset;
				}

				// Recurse into sub-class
				Class* subClass = Program::GetCompiledProgram()->GetClass(member.type.type);
				if (!subClass)
					throw std::runtime_error("Invalid class type for member: " + member.name);

				return subClass->CalculateOffset(members, memberTypeInfo, i + 1, memberOffset);
			}
		}
	}

	// No member found
	return UINT64_MAX;
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
