#include "Scope.h"
#include "Program.h"
#include "Class.h"

Scope::Scope(Scope* parent) :
	m_Parent(parent)
{
	m_Storage.reserve(64);
}

void Scope::AddVariable(ID variableID, const Value& value)
{
	m_Storage.push_back(value);
	uint32 index = static_cast<uint32>(m_Storage.size() - 1);
	m_VariableMap[variableID] = index;
}

void Scope::AddVariableTest(ID variableID, const Value& value)
{
	uint32 index = m_Storage.size();
}

Value* Scope::GetVariable(ID variableID)
{
	const auto&& it = m_VariableMap.find(variableID);
	if (it == m_VariableMap.end())
	{
		if (m_Parent)
			return m_Parent->GetVariable(variableID);
		else
			return nullptr;
	}

	return &m_Storage[it->second];
}

ID Scope::GetVariableID(const std::string& name, bool makeID)
{
	if (m_Parent)
	{
		ID variableID = m_Parent->GetVariableID(name, false);
		if (variableID != INVALID_ID)
			return variableID;
	}

	auto it = m_VariableNameMap.find(name);
	if (it != m_VariableNameMap.end())
		return it->second;

	if (makeID)
	{
		ID newID = GenVariableID();
		m_VariableNameMap[name] = newID;
		return newID;
	}
	else
	{
		return INVALID_ID;
	}
}

void Scope::DeclareVariable(ID variableID, const TypeInfo& typeInfo, const std::string& templateTypeName, uint16 derivedType)
{
	DeclaredVariable variable;
	variable.type = typeInfo;
	variable.templateTypeName = templateTypeName;
	variable.derivedType = derivedType == INVALID_ID ? typeInfo.type : derivedType;
	m_VariableTypes[variableID] = variable;
}

TypeInfo Scope::GetVariableTypeInfo(ID variableID)
{
	const auto&& it = m_VariableTypes.find(variableID);
	if (it == m_VariableTypes.end())
	{
		if (m_Parent) return m_Parent->GetVariableTypeInfo(variableID);
	}

	if (it == m_VariableTypes.end()) return TypeInfo(INVALID_ID, 0);
	return it->second.type;
}

TypeInfo Scope::GetVariableDerivedTypeInfo(ID variableID)
{
	const auto&& it = m_VariableTypes.find(variableID);
	if (it == m_VariableTypes.end())
	{
		if (m_Parent) return m_Parent->GetVariableDerivedTypeInfo(variableID);
	}

	if (it == m_VariableTypes.end()) return TypeInfo(INVALID_ID, 0);
	TypeInfo typeInfo = it->second.type;
	typeInfo.type = it->second.derivedType;
	return typeInfo;
}

static void AddDestructorRecursive(Program* program, const Value& value, uint32 offset = 0)
{
	if (value.IsPrimitive() || value.IsPointer())
		return;

	Class* cls = program->GetClass(value.type);
	if (!cls)
		return;

	const std::vector<ClassField>& members = cls->GetMemberFields();
	for (int32 i = (int32)members.size() - 1; i >= 0; i--)
	{
		const ClassField& field = members[i];

		if (Value::IsPrimitiveType(field.type.type) || field.type.pointerLevel > 0)
			continue;

		Value member;
		member.type = field.type.type;
		member.pointerLevel = 0;
		member.data = (uint8*)value.data + field.offset;

		AddDestructorRecursive(program, member);
	}

	program->AddPendingDestructor(value);
}

void Scope::Clear(Program* program)
{
	for (Value& value : m_Storage)
	{
		AddDestructorRecursive(program, value);
	}

	for (Value& value : m_Temps)
	{
		AddDestructorRecursive(program, value);
	}

	m_VariableMap.clear();
	m_Storage.clear();
	m_Temps.clear();
}
