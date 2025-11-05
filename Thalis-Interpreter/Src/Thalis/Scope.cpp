#include "Scope.h"
#include "Program.h"

Scope::Scope(Scope* parent) :
	m_Parent(parent)
{
	m_Storage.reserve(64);
}

void Scope::AddVariable(ID variableID, const Value& value)
{
	uint32 index = m_Storage.size();
	m_Storage.push_back(value);
	m_VariableMap[variableID] = index;
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

void Scope::DeclareVariable(ID variableID, const TypeInfo& typeInfo)
{
	m_VariableTypes[variableID] = typeInfo;
}

TypeInfo Scope::GetVariableTypeInfo(ID variableID)
{
	const auto&& it = m_VariableTypes.find(variableID);
	if (it == m_VariableTypes.end())
	{
		if (m_Parent) return m_Parent->GetVariableTypeInfo(variableID);
	}

	if (it == m_VariableTypes.end()) return TypeInfo(INVALID_ID, 0);
	return it->second;
}

void Scope::Clear(Program* program)
{
	HeapAllocator* halloc = program->GetHeapAllocator();
	for (Value& value : m_Storage)
	{
		
	}

	m_VariableMap.clear();
	m_Storage.clear();
	m_Temps.clear();
}
