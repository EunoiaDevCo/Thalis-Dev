#pragma once

#include <deque>
#include <unordered_map>
#include <unordered_set>
#include "Value.h"
#include "ID.h"
#include "TypeInfo.h"

struct DeclaredVariable
{
	TypeInfo type;
	uint16 derivedType;
	std::string templateTypeName;
};

class Program;
class Scope
{
public:
	Scope(Scope* parent = nullptr);

	void AddVariable(ID variableID, const Value& value);
	void AddVariableTest(ID variableID, const Value& value);
	Value* GetVariable(ID variableID);

	ID GetVariableID(const std::string& name, bool makeID = true);
	void DeclareVariable(ID variableID, const TypeInfo& typeInfo, const std::string& templatedTypeName = "", uint16 derivedType = INVALID_ID);
	inline const std::unordered_map<ID, DeclaredVariable>& GetDeclaredVariables() const { return m_VariableTypes; }

	TypeInfo GetVariableTypeInfo(ID variableID);
	TypeInfo GetVariableDerivedTypeInfo(ID variableID);
	void Clear(Program* program);

private:
	Scope* m_Parent;
	std::vector<Value> m_Storage;
	std::unordered_map<ID, uint32> m_VariableMap;
	std::unordered_map<std::string, ID>	m_VariableNameMap;
	std::unordered_map<ID, DeclaredVariable> m_VariableTypes;
};