#pragma once

#include <deque>
#include <unordered_map>
#include <unordered_set>
#include "Value.h"
#include "ID.h"
#include "TypeInfo.h"

class Program;
class Scope
{
public:
	Scope(Scope* parent = nullptr);

	void AddVariable(ID variableID, const Value& value);
	Value* GetVariable(ID variableID);

	ID GetVariableID(const std::string& name, bool makeID = true);
	void DeclareVariable(ID variableID, const TypeInfo& typeInfo);
	inline const std::unordered_map<ID, TypeInfo>& GetDeclaredVariables() const { return m_VariableTypes; }

	TypeInfo GetVariableTypeInfo(ID variableID);
	void Clear(Program* program);

	inline void AddTemp(const Value& value) { m_Temps.push_back(value); }
private:
	Scope* m_Parent;
	std::vector<Value> m_Storage;
	std::unordered_map<ID, uint32> m_VariableMap;
	std::unordered_map<std::string, ID>	m_VariableNameMap;
	std::unordered_map<ID, TypeInfo> m_VariableTypes;
	std::vector<Value> m_Temps;

	std::unordered_set<void*> m_AlreadyFreedData;
};