#pragma once

#include "Common.h"
#include <string>
#include "TypeInfo.h"
#include <vector>
#include "ASTExpression.h"
#include "Template.h"
#include <unordered_map>

enum class AccessModifier : uint32
{
	PUBLIC, PRIVATE
};

struct FunctionParameter
{
	TypeInfo type;
	ID variableID;
	std::string templateTypeName;
};

struct Function
{
	std::string name;
	bool isStatic;
	bool isVirtual;
	AccessModifier accessModifier;
	uint32 pc;
	TypeInfo returnInfo;
	std::vector<FunctionParameter> parameters;
	std::vector<ASTExpression*> body;
	uint32 id;
	ID scope;
	std::string returnTemplateTypeName;

	std::string GenerateSignature() const;

	static std::string GenerateSignatureFromArgs(Program* program, const std::string& name, const std::vector<ASTExpression*>& args);
};

struct ClassField
{
	TypeInfo type;
	uint32 arrayLength;
	uint64 size;
	uint64 offset;
	std::string name;
	ASTExpression* initializeExpr;
	std::string templateTypeName;
	std::string arrayDimensionTemplateSize;
};

struct ClassStaticData
{
	void* data;
};

class Program;
class Class
{
public:
	Class(const std::string& name, ID scopeID) : 
		m_NextFunctionID(0),
		m_Name(name),
		m_ScopeID(scopeID),
		m_Destructor(nullptr),
		m_IsTemplateInstance(false)
	{ }

	inline void SetID(ID id) { m_ID = id; }
	inline const std::string& GetName() const { return m_Name; }
	inline ID GetScopeID() const { return m_ScopeID; }
	inline uint64 GetSize() const { return m_Size; }
	inline void SetSize(uint64 size) { m_Size = size; }

	void AddFunction(Function* function);
	void AddStaticField(Program* program, uint16 type, uint8 pointerLevel, uint32 arrayLength, uint64 size, const std::string& name, ASTExpression* initializeExpr = nullptr);
	void AddMemberField(uint16 type, uint8 pointerLevel, uint32 arrayLength, uint64 size, uint64 offset, const std::string& name, const std::string& templateTypeName);

	ClassField GetMemberFieldByOffset(uint64 offset) const;

	inline Function* GetFunction(ID functionID) { return m_FunctionMap[functionID]; }
	inline std::vector<FunctionParameter>& GetFunctionParameters(ID functionID) { return m_FunctionMap[functionID]->parameters; }
	inline Function* GetDestructor() { return m_Destructor; }
	inline const std::vector<ClassField>& GetMemberFields() const { return m_MemberFields; }

	uint32 GetFunctionID(const std::string& name, const std::vector<ASTExpression*>& args);
	Function* FindFunctionBySignature(const std::string& signature);

	void EmitCode(Program* program);

	void InitStatics(Program* program);

	uint64 CalculateOffset(const std::vector<std::string>& members, TypeInfo* memberTypeInfo, bool* isTemplated, uint32 currentMember = 0, uint64 currentOffset = 0);

	inline bool IsTemplateClass() const { return m_TemplateDefinition.HasTemplate(); }
	inline bool IsTemplateInstance() const { return m_IsTemplateInstance; }
	inline void SetTemplateDefinition(const TemplateDefinition& definition) { m_TemplateDefinition = definition; }
	inline const TemplateDefinition& GetTemplateDefinition() const { return m_TemplateDefinition; }

	ID InstantiateTemplate(Program* program, const TemplateInstantiation& instantiation, ID generatedID = INVALID_ID);
	ID AddInstantiationCommand(TemplateInstantiationCommand* command);
	int32 InstantiateTemplateGetIndex(Program* program, const std::string& templateTypeName);
private:
	void ExecuteInstantiationCommands(Program* program, const TemplateInstantiation& instantiation);
	uint32 ExecuteInstantiationCommand(Program* program, TemplateInstantiationCommand* command, const TemplateInstantiation& instantiation, ID generatedID);
	Function* InstantiateTemplateInjectFunction(Program* program, Function* templatedFunction, ID templatedID, const std::string& templatedName, const TemplateInstantiation& instantiation, Class* templatedClass);
	std::string GenerateTemplateClassName(Program* program, const std::string& className, const TemplateInstantiation& instantiation);
private:
	std::string m_Name;
	ID m_ID;
	ID m_ScopeID;
	uint64 m_Size;

	std::unordered_map<std::string, std::vector<Function*>> m_Functions;
	std::unordered_map<std::string, uint32> m_FunctionDefinitionMap;
	std::vector<Function*> m_FunctionMap; //FunctionID is the index
	uint32 m_NextFunctionID;

	Function* m_Destructor;

	std::vector<ClassField> m_StaticFields;
	std::vector<ClassField> m_MemberFields;

	TemplateDefinition m_TemplateDefinition;
	bool m_IsTemplateInstance;
	std::vector<std::pair<TemplateInstantiationCommand*, ID>> m_InstantiationCommands;
};