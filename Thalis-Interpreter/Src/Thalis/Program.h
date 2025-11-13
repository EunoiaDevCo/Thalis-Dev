#pragma once

#include "Memory/BumpAllocator.h"
#include "Memory/HeapAllocator.h"
#include "Value.h"
#include "ID.h"
#include "FunctionArg.h"
#include <vector>
#include <unordered_map>

class Class;
class Scope;
struct ASTExpression;
struct Function;

enum class OpCode : uint16
{
	JUMP, JUMP_IF_FALSE,
	PUSH_UINT8, PUSH_UINT16, PUSH_UINT32, PUSH_UINT64,
	PUSH_INT8, PUSH_INT16, PUSH_INT32, PUSH_INT64,
	PUSH_REAL32, PUSH_REAL64,
	PUSH_CHAR, PUSH_BOOL, PUSH_STRING,
	PUSH_VARIABLE, PUSH_MEMBER, PUSH_THIS,

	PUSH_SCOPE, POP_SCOPE, PUSH_LOOP, POP_LOOP,
	BREAK, CONTINUE,

	DECLARE_UINT8, DECLARE_UINT16, DECLARE_UINT32, DECLARE_UINT64,
	DECLARE_INT8, DECLARE_INT16, DECLARE_INT32, DECLARE_INT64,
	DECLARE_REAL32, DECLARE_REAL64, DECLARE_CHAR, DECLARE_BOOL, 
	DECLARE_STRING, DECLARE_POINTER, DECLARE_NULLPTR, DECLARE_ARRAY,
	DECLARE_OBJECT_WITH_CONSTRUCTOR, DECLARE_OBJECT_WITH_ASSIGN,

	ADDRESS_OF, DEREFERENCE,

	VARIABLE_SET, MEMBER_SET,
	INDEX, INDEX_ASSIGN, DIRECT_MEMBER_ACCESS, DIRECT_MEMBER_ASSIGN,

	NEW, NEW_ARRAY,
	DELETE, DELETE_ARRAY,

	POP, DUP, SWAP,

	ADD, SUBTRACT, MULTIPLY, DIVIDE, MOD,
	LESS, GREATER, LESS_EQUAL, GREATER_EQUAL, EQUALS, NOT_EQUALS,
	UNARY_UPDATE, NOT, NEGATE, LOGICAL_OR, LOGICAL_AND,

	POINTER_CAST,

	MODULE_CONSTANT, RETURN,
	MODULE_FUNCTION_CALL, STATIC_FUNCTION_CALL, MEMBER_FUNCTION_CALL,
	CONSTRUCTOR_CALL, VIRTUAL_FUNCTION_CALL,

	STRLEN,

	END

};

struct CallFrame
{
	uint32 returnPC;      // Where to continue after function returns
	uint32 basePointer;   // Stack index where this function's locals begin
	uint64 marker;		  // Marker to indicate where we are at in the stack allocator
	ID functionScope;	  // Scope of the function to clear
	bool usesReturnValue;
	bool popThisStack;
	uint32 scopeStackSize;
};

struct LoopFrame
{
	uint32 startPC;
	uint32 endPC;
};

struct PendingCopyConstructor
{
	PendingCopyConstructor(const Value& dst, const Value& src, Function* function) :
		dst(dst), src(src), function(function) { }

	PendingCopyConstructor() :
		dst(Value::MakeNULL()), src(Value::MakeNULL()), function(nullptr) { }

	Value dst;
	Value src;
	Function* function;
};

class Program
{
public:
	Program();
	void ExecuteProgram(uint32 pc);

	void AddJumpCommand(uint32 pc);
	void AddPushConstantUInt8Command(uint8 value);
	void AddPushConstantUInt16Command(uint16 value);
	void AddPushConstantUInt32Command(uint32 value);
	void AddPushConstantUInt64Command(uint64 value);
	void AddPushConstantInt8Command(int8 value);
	void AddPushConstantInt16Command(int16 value);
	void AddPushConstantInt32Command(int32 value);
	void AddPushConstantInt64Command(int64 value);
	void AddPushConstantReal32Command(real32 value);
	void AddPushConstantReal64Command(real64 value);
	void AddPushConstantCharCommand(char value);
	void AddPushConstantBoolCommand(bool value);
	void AddPushConstantStringCommand(const std::string& value, ID scopeID);
	void AddPushVariableCommand(ID scope, ID variableID);
	void AddPushMemberCommand(ID scope, ID variableID, uint64 offset, uint16 memberType, uint8 memberPointerLevel, bool indexArray);
	void AddEndCommand();
	void AddReturnCommand(bool returnsValue, bool clone);
	void AddDeclarePrimitiveCommand(ValueType type, ID scope, ID variableID);
	void AddDeclarePointerCommand(uint16 type, uint8 pointerLevel, ID scope, ID variableID);
	void AddDeclareNullPtrCommand(uint16 type, uint8 pointerLevel, ID scope, ID variableID);
	void AddDeclareArrayCommand(uint16 type, uint8 pointerLevel, uint32 length, uint32 initializerCount, ID scope, ID variableID);
	void AddDeclareObjectWithConstructorCommand(uint16 type, uint16 functionID, ID scope, ID variableID);
	void AddDeclareObjectWithAssignCommand(uint16 type, ID scope, ID variableID, uint16 assignFunctionID);
	void AddVariableSetCommand(ID scope, ID variableID, uint16 assignFunctionID, uint16 variableType);
	void AddMemberSetCommand(ID scope, ID variableID, uint64 offset, uint16 type, uint64 size, uint8 memberPointerLevel, bool indexArray, uint16 assignFunctionID);
	void AddNewArrayCommand(uint16 type, uint8 pointerLevel);

	void AddModuleFunctionCallCommand(ID moduleID, uint16 functionID, uint8 argCount, bool usesReturnValue);
	void AddModuleConstantCommand(ID moduleID, uint16 constant);
	void AddStaticFunctionCallCommand(ID classID, uint16 functionID, bool usesReturnValue);
	void AddMemberFunctionCallCommand(ID classID, uint16 functionID, bool usesReturnValue);
	void AddVirtualFunctionCallCommand(uint16 functionID, bool usesReturnValue);

	void AddDirectMemberAccessCommand(uint64 offset, uint16 memberType, uint8 memberPointerLevel);
	void AddDirectMemberAssignCommand(uint64 offset, uint16 memberType, uint8 memberPointerLevel, uint64 memberTypeSize, uint16 assignFunctionID);

	void AddPushScopeCommand(ID scope);
	void AddPopScopeCommand();
	void AddUnaryUpdateCommand(uint8 type, bool pushResultToStack);
	void AddConstructorCallCommand(uint16 type, uint16 functionID);
	uint32 AddPushLoopCommand();
	void AddPopLoopCommand();
	void AddNewCommand(uint16 type, uint16 functionID);
	void AddIndexAssignCommand(uint16 assignFunctionID);

	void AddAddCommand(uint16 functionID);
	void AddSubCommand(uint16 functionID);
	void AddMulCommand(uint16 functionID);
	void AddDivCommand(uint16 functionID);
	void AddModCommand(uint16 functionID);

	void AddPointerCastCommand(uint16 castToType);

	uint32 GetCodeSize() const;
	uint32 GetStackSize() const;

	void AddModule(const std::string& name, ID id);
	ID GetModuleID(const std::string& name) const;

	ID AddClass(const std::string& name, Class* cls, ID generatedID = INVALID_ID);
	void AddClass2(Class* cls, ID generatedID);
	ID GetClassID(const std::string& name) const;
	Class* GetClass(ID id);
	uint16 GetTypeID(const std::string& name) const;

	void WriteUInt64(uint64 value);
	void WriteUInt32(uint32 value);
	void WriteUInt16(uint16 value);
	void WriteUInt8(uint8 value);
	void WriteInt8(int8 value);
	void WriteInt16(int16 value);
	void WriteInt32(int32 value);
	void WriteInt64(int64 value);
	void WriteReal32(real32 value);
	void WriteReal64(real64 value);
	void WriteOPCode(OpCode code);
	void WriteString(const std::string& str);

	void PatchUInt32(uint32 pos, uint32 value);
	void PatchPushLoopCommand(uint32 pos, uint32 start, uint32 end);

	HeapAllocator* GetHeapAllocator() const;
	BumpAllocator* GetStackAllocator() const;

	std::string GetTypeName(uint16 type);
	inline void SetClassWithMainFunction(ID id) { m_ClassWithMainFunction = id; }
	inline ID GetClassIDWithMainFunction() const { return m_ClassWithMainFunction; }

	uint64 GetTypeSize(uint16 type);

	ID CreateScope(ID parent = INVALID_ID);
	Scope* GetScope(ID scope);

	void AddCreatedExpression(ASTExpression* expression);
	bool Resolve();
	void EmitCode();
	void BuildVTables();

	void InitStatics();

	Class* GetClassByName(const std::string& name);

	inline void AddPendingDestructor(const Value& value) { m_PendingDestructors.push_back(value); }
	void PushCallStack(const CallFrame& frame);
private:
	void AddDestructorRecursive(const Value& value, uint32 offset = 0);
	void CleanUpForExecution();
	void ExecuteOpCode(OpCode opcode);
	void ExecutePendingDestructors(uint32 offset = 0);
	void ExecuteModuleFunctionCall(ID moduleID, uint16 function, bool usesReturnValue);
	void ExecuteModuleConstant(ID moduleID, uint16 constant);
	void AddFunctionParametersToScope(Scope* scope, Function* function);
	void AddCopyConstructorRecursive(const Value& dst, const Value& src, uint64 offset = 0);
	void ExecutePendingCopyConstructors(uint32 offset = 0);
	void ExecuteAssignFunction(const Value& thisValue, const Value& assignValue, Function* assignFunction);
	void ExecuteArithmaticFunction(const Value& lhs, const Value& rhs, Function* function);

	uint64 ReadUInt64();
	uint32 ReadUInt32();
	uint16 ReadUInt16();
	uint8 ReadUInt8();
	int8 ReadInt8();
	int16 ReadInt16();
	int32 ReadInt32();
	int64 ReadInt64();
	real32 ReadReal32();
	real64 ReadReal64();
	OpCode ReadOPCode();
	std::string ReadString();
public:
	static Program* GetCompiledProgram();
private:
	std::vector<uint8> m_Code;
	uint32 m_ProgramCounter;

	BumpAllocator* m_StackAllocator;
	HeapAllocator* m_HeapAllocator;
	BumpAllocator* m_ReturnAllocator;

	std::vector<Value> m_Stack;
	std::vector<CallFrame> m_CallStack;

	std::vector<FunctionArg> m_ArgStorage;
	std::unordered_map<std::string, ID> m_ModuleNameMap;
	std::unordered_map<std::string, ID> m_ClassNameMap;
	std::unordered_map<ID, Class*> m_Classes;

	std::vector<Scope*> m_Scopes;

	std::vector<ASTExpression*> m_CreatedExpressions;
	std::vector<uint32> m_ArrayDimensions;

	std::vector<void*> m_StringLiterals;

	std::vector<Value> m_ThisStack;
	std::vector<std::pair<ID, uint64>> m_ScopeStack;
	std::vector<LoopFrame> m_LoopStack;

	std::vector<Value> m_PendingDestructors;
	std::vector<PendingCopyConstructor> m_PendingCopyConstructors;
	void* m_PendingDelete;

	ID m_ClassWithMainFunction;
};
