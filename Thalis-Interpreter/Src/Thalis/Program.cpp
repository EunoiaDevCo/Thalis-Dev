#include "Program.h"
#include "Modules/IOModule.h"
#include "Modules/ModuleID.h"
#include "Modules/MathModule.h"
#include "Modules/WindowModule.h"
#include "Modules/GLModule.h"
#include "Class.h"
#include "Scope.h"
#include "Memory/Memory.h"
#include "ASTExpression.h"

static Program* g_CompiledProgram;

Program::Program() :
	m_ProgramCounter(0),
	m_PendingDelete(nullptr)
{
	m_Stack.reserve(2048);
	m_HeapAllocator = new HeapAllocator();
	m_StackAllocator = new BumpAllocator(Memory::KBToBytes(64));
	m_ReturnAllocator = new BumpAllocator(Memory::KBToBytes(16));
	g_CompiledProgram = this;
}

void Program::ExecuteProgram(uint32 pc)
{
	uint32 initStaticsPC = GetCodeSize();
	InitStatics();
	CleanUpForExecution();
	m_StackAllocator->Free();
	AddJumpCommand(pc);

	m_ProgramCounter = initStaticsPC;
	OpCode opcode = ReadOPCode();
	while (opcode != OpCode::END)
	{
		ExecuteOpCode(opcode);
		opcode = ReadOPCode();
	}
}

void Program::ExecutePendingDestructors(uint32 offset)
{
	for (uint32 i = offset; i < m_PendingDestructors.size(); i++)
	{
		const Value& object = m_PendingDestructors[i];
		Function* destructor = GetClass(object.type)->GetDestructor();
		if (!destructor)
			continue;

		m_ThisStack.push_back(object);

		CallFrame frame;
		frame.returnPC = m_ProgramCounter; // after this instruction
		frame.basePointer = (uint32)m_Stack.size(); // current stack top before call
		frame.marker = m_StackAllocator->GetMarker();
		frame.functionScope = destructor->scope;
		frame.usesReturnValue = false;
		frame.popThisStack = true;
		frame.scopeStackSize = m_ScopeStack.size();
		PushCallStack(frame);

		m_ProgramCounter = destructor->pc;

		while (m_ProgramCounter != frame.returnPC)
		{
			OpCode innerOpCode = ReadOPCode();
			if (innerOpCode == OpCode::END) break;
			ExecuteOpCode(innerOpCode);
		}
	}

	m_PendingDestructors.resize(offset);
	if (m_PendingDelete)
	{
		m_HeapAllocator->Free(m_PendingDelete);
		m_PendingDelete = nullptr;
	}
}

void Program::AddJumpCommand(uint32 pc)
{
	WriteOPCode(OpCode::JUMP);
	WriteUInt32(pc);
}

void Program::AddPushConstantUInt8Command(uint8 value)
{
	WriteOPCode(OpCode::PUSH_UINT8);
	WriteUInt8(value);
}

void Program::AddPushConstantUInt16Command(uint16 value)
{
	WriteOPCode(OpCode::PUSH_UINT16);
	WriteUInt16(value);
}

void Program::AddPushConstantUInt32Command(uint32 value)
{
	WriteOPCode(OpCode::PUSH_UINT32);
	WriteUInt32(value);
}

void Program::AddPushConstantUInt64Command(uint64 value)
{
	WriteOPCode(OpCode::PUSH_UINT64);
	WriteUInt64(value);
}

void Program::AddPushConstantInt8Command(int8 value)
{
	WriteOPCode(OpCode::PUSH_INT8);
	WriteInt8(value);
}

void Program::AddPushConstantInt16Command(int16 value)
{
	WriteOPCode(OpCode::PUSH_INT16);
	WriteInt16(value);
}

void Program::AddPushConstantInt32Command(int32 value)
{
	WriteOPCode(OpCode::PUSH_INT32);
	WriteInt32(value);
}

void Program::AddPushConstantInt64Command(int64 value)
{
	WriteOPCode(OpCode::PUSH_INT64);
	WriteInt64(value);
}

void Program::AddPushConstantReal32Command(real32 value)
{
	WriteOPCode(OpCode::PUSH_REAL32);
	WriteReal32(value);
}

void Program::AddPushConstantReal64Command(real64 value)
{
	WriteOPCode(OpCode::PUSH_REAL64);
	WriteReal64(value);
}

void Program::AddPushConstantCharCommand(char value)
{
	WriteOPCode(OpCode::PUSH_CHAR);
	WriteInt8(value);
}

void Program::AddPushConstantBoolCommand(bool value)
{
	WriteOPCode(OpCode::PUSH_BOOL);
	WriteUInt8(value);
}

void Program::AddPushConstantStringCommand(const std::string& value, ID scope)
{
	WriteOPCode(OpCode::PUSH_STRING);
	WriteUInt32(scope);
	WriteString(value);
}

void Program::AddPushVariableCommand(ID scope, ID variableID)
{
	WriteOPCode(OpCode::PUSH_VARIABLE);
	WriteUInt32(scope);
	WriteUInt32(variableID);
}

void Program::AddPushMemberCommand(ID scope, ID variableID, uint64 offset, uint16 memberType, uint8 memberPointerLevel, bool indexArray)
{
	WriteOPCode(OpCode::PUSH_MEMBER);
	WriteUInt32(scope);
	WriteUInt32(variableID);
	WriteUInt64(offset);
	WriteUInt16(memberType);
	WriteUInt8(memberPointerLevel);
	WriteUInt8(indexArray);
}

void Program::AddEndCommand()
{
	WriteOPCode(OpCode::END);
}

void Program::AddReturnCommand(bool returnsValue, bool clone)
{
	WriteOPCode(OpCode::RETURN);
	WriteUInt8(returnsValue);
	WriteUInt8(clone);
}

void Program::AddDeclarePrimitiveCommand(ValueType type, ID scope, ID variableID)
{
	switch (type)
	{
	case ValueType::UINT8: WriteOPCode(OpCode::DECLARE_UINT8); break;
	case ValueType::UINT16: WriteOPCode(OpCode::DECLARE_UINT16); break;
	case ValueType::UINT32: WriteOPCode(OpCode::DECLARE_UINT32); break;
	case ValueType::UINT64: WriteOPCode(OpCode::DECLARE_UINT64); break;
	case ValueType::INT8: WriteOPCode(OpCode::DECLARE_INT8); break;
	case ValueType::INT16: WriteOPCode(OpCode::DECLARE_INT16); break;
	case ValueType::INT32: WriteOPCode(OpCode::DECLARE_INT32); break;
	case ValueType::INT64: WriteOPCode(OpCode::DECLARE_INT64); break;
	case ValueType::REAL32: WriteOPCode(OpCode::DECLARE_REAL32); break;
	case ValueType::REAL64: WriteOPCode(OpCode::DECLARE_REAL64); break;
	case ValueType::CHAR: WriteOPCode(OpCode::DECLARE_CHAR); break;
	case ValueType::BOOL: WriteOPCode(OpCode::DECLARE_BOOL); break;
	case ValueType::STRING: WriteOPCode(OpCode::DECLARE_STRING); break;
	}

	WriteUInt32(scope);
	WriteUInt32(variableID);
}

void Program::AddDeclarePointerCommand(uint16 type, uint8 pointerLevel, ID scope, ID variableID)
{
	WriteOPCode(OpCode::DECLARE_POINTER);
	WriteUInt16(type);
	WriteUInt8(pointerLevel);
	WriteUInt32(scope);
	WriteUInt32(variableID);
}

void Program::AddDeclareNullPtrCommand(uint16 type, uint8 pointerLevel, ID scope, ID variableID)
{
	WriteOPCode(OpCode::DECLARE_NULLPTR);
	WriteUInt16(type);
	WriteUInt8(pointerLevel);
	WriteUInt32(scope);
	WriteUInt32(variableID);
}

void Program::AddDeclareArrayCommand(uint16 type, uint8 pointerLevel, uint32 length, uint32 initializerCount, ID scope, ID variableID)
{
	WriteOPCode(OpCode::DECLARE_ARRAY);
	WriteUInt16(type);
	WriteUInt8(pointerLevel);
	WriteUInt32(length);
	WriteUInt32(initializerCount);
	WriteUInt32(scope);
	WriteUInt32(variableID);
}

void Program::AddDeclareObjectWithConstructorCommand(uint16 type, uint16 functionID, ID scope, ID variableID)
{
	WriteOPCode(OpCode::DECLARE_OBJECT_WITH_CONSTRUCTOR);
	WriteUInt16(type);
	WriteUInt16(functionID);
	WriteUInt32(scope);
	WriteUInt32(variableID);
}

void Program::AddDeclareObjectWithAssignCommand(uint16 type, ID scope, ID variableID, uint16 assignFunctionID)
{
	WriteOPCode(OpCode::DECLARE_OBJECT_WITH_ASSIGN);
	WriteUInt16(type);
	WriteUInt32(scope);
	WriteUInt32(variableID);
	WriteUInt16(assignFunctionID);
}

void Program::AddVariableSetCommand(ID scope, ID variableID, uint16 assignFunctionID, uint16 variableType)
{
	WriteOPCode(OpCode::VARIABLE_SET);
	WriteUInt32(scope);
	WriteUInt32(variableID);
	WriteUInt16(assignFunctionID);
	if (assignFunctionID != INVALID_ID)
	{
		WriteUInt16(variableType);
	}
}

void Program::AddMemberSetCommand(ID scope, ID variableID, uint64 offset, uint16 type, uint64 size, uint8 memberPointerLevel, bool indexArray, uint16 assignFunctionID)
{
	WriteOPCode(OpCode::MEMBER_SET);
	WriteUInt32(scope);
	WriteUInt32(variableID);
	WriteUInt64(offset);
	WriteUInt16(type);
	WriteUInt64(size);
	WriteUInt8(memberPointerLevel);
	WriteUInt8(indexArray);
	WriteUInt16(assignFunctionID);
}

void Program::AddNewArrayCommand(uint16 type, uint8 pointerLevel)
{
	WriteOPCode(OpCode::NEW_ARRAY);
	WriteUInt16(type);
	WriteUInt8(pointerLevel);
}

void Program::AddModuleFunctionCallCommand(ID id, uint16 functionID, uint8 argCount, bool usesReturnValue)
{
	WriteOPCode(OpCode::MODULE_FUNCTION_CALL);
	WriteUInt32(id);
	WriteUInt16(functionID);
	WriteUInt8(argCount);
	WriteUInt8(usesReturnValue);
}

void Program::AddModuleConstantCommand(ID moduleID, uint16 constant)
{
	WriteOPCode(OpCode::MODULE_CONSTANT);
	WriteUInt32(moduleID);
	WriteUInt16(constant);
}

void Program::AddStaticFunctionCallCommand(ID classID, uint16 functionID, bool usesReturnValue)
{
	WriteOPCode(OpCode::STATIC_FUNCTION_CALL);
	WriteUInt32(classID);
	WriteUInt16(functionID);
	WriteUInt8(usesReturnValue);
}

void Program::AddMemberFunctionCallCommand(ID classID, uint16 functionID, bool usesReturnValue)
{
	WriteOPCode(OpCode::MEMBER_FUNCTION_CALL);
	WriteUInt32(classID);
	WriteUInt16(functionID);
	WriteUInt8(usesReturnValue);
}

void Program::AddVirtualFunctionCallCommand(uint16 functionID, bool usesReturnValue)
{
	WriteOPCode(OpCode::VIRTUAL_FUNCTION_CALL);
	WriteUInt16(functionID);
	WriteUInt8(usesReturnValue);
}

void Program::AddDirectMemberAccessCommand(uint64 offset, uint16 memberType, uint8 memberPointerLevel)
{
	WriteOPCode(OpCode::DIRECT_MEMBER_ACCESS);
	WriteUInt64(offset);
	WriteUInt16(memberType);
	WriteUInt8(memberPointerLevel);
}

void Program::AddDirectMemberAssignCommand(uint64 offset, uint16 memberType, uint8 memberPointerLevel, uint64 memberTypeSize, uint16 assignFunctionID)
{
	WriteOPCode(OpCode::DIRECT_MEMBER_ASSIGN);
	WriteUInt64(offset);
	WriteUInt16(memberType);
	WriteUInt8(memberPointerLevel);
	WriteUInt64(memberTypeSize);
	WriteUInt16(assignFunctionID);
}

void Program::AddPushScopeCommand(ID scope)
{
	WriteOPCode(OpCode::PUSH_SCOPE);
	WriteUInt32(scope);
}

void Program::AddPopScopeCommand()
{
	WriteOPCode(OpCode::POP_SCOPE);
}

void Program::AddUnaryUpdateCommand(uint8 type, bool pushResultToStack)
{
	WriteOPCode(OpCode::UNARY_UPDATE);
	WriteUInt8(type);
	WriteUInt8(pushResultToStack);
}

void Program::AddConstructorCallCommand(uint16 type, uint16 functionID)
{
	WriteOPCode(OpCode::CONSTRUCTOR_CALL);
	WriteUInt16(type);
	WriteUInt16(functionID);
}

uint32 Program::AddPushLoopCommand()
{
	WriteOPCode(OpCode::PUSH_LOOP);
	uint32 pos = GetCodeSize();
	WriteUInt32(0);//Patch later
	WriteUInt32(0);//Patch later
	return pos;
}

void Program::AddPopLoopCommand()
{
	WriteOPCode(OpCode::POP_LOOP);
}

void Program::AddNewCommand(uint16 type, uint16 functionID)
{
	WriteOPCode(OpCode::NEW);
	WriteUInt16(type);
	WriteUInt16(functionID);
}

void Program::AddIndexAssignCommand(uint16 assignFunctionID)
{
	WriteOPCode(OpCode::INDEX_ASSIGN);
	WriteUInt16(assignFunctionID);
}

void Program::AddAddCommand(uint16 functionID)
{
	WriteOPCode(OpCode::ADD);
	WriteUInt16(functionID);
}

void Program::AddSubCommand(uint16 functionID)
{
	WriteOPCode(OpCode::SUBTRACT);
	WriteUInt16(functionID);
}

void Program::AddMulCommand(uint16 functionID)
{
	WriteOPCode(OpCode::MULTIPLY);
	WriteUInt16(functionID);
}

void Program::AddDivCommand(uint16 functionID)
{
	WriteOPCode(OpCode::DIVIDE);
	WriteUInt16(functionID);
}

void Program::AddModCommand(uint16 functionID)
{
	WriteOPCode(OpCode::MOD);
	WriteUInt16(functionID);
}

void Program::AddPointerCastCommand(uint16 castToType)
{
	WriteOPCode(OpCode::POINTER_CAST);
	WriteUInt16(castToType);
}

uint32 Program::GetCodeSize() const
{
	return m_Code.size();
}

uint32 Program::GetStackSize() const
{
	return m_Stack.size();
}

uint64 Program::GetTypeSize(uint16 type)
{
	switch ((ValueType)type)
	{
	case ValueType::UINT8:	return sizeof(uint8);
	case ValueType::UINT16: return sizeof(uint16);
	case ValueType::UINT32: return sizeof(uint32);
	case ValueType::UINT64: return sizeof(uint64);
	case ValueType::INT8:	return sizeof(int8);
	case ValueType::INT16:	return sizeof(int16);
	case ValueType::INT32:	return sizeof(int32);
	case ValueType::INT64:	return sizeof(int64);
	case ValueType::REAL32: return sizeof(real32);
	case ValueType::REAL64: return sizeof(real64);
	case ValueType::CHAR:	return sizeof(char);
	case ValueType::BOOL:	return sizeof(bool);
	case ValueType::STRING: return sizeof(char*);
	case ValueType::VOID_T:	return 0;
	case ValueType::TEMPLATE_TYPE: return 0;
	}

	return GetClass(type)->GetSize();
}

ID Program::CreateScope(ID parent)
{
	Scope* parentScope = parent == INVALID_ID ? nullptr : m_Scopes[parent];
	Scope* scope = new Scope(parentScope);
	ID scopeID = m_Scopes.size();
	m_Scopes.push_back(scope);
	return scopeID;
}

Scope* Program::GetScope(ID scope)
{
	return m_Scopes[scope];
}

void Program::AddCreatedExpression(ASTExpression* expression)
{
	m_CreatedExpressions.push_back(expression);
}

bool Program::Resolve()
{
	for (uint32 i = 0; i < m_CreatedExpressions.size(); i++)
		if (!m_CreatedExpressions[i]->Resolve(this))
			return false;

	return true;
}

void Program::EmitCode()
{
	for (auto&& it : m_Classes)
	{
		it.second->EmitCode(this);
	}
}

void Program::BuildVTables()
{
	for (auto&& it : m_Classes)
	{
		it.second->BuildVTable();
	}
}

void Program::InitStatics()
{
	for (auto&& it : m_Classes)
	{
		it.second->InitStatics(this);
	}
}

Class* Program::GetClassByName(const std::string& name)
{
	for (auto&& it : m_Classes)
	{
		if (it.second->GetName() == name)
			return it.second;
	}

	return nullptr;
}

void Program::CleanUpForExecution()
{
	for (uint32 i = 0; i < m_CreatedExpressions.size(); i++)
	{
		m_CreatedExpressions[i]->CleanUp(this);
		delete m_CreatedExpressions[i];
	}

	m_CreatedExpressions.reserve(0);
}

void Program::PushCallStack(const CallFrame& frame)
{
	m_CallStack.push_back(frame);
}

void Program::AddDestructorRecursive(const Value& value, uint32 offset)
{
	if (value.IsPrimitive() || value.IsPointer())
		return;

	Class* cls = GetClass(value.type);
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
		member.data = (uint8*)value.data + field.offset + offset;

		AddDestructorRecursive(member, offset + field.offset);
	}

	AddPendingDestructor(value);
}

void Program::ExecuteOpCode(OpCode opcode)
{
	switch (opcode)
	{
	case OpCode::JUMP: {
		m_ProgramCounter = ReadUInt32();
	} break; 
	case OpCode::JUMP_IF_FALSE: {
		uint32 target = ReadUInt32();
		Value condition = m_Stack.back();
		m_Stack.pop_back();
		if (!condition.GetBool())
			m_ProgramCounter = target;
	} break;
	case OpCode::PUSH_SCOPE: {
		ID scope = ReadUInt32();
		uint64 marker = m_StackAllocator->GetMarker();
		m_ScopeStack.push_back(std::make_pair(scope, marker));
	} break;
	case OpCode::POP_SCOPE: {
		const std::pair<ID, uint64>& scope = m_ScopeStack.back();
		m_ScopeStack.pop_back();
		uint32 count = m_PendingDestructors.size();
		if(scope.first != INVALID_ID)
			GetScope(scope.first)->Clear(this);
		ExecutePendingDestructors(count);
		m_StackAllocator->FreeToMarker(scope.second);
	} break;
	case OpCode::PUSH_LOOP: {
		LoopFrame loop;
		loop.startPC = ReadUInt32();
		loop.endPC = ReadUInt32();
		m_LoopStack.push_back(loop);
	} break;
	case OpCode::POP_LOOP: {
		m_LoopStack.pop_back();
	} break;
	case OpCode::BREAK: {
		const LoopFrame& loop = m_LoopStack.back();//gets popped elsewhere(next command will be POP_LOOP)
		const std::pair<ID, uint64>& scope = m_ScopeStack.back();
		m_ScopeStack.pop_back();
		uint32 count = m_PendingDestructors.size();
		if (scope.first != INVALID_ID)
			GetScope(scope.first)->Clear(this);

		ExecutePendingDestructors(count);
		m_StackAllocator->FreeToMarker(scope.second);
		m_ProgramCounter = loop.endPC;
	} break;
	case OpCode::CONTINUE: {
		const LoopFrame& loop = m_LoopStack.back();
		const std::pair<ID, uint64>& scope = m_ScopeStack.back();
		m_ScopeStack.pop_back();
		uint32 count = m_PendingDestructors.size();
		if (scope.first != INVALID_ID)
			GetScope(scope.first)->Clear(this);

		ExecutePendingDestructors(count);
		m_StackAllocator->FreeToMarker(scope.second);
		m_ProgramCounter = loop.startPC;
	} break;
	case OpCode::PUSH_UINT8: {
		m_Stack.push_back(Value::MakeUInt8(ReadUInt8(), m_StackAllocator));
	} break;
	case OpCode::PUSH_UINT16: {
		m_Stack.push_back(Value::MakeUInt16(ReadUInt16(), m_StackAllocator));
	} break;
	case OpCode::PUSH_UINT32: {
		m_Stack.push_back(Value::MakeUInt32(ReadUInt32(), m_StackAllocator));
	} break;
	case OpCode::PUSH_UINT64: {
		m_Stack.push_back(Value::MakeUInt64(ReadUInt64(), m_StackAllocator));
	} break;
	case OpCode::PUSH_INT8: {
		m_Stack.push_back(Value::MakeInt8(ReadInt8(), m_StackAllocator));
	} break;
	case OpCode::PUSH_INT16: {
		m_Stack.push_back(Value::MakeInt16(ReadInt16(), m_StackAllocator));
	} break;
	case OpCode::PUSH_INT32: {
		m_Stack.push_back(Value::MakeInt32(ReadInt32(), m_StackAllocator));
	} break;
	case OpCode::PUSH_INT64: {
		m_Stack.push_back(Value::MakeInt64(ReadInt64(), m_StackAllocator));
	} break;
	case OpCode::PUSH_REAL32: {
		m_Stack.push_back(Value::MakeReal32(ReadReal32(), m_StackAllocator));
	} break;
	case OpCode::PUSH_REAL64: {
		m_Stack.push_back(Value::MakeReal64(ReadReal64(), m_StackAllocator));
	} break;
	case OpCode::PUSH_CHAR: {
		m_Stack.push_back(Value::MakeChar(ReadInt8(), m_StackAllocator));
	} break;
	case OpCode::PUSH_BOOL: {
		m_Stack.push_back(Value::MakeBool(ReadUInt8(), m_StackAllocator));
	} break;
	case OpCode::PUSH_STRING: {
		ID scopeID = ReadUInt32();
		Value string = Value::MakeString(ReadString(), m_StackAllocator);
		m_Stack.push_back(string);
		//m_StringLiterals.push_back(string.data);
		//GetScope(scopeID)->AddTemp(m_Stack.back());
	} break;
	case OpCode::PUSH_VARIABLE: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		Scope* scope = GetScope(scopeID);
		m_Stack.push_back(*GetScope(scopeID)->GetVariable(variableID));
	} break;
	case OpCode::PUSH_MEMBER: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		uint64 offset = ReadUInt64();
		uint16 memberType = ReadUInt16();
		uint8 memberPointerLevel = ReadUInt8();
		bool indexArray = ReadUInt8();
		uint32 oo = offset;
		uint32 i = 0;
		uint64 typeSize = 0;
		if (indexArray)
		{
			uint32 index = m_Stack.back().GetUInt32();
			i = index;
			m_Stack.pop_back();
			typeSize = GetTypeSize(memberType);
			offset = typeSize * index + offset;
		}

		Value* variable = GetScope(scopeID)->GetVariable(variableID);
		Value member;
		member.type = memberType;
		member.pointerLevel = memberPointerLevel;

		void* memberAddress = (uint8*)variable->data + offset;

		if ((ValueType)memberType == ValueType::STRING || (memberPointerLevel > 0 && !indexArray))
		{
			// Field stores a pointer to the string data
			member.data = *(void**)memberAddress;
		}
		else if (indexArray)
		{
			void* v = *(void**)((uint8*)variable->data + oo);
			member.data = (uint8*)v + i * typeSize;

			char c = *(char*)member.data;
			uint32 bp = 0;
		}
		else
		{
			// Field is direct value
			member.data = memberAddress;
		}

		m_Stack.push_back(member);
	} break;
	case  OpCode::PUSH_THIS: {
		m_Stack.push_back(m_ThisStack.back());
	} break;
	case OpCode::ADD: {
		uint16 functionID = ReadUInt16();
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		if (functionID == INVALID_ID)
		{
			Value val = lhs.Add(rhs, m_StackAllocator);
			m_Stack.push_back(val);
		}
		else
		{
			Function* function = GetClass(lhs.type)->GetFunction(functionID);
			ExecuteArithmaticFunction(lhs, rhs, function);
		}
	} break;
	case OpCode::SUBTRACT: {
		uint16 functionID = ReadUInt16();
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		if (functionID == INVALID_ID)
		{
			Value val = lhs.Sub(rhs, m_StackAllocator);
			m_Stack.push_back(val);
		}
		else
		{
			Function* function = GetClass(lhs.type)->GetFunction(functionID);
			ExecuteArithmaticFunction(lhs, rhs, function);
		}
	} break;
	case OpCode::MULTIPLY: {
		uint16 functionID = ReadUInt16();
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		if (functionID == INVALID_ID)
		{
			Value val = lhs.Mul(rhs, m_StackAllocator);
			m_Stack.push_back(val);
		}
		else
		{
			Function* function = GetClass(lhs.type)->GetFunction(functionID);
			ExecuteArithmaticFunction(lhs, rhs, function);
		}
	} break;
	case OpCode::DIVIDE: {
		uint16 functionID = ReadUInt16();
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		if (functionID == INVALID_ID)
		{
			Value val = lhs.Div(rhs, m_StackAllocator);
			m_Stack.push_back(val);
		}
		else
		{
			Function* function = GetClass(lhs.type)->GetFunction(functionID);
			ExecuteArithmaticFunction(lhs, rhs, function);
		}
	} break;
	case OpCode::MOD: {
		uint16 functionID = ReadUInt16();
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		if (functionID == INVALID_ID)
		{
			Value val = lhs.Mod(rhs, m_StackAllocator);
			m_Stack.push_back(val);
		}
		else
		{
			Function* function = GetClass(lhs.type)->GetFunction(functionID);
			ExecuteArithmaticFunction(lhs, rhs, function);
		}
	} break;
	case OpCode::LESS: {
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		Value result = lhs.LessThan(rhs, m_StackAllocator);
		m_Stack.push_back(result);
	} break;
	case OpCode::GREATER: {
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		Value result = lhs.GreaterThan(rhs, m_StackAllocator);
		m_Stack.push_back(result);
	} break;
	case OpCode::LESS_EQUAL: {
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		Value result = lhs.LessThanOrEqual(rhs, m_StackAllocator);
		m_Stack.push_back(result);
	} break;
	case OpCode::GREATER_EQUAL: {
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		Value result = lhs.GreaterThanOrEqual(rhs, m_StackAllocator);
		m_Stack.push_back(result);
	} break;
	case OpCode::EQUALS: {
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		Value result = lhs.Equals(rhs, m_StackAllocator);
		m_Stack.push_back(result);
	} break;
	case OpCode::NOT_EQUALS: {
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		Value result = lhs.NotEquals(rhs, m_StackAllocator);
		m_Stack.push_back(result);
	} break;
	case OpCode::LOGICAL_AND: {
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		Value result = lhs.LogicalAnd(rhs, m_StackAllocator);
		m_Stack.push_back(result);
	} break;
	case OpCode::LOGICAL_OR: {
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		Value result = lhs.LogicalOr(rhs, m_StackAllocator);
		m_Stack.push_back(result);
	} break;
	case OpCode::POP: {
		m_Stack.pop_back();
	} break;
	case OpCode::DUP: {
		Value dup = m_Stack.back().Clone(this, m_StackAllocator);
		m_Stack.push_back(dup);
	} break;
	case OpCode::SWAP: {
		std::swap(m_Stack[m_Stack.size() - 1], m_Stack[m_Stack.size() - 2]);
	} break;
	case OpCode::MODULE_FUNCTION_CALL: {
		ID moduleID = ReadUInt32();
		uint16 functionID = ReadUInt16();
		uint8 argCount = ReadUInt8();
		bool usesReturnValue = ReadUInt8();
		m_ArgStorage.clear();
		for (uint32 i = 0; i < argCount; i++)
		{
			m_ArgStorage.push_back(FunctionArg(m_Stack.back()));
			m_Stack.pop_back();
		}

		ExecuteModuleFunctionCall(moduleID, functionID, usesReturnValue);
	} break;
	case OpCode::MODULE_CONSTANT: {
		ID moduleID = ReadUInt32();
		uint16 constantID = ReadUInt16();
		ExecuteModuleConstant(moduleID, constantID);
	} break;
	case OpCode::STATIC_FUNCTION_CALL: {
		ID classID = ReadUInt32();
		uint16 functionID = ReadUInt16();
		bool usesReturnValue = ReadUInt8();

		Class* cls = GetClass(classID);
		Function* function = cls->GetFunction(functionID);

		Scope* scope = GetScope(function->scope);
		AddFunctionParametersToScope(scope, function);

		// Save current frame
		CallFrame frame;
		frame.returnPC = m_ProgramCounter; // after this instruction
		frame.basePointer = (uint32)m_Stack.size(); // current stack top before call
		frame.marker = m_StackAllocator->GetMarker();
		frame.functionScope = function->scope;
		frame.usesReturnValue = usesReturnValue;
		frame.popThisStack = false;
		frame.scopeStackSize = m_ScopeStack.size();
		PushCallStack(frame);

		// Jump to function bytecode
		uint32 functionPC = function->pc;
		m_ProgramCounter = functionPC;
	} break;
	case OpCode::MEMBER_FUNCTION_CALL: {
		ID classID = ReadUInt32();
		uint16 functionID = ReadUInt16();
		bool usesReturnValue = ReadUInt8();

		Class* cls = GetClass(classID);
		Function* function = cls->GetFunction(functionID);

		Value objToCallFunctionOn = m_Stack.back(); m_Stack.pop_back();
		m_ThisStack.push_back(objToCallFunctionOn);

		Scope* scope = GetScope(function->scope);
		AddFunctionParametersToScope(scope, function);

		CallFrame frame;
		frame.returnPC = m_ProgramCounter; // after this instruction
		frame.basePointer = (uint32)m_Stack.size(); // current stack top before call
		frame.marker = m_StackAllocator->GetMarker();
		frame.functionScope = function->scope;
		frame.usesReturnValue = usesReturnValue;
		frame.popThisStack = true;
		frame.scopeStackSize = m_ScopeStack.size();
		PushCallStack(frame);

		// Jump to function bytecode
		uint32 functionPC = function->pc;
		m_ProgramCounter = functionPC;
	} break;
	case OpCode::CONSTRUCTOR_CALL: {
		uint16 type = ReadUInt16();
		uint16 functionID = ReadUInt16();

		Value object = Value::MakeObject(this, type, m_StackAllocator);
		Function* function = GetClass(type)->GetFunction(functionID);

		m_ThisStack.push_back(object);

		Scope* scope = GetScope(function->scope);
		AddFunctionParametersToScope(scope, function);

		GetScope(m_CallStack[m_CallStack.size() - 1].functionScope)->AddTemp(object); // Add this object to the functions's scope that calls this constructor
		//TODO: Instead of adding it to the function, call destructor when the object is popped from the stack.
		//Cant think of a way without complicating the stack too much
		m_Stack.push_back(object); 

		CallFrame frame;
		frame.returnPC = m_ProgramCounter; // after this instruction
		frame.basePointer = (uint32)m_Stack.size(); // current stack top before call
		frame.marker = m_StackAllocator->GetMarker();
		frame.functionScope = function->scope;
		frame.usesReturnValue = false;
		frame.scopeStackSize = m_ScopeStack.size();
		PushCallStack(frame);

		m_ProgramCounter = function->pc;
	} break;
	case OpCode::VIRTUAL_FUNCTION_CALL: {
		uint16 functionID = ReadUInt16();
		bool usesReturnValue = ReadUInt8();

		Value objToCallFunctionOn = m_Stack.back(); m_Stack.pop_back();
		m_ThisStack.push_back(objToCallFunctionOn);

		VTable* vtable = *(VTable**)((uint8*)objToCallFunctionOn.data - sizeof(VTable*));
		Function* function = vtable->GetFunction(functionID);

		Scope* scope = GetScope(function->scope);
		AddFunctionParametersToScope(scope, function);

		CallFrame frame;
		frame.returnPC = m_ProgramCounter; // after this instruction
		frame.basePointer = (uint32)m_Stack.size(); // current stack top before call
		frame.marker = m_StackAllocator->GetMarker();
		frame.functionScope = function->scope;
		frame.usesReturnValue = usesReturnValue;
		frame.popThisStack = true;
		frame.scopeStackSize = m_ScopeStack.size();
		PushCallStack(frame);

		// Jump to function bytecode
		uint32 functionPC = function->pc;
		m_ProgramCounter = functionPC;
	} break;
	case OpCode::RETURN: {
		bool hasReturnValue = ReadUInt8();
		bool clone = ReadUInt8();

		CallFrame frame = m_CallStack.back();
		m_CallStack.pop_back();

		while (m_ScopeStack.size() > frame.scopeStackSize)
		{
			std::pair<ID, uint64> scope = m_ScopeStack.back();
			m_ScopeStack.pop_back();

			uint32 count = m_PendingDestructors.size();
			GetScope(scope.first)->Clear(this);
			ExecutePendingDestructors(count);
		}

		if (frame.popThisStack)
			m_ThisStack.pop_back();

		Value ret;
		uint64 returnMarker = m_ReturnAllocator->GetMarker();
		if (hasReturnValue)
		{
			if(frame.usesReturnValue)
			{
				ret = m_Stack.back();
				if(ret.type != (uint16)ValueType::STRING)
				{
					if (!ret.IsPrimitive() && !ret.IsPointer() && clone)
					{
						Class* cls = GetClass(ret.type);
						Function* copyConstructor = cls->GetCopyConstructor();
						
						uint32 ccount = m_PendingCopyConstructors.size();
						if (copyConstructor)
						{
							Value original = ret;
							ret = Value::MakeObject(this, original.type, m_ReturnAllocator);
							m_PendingCopyConstructors.push_back(PendingCopyConstructor(ret, original, copyConstructor));
						}
						else
						{
							Value original = ret;
							AddCopyConstructorRecursive(ret, original);
						}

						ExecutePendingCopyConstructors(ccount);
						uint32 bp = 0;
					}
					else
					{
						ret = ret.Clone(this, m_ReturnAllocator);
					}
				}
			}

			m_Stack.pop_back();
		}

		m_Stack.resize(frame.basePointer);

		uint32 count = m_PendingDestructors.size();
		GetScope(frame.functionScope)->Clear(this);
		ExecutePendingDestructors(count);

		m_StackAllocator->FreeToMarker(frame.marker);

		if (hasReturnValue && frame.usesReturnValue)
		{
			if (ret.type != (uint16)ValueType::STRING)
			{
				ret = ret.Clone(this, m_StackAllocator);
				m_ReturnAllocator->FreeToMarker(returnMarker);
			}

			m_Stack.push_back(ret);
			if(!ret.IsPrimitive() && !ret.IsPointer())
				GetScope(m_CallStack.back().functionScope)->AddTemp(ret);
		}

		m_ProgramCounter = frame.returnPC;
	} break;
	case OpCode::DECLARE_UINT8: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		Value value = Value::MakeUInt8(m_Stack.back().GetUInt8(), m_StackAllocator);
		m_Stack.pop_back();
		GetScope(scopeID)->AddVariable(variableID, value);
	} break;
	case OpCode::DECLARE_UINT16: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		Value value = Value::MakeUInt16(m_Stack.back().GetUInt16(), m_StackAllocator);
		m_Stack.pop_back();
		GetScope(scopeID)->AddVariable(variableID, value);
	} break;
	case OpCode::DECLARE_UINT32: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		Value value = Value::MakeUInt32(m_Stack.back().GetUInt32(), m_StackAllocator);
		m_Stack.pop_back();
		GetScope(scopeID)->AddVariable(variableID, value);
	} break;
	case OpCode::DECLARE_UINT64: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		Value value = Value::MakeUInt64(m_Stack.back().GetUInt64(), m_StackAllocator);
		m_Stack.pop_back();
		GetScope(scopeID)->AddVariable(variableID, value);
	} break;
	case OpCode::DECLARE_INT8: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		Value value = Value::MakeInt8(m_Stack.back().GetInt8(), m_StackAllocator);
		m_Stack.pop_back();
		GetScope(scopeID)->AddVariable(variableID, value);
	} break;
	case OpCode::DECLARE_INT16: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		Value value = Value::MakeInt16(m_Stack.back().GetInt16(), m_StackAllocator);
		m_Stack.pop_back();
		GetScope(scopeID)->AddVariable(variableID, value);
	} break;
	case OpCode::DECLARE_INT32: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		Value value = Value::MakeInt32(m_Stack.back().GetInt32(), m_StackAllocator);
		m_Stack.pop_back();
		GetScope(scopeID)->AddVariable(variableID, value);
	} break;
	case OpCode::DECLARE_INT64: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		Value value = Value::MakeInt64(m_Stack.back().GetInt64(), m_StackAllocator);
		m_Stack.pop_back();
		GetScope(scopeID)->AddVariable(variableID, value);
	} break;
	case OpCode::DECLARE_REAL32: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		Value value = Value::MakeReal32(m_Stack.back().GetReal32(), m_StackAllocator);
		m_Stack.pop_back();
		GetScope(scopeID)->AddVariable(variableID, value);
	} break;
	case OpCode::DECLARE_REAL64: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		Value value = Value::MakeReal64(m_Stack.back().GetReal64(), m_StackAllocator);
		m_Stack.pop_back();
		GetScope(scopeID)->AddVariable(variableID, value);
	} break;
	case OpCode::DECLARE_CHAR: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		Value value = Value::MakeChar(m_Stack.back().GetChar(), m_StackAllocator);
		m_Stack.pop_back();
		GetScope(scopeID)->AddVariable(variableID, value);
	} break;
	case OpCode::DECLARE_BOOL: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		Value value = Value::MakeBool(m_Stack.back().GetBool(), m_StackAllocator);
		m_Stack.pop_back();
		GetScope(scopeID)->AddVariable(variableID, value);
	} break;
	case OpCode::DECLARE_STRING: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		Value back = m_Stack.back();
		Value value = back;
		m_Stack.pop_back();
		GetScope(scopeID)->AddVariable(variableID, value);
	} break;
	case OpCode::DECLARE_POINTER: {
		uint16 type = ReadUInt16();
		uint8 pointerLevel = ReadUInt8();
		uint32 scopeID = ReadUInt32();
		uint32 variableID = ReadUInt32();
		Value value = m_Stack.back();
		m_Stack.pop_back();

		if (value.data == nullptr)
		{
			value.pointerLevel = pointerLevel;
			value.type = type;
		}
		if (value.pointerLevel != pointerLevel) //Error
		{
			return;
		}

		if (value.type != type)
		{
			if (Value::IsPrimitiveType(type) || Value::IsPrimitiveType(value.type))
				return;

			Class* cls = GetClass(value.type);
			if (!cls->InheritsFrom(type))
				return;
		}

		GetScope(scopeID)->AddVariable(variableID, value);
	} break;
	case OpCode::DECLARE_NULLPTR: {
		uint16 type = ReadUInt16();
		uint8 pointerLevel = ReadUInt8();
		uint32 scopeID = ReadUInt32();
		uint32 variableID = ReadUInt32();

		GetScope(scopeID)->AddVariable(variableID, Value::MakePointer(pointerLevel, type, nullptr));
	} break;
	case OpCode::DECLARE_ARRAY: {
		uint16 type = ReadUInt16();
		uint8 pointerLevel = ReadUInt8();
		uint32 length = ReadUInt32();
		uint32 initializerCount = ReadUInt32();
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();

		Value array = Value::MakeArray(this, type, pointerLevel, length, m_StackAllocator);
		array.pointerLevel += pointerLevel;

		if (initializerCount > 0)
		{
			uint64 typeSize = GetTypeSize(type);
			for (int32 i = initializerCount - 1; i >= 0; i--)
			{
				Value value = m_Stack.back();
				m_Stack.pop_back();
				array.IndexAssign(i, typeSize, value);
			}
		}

		GetScope(scopeID)->AddVariable(variableID, array);
	} break;
	case OpCode::DECLARE_OBJECT_WITH_CONSTRUCTOR: {
		uint16 type = ReadUInt16();
		uint16 functionID = ReadUInt16();
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();

		Value object = Value::MakeObject(this, type, m_StackAllocator);
		if (functionID != INVALID_ID)
		{
			Function* function = GetClass(type)->GetFunction(functionID);

			Scope* scope = GetScope(function->scope);
			AddFunctionParametersToScope(scope, function);

			CallFrame frame;
			frame.returnPC = m_ProgramCounter; // after this instruction
			frame.basePointer = (uint32)m_Stack.size(); // current stack top before call
			frame.marker = m_StackAllocator->GetMarker();
			frame.functionScope = function->scope;
			frame.usesReturnValue = false;
			frame.popThisStack = true;
			frame.scopeStackSize = m_ScopeStack.size();

			m_ThisStack.push_back(object);

			PushCallStack(frame);
			m_ProgramCounter = function->pc;
		}

		GetScope(scopeID)->AddVariable(variableID, object);
	} break;
	case OpCode::DECLARE_OBJECT_WITH_ASSIGN: {
		uint16 type = ReadUInt16();
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		uint16 assignFunctionID = ReadUInt16();

		Value assignValue = m_Stack.back(); m_Stack.pop_back();

		Value object = Value::MakeObject(this, type, m_StackAllocator);

		if (assignFunctionID == INVALID_ID)
		{
			object.Assign(assignValue);
		}
		else
		{
			Function* function = GetClass(type)->GetFunction(assignFunctionID);
			ExecuteAssignFunction(object, assignValue, function);
		}

		GetScope(scopeID)->AddVariable(variableID, object);
	} break;
	case OpCode::ADDRESS_OF: {
		Value value = m_Stack.back();
		m_Stack.pop_back();
		Value pointer = Value::MakePointer(value.pointerLevel + 1, value.type, value.data);
		m_Stack.push_back(pointer);
	} break;
	case OpCode::DEREFERENCE: {
		Value value = m_Stack.back();
		m_Stack.pop_back();
		value = value.Dereference();
		m_Stack.push_back(value);
	} break;
	case OpCode::VARIABLE_SET: {
		Value value = m_Stack.back();
		m_Stack.pop_back();
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		uint16 assignFunctionID = ReadUInt16();
		Value* variable = GetScope(scopeID)->GetVariable(variableID);
		if (variable->IsPointer())
		{
			variable->data = value.data;
		}
		else if(assignFunctionID == INVALID_ID)
		{
			variable->Assign(value);
		}
		else //if assignFunctionID is valid then the variable is guaranteed to not be a pointer
		{
			uint16 variableType = ReadUInt16();
			Class* cls = GetClass(variableType);
			Function* assignFunction = cls->GetFunction(assignFunctionID);

			ExecuteAssignFunction(*variable, value, assignFunction);
		}
	} break;
	case OpCode::MEMBER_SET: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		uint64 offset = ReadUInt64();
		uint16 type = ReadUInt16();
		uint64 typeSize = ReadUInt64();
		uint8 memberPointerLevel = ReadUInt8();
		bool indexArray = ReadUInt8();
		uint16 assignFunctionID = ReadUInt16();
		uint32 oo = offset;
		uint32 i = 0;
		if (indexArray)
		{
			uint32 index = m_Stack.back().GetUInt32();
			i = index;
			m_Stack.pop_back();
			offset = typeSize * index + offset;
		}

		Value* variable = GetScope(scopeID)->GetVariable(variableID);
		Value assignValue = m_Stack.back(); m_Stack.pop_back();

		if (memberPointerLevel > 0)
		{
			if (indexArray)
			{
				void* v = *(void**)((uint8*)variable->data + oo);
				v = (uint8*)v + i * typeSize;
				memcpy(v, assignValue.data, typeSize);
			}
			else
				*(void**)((uint8*)variable->data + offset) = assignValue.data;
		}
		else if (assignFunctionID == INVALID_ID)
		{
			variable->AssignMember(offset, type, memberPointerLevel, typeSize, assignValue);
		}
		else
		{
			Value member;
			member.type = type;
			member.pointerLevel = 0;
			member.isArray = false;
			member.data = (uint8*)variable->data + offset;

			Function* assignFunction = GetClass(type)->GetFunction(assignFunctionID);
			ExecuteAssignFunction(member, assignValue, assignFunction);
		}
	} break;
	case OpCode::INDEX: {
		Value index = m_Stack.back(); m_Stack.pop_back();
		Value base = m_Stack.back(); m_Stack.pop_back();

		if (base.IsPointer() && !base.isArray)
		{
			uint64 typeSize = GetTypeSize(base.type);
			uint32 indexValue = index.GetUInt32();
			Value element;
			element.type = base.type;
			element.pointerLevel = base.pointerLevel - 1;
			element.isArray = false;
			if (element.pointerLevel == 0)
				element.data = (uint8*)base.data + typeSize * indexValue;
			else
				element.data = *(void**)((uint8*)base.data + typeSize * indexValue);

			m_Stack.push_back(element);
			break;
		}

		if (base.type == (uint16)ValueType::STRING && base.pointerLevel == 0)
		{
			uint32 indexValue = index.GetUInt32();
			char* chars = (char*)((uint8*)base.data + sizeof(uint32));
			Value result = Value::MakeChar(chars[indexValue], m_StackAllocator);
			m_Stack.push_back(result);
			break;
		}

		uint32 elementSize = GetTypeSize(base.type);
		if (!base.IsPrimitive() && base.pointerLevel == 1) // if its an array of non pointer objects
			elementSize += sizeof(VTable*);

		uint32 offset = index.GetUInt32();

		uint8* address = (uint8*)base.data + offset * elementSize;
		if (base.pointerLevel > 1)
			address = *(uint8**)address;

		ArrayHeader* header = (ArrayHeader*)(address - sizeof(ArrayHeader));

		Value result;
		result.type = base.type;
		result.pointerLevel = header->elementPointerLevel;
		result.data = address;
		result.isArray = false;

		m_Stack.push_back(result);
	} break;
	case OpCode::INDEX_ASSIGN: {
		uint16 assignFunctionID = ReadUInt16();
		Value index = m_Stack.back(); m_Stack.pop_back();
		Value base = m_Stack.back(); m_Stack.pop_back();
		Value assign = m_Stack.back(); m_Stack.pop_back();
		uint32 elementSize = GetTypeSize(base.type);
		if (!base.IsPrimitive() && base.pointerLevel == 1) // if its an array of non pointer objects
			elementSize += sizeof(VTable*);

		uint32 offset = index.GetUInt32();

		if (base.pointerLevel > 1)
		{
			*(void**)((uint8*)base.data + offset * elementSize) = assign.data;
		}
		if (assignFunctionID == INVALID_ID) //only primitive, objects are guaranteed to have an assign function
		{
			uint8* address = (uint8*)base.data + offset * elementSize;
			memcpy(address, assign.data, elementSize);
		}
		else
		{
			Value element;
			element.type = base.type;
			element.pointerLevel = 0;
			element.isArray = false;
			element.data = (uint8*)base.data + offset * elementSize + sizeof(VTable*);

			Function* assignFunction = GetClass(element.type)->GetFunction(assignFunctionID);

			ExecuteAssignFunction(element, assign, assignFunction);
		}
	} break;
	case OpCode::NEW_ARRAY: {
		Value length = m_Stack.back(); m_Stack.pop_back();
		uint16 type = ReadUInt16();
		uint8 pointerLevel = ReadUInt8();

		Value array = Value::MakeArray(this, type, pointerLevel, length.GetUInt32(), m_HeapAllocator);
		array.pointerLevel += pointerLevel;
		m_Stack.push_back(array);
	} break;
	case OpCode::DIRECT_MEMBER_ACCESS: {
		uint64 offset = ReadUInt64();
		uint16 memberType = ReadUInt16();
		uint8 memberPointerLevel = ReadUInt8();
		Value thisValue = m_ThisStack.back();
		Value member; //HERE
		member.type = memberType;
		member.pointerLevel = memberPointerLevel;
		member.isArray = false;
		if(memberPointerLevel > 0)
			member.data = *(void**)((uint8*)thisValue.data + offset);
		else
			member.data = (uint8*)thisValue.data + offset;

		

		m_Stack.push_back(member);
	} break;
	case OpCode::DIRECT_MEMBER_ASSIGN: {
		uint64 offset = ReadUInt64();
		uint16 memberType = ReadUInt16();
		uint8 memberPointerLevel = ReadUInt8();
		uint64 memberTypeSize = ReadUInt64();
		uint16 assignFunctionID = ReadUInt16();
		Value& thisValue = m_ThisStack.back();
		Value& assignValue = m_Stack.back(); m_Stack.pop_back();

		if (memberPointerLevel > 0)
		{
			*(void**)((uint8*)thisValue.data + offset) = assignValue.data;
		}
		else if (assignFunctionID == INVALID_ID)
		{
			thisValue.AssignMember(offset, memberType, memberPointerLevel, memberTypeSize, assignValue);
		}
		else
		{
			Value member;
			member.type = memberType;
			member.isArray = false;
			member.pointerLevel = 0;
			member.data = (uint8*)thisValue.data + offset;

			Function* assignFunction = GetClass(memberType)->GetFunction(assignFunctionID);

			ExecuteAssignFunction(member, assignValue, assignFunction);
		}
	} break;
	case OpCode::UNARY_UPDATE: {
		uint8 type = ReadUInt8();
		bool pushToStack = ReadUInt8();
		switch (type)
		{
		case 0: { //Pre-inc
			Value value = m_Stack.back();
			value.Increment();
			if (!pushToStack)
				m_Stack.pop_back();
		} break;
		case 1: { //Pre-dec
			Value value = m_Stack.back();
			value.Decrement();
			if (!pushToStack)
				m_Stack.pop_back();
		} break;
		case 2: { //Post-inc
			Value value = m_Stack.back(); m_Stack.pop_back();
			Value clone = pushToStack ? value.Clone(this, m_StackAllocator) : Value::MakeNULL();
			value.Increment();
			if (pushToStack)
				m_Stack.push_back(clone);
		} break;
		case 3: { //Post-dec
			Value value = m_Stack.back();
			Value clone = pushToStack ? value.Clone(this, m_StackAllocator) : Value::MakeNULL();
			value.Decrement();
			if (pushToStack)
				m_Stack.push_back(clone);
		} break;
		}
	} break;
	case OpCode::DELETE: {
		Value value = m_Stack.back();
		m_Stack.pop_back();
		uint32 count = m_PendingDestructors.size();
		AddDestructorRecursive(value);
		m_PendingDelete = (uint8*)value.data - sizeof(VTable*);
		ExecutePendingDestructors(count);
	} break;
	case OpCode::DELETE_ARRAY: {
		Value value = m_Stack.back();
		m_Stack.pop_back();

		uint32 count = m_PendingDestructors.size();
		ArrayHeader* arrayHeader = (ArrayHeader*)((uint8*)value.data - sizeof(ArrayHeader));
		if (arrayHeader->elementPointerLevel == 0)
		{
			uint64 typeSize = GetTypeSize(value.type);
			for (uint32 i = 0; i < arrayHeader->length; i++)
			{
				Value element;
				element.type = value.type;
				element.isArray = false;
				element.pointerLevel = 0;
				element.data = (uint8*)value.data + (typeSize * i);
				AddDestructorRecursive(element);
			}
		}

		m_PendingDelete = arrayHeader;
		ExecutePendingDestructors(count);
	} break;
	case OpCode::NEW: {
		uint16 type = ReadUInt16();
		uint16 functionID = ReadUInt16();
		Value object = Value::MakeObject(this, type, m_HeapAllocator);
		object.pointerLevel = 1;
		if (functionID != INVALID_ID)
		{
			Function* function = GetClass(type)->GetFunction(functionID);

			m_ThisStack.push_back(object);

			Scope* scope = GetScope(function->scope);
			AddFunctionParametersToScope(scope, function);

			m_Stack.push_back(object);

			CallFrame frame;
			frame.returnPC = m_ProgramCounter; // after this instruction
			frame.basePointer = (uint32)m_Stack.size(); // current stack top before call
			frame.marker = m_StackAllocator->GetMarker();
			frame.functionScope = function->scope;
			frame.usesReturnValue = false;
			frame.popThisStack = true;
			frame.scopeStackSize = m_ScopeStack.size();
			PushCallStack(frame);

			m_ProgramCounter = function->pc;
		}
		else
		{
			m_Stack.push_back(object);
		}
	} break;
	case OpCode::NOT: {
		Value value = m_Stack.back();
		m_Stack.pop_back();
		m_Stack.push_back(value.Invert(m_StackAllocator));
	} break;
	case OpCode::POINTER_CAST: {
		uint16 castToType = ReadUInt16();
		Value value = m_Stack.back();
		m_Stack.pop_back();
		value.type = castToType;
		m_Stack.push_back(value);
	} break;
	case OpCode::STRLEN: {
		Value str = m_Stack.back();
		m_Stack.pop_back();
		uint32 length = *(uint32*)str.data;
		m_Stack.push_back(Value::MakeUInt32(length, m_StackAllocator));
	} break;
	} 
}

void Program::ExecuteModuleFunctionCall(ID moduleID, uint16 function, bool usesReturnValue)
{
	Value value = Value::MakeNULL();
	switch (moduleID)
	{
	case IO_MODULE_ID: value = IOModule::CallFunction(this, function, m_ArgStorage); break;
	case MATH_MODULE_ID: value = MathModule::CallFunction(this, function, m_ArgStorage); break;
	case WINDOW_MODULE_ID: value = WindowModule::CallFunction(this, function, m_ArgStorage); break;
	case OPENGL_MODULE_ID: value = GLModule::CallFunction(this, function, m_ArgStorage); break;
	}

	if (value.type != INVALID_ID && usesReturnValue)
		m_Stack.push_back(value);
}

void Program::ExecuteModuleConstant(ID moduleID, uint16 constant)
{
	switch (moduleID)
	{
	case IO_MODULE_ID: m_Stack.push_back(IOModule::Constant(this, constant)); break;
	case MATH_MODULE_ID: m_Stack.push_back(MathModule::Constant(this, constant)); break;
	case WINDOW_MODULE_ID: m_Stack.push_back(WindowModule::Constant(this, constant)); break;
	case OPENGL_MODULE_ID: m_Stack.push_back(GLModule::Constant(this, constant)); break;
	}
}

void Program::AddFunctionParametersToScope(Scope* scope, Function* function)
{
	for (int32 i = function->parameters.size() - 1; i >= 0; i--)
	{
		const FunctionParameter& param = function->parameters[i];
		Value arg = m_Stack.back(); m_Stack.pop_back();
		if (!param.isReference)
		{
			Value original = arg;
			arg = arg.Clone(this, m_StackAllocator);
			if (!Value::IsPrimitiveType(param.type.type) && param.type.pointerLevel == 0)
			{
				Class* cls = GetClass(param.type.type);
				Function* copyConstructor = cls->GetCopyConstructor();
				uint32 count = m_PendingCopyConstructors.size();
				if (copyConstructor)
				{
					m_PendingCopyConstructors.push_back(PendingCopyConstructor(arg, original, copyConstructor));
				}
				else
				{
					AddCopyConstructorRecursive(arg, original);
				}

				ExecutePendingCopyConstructors(count);
			}
		}
		else
		{
			arg.pointerLevel = POINTER_LEVEL_REFERENCE;
		}

		if (arg.type != param.type.type)
		{
			arg = arg.CastTo(this, param.type.type, param.isReference ? POINTER_LEVEL_REFERENCE : param.type.pointerLevel, m_StackAllocator);
		}

		scope->AddVariable(param.variableID, arg);
	}
}

void Program::AddCopyConstructorRecursive(const Value& dst, const Value& src, uint64 offset)
{
	Class* cls = GetClass(dst.type);
	const std::vector<ClassField>& members = cls->GetMemberFields();
	for (uint32 i = 0; i < members.size(); i++)
	{
		const TypeInfo& memberType = members[i].type;
		uint64 memberOffset = members[i].offset;
		if (Value::IsPrimitiveType(memberType.type) || memberType.pointerLevel > 0)
			continue;

		Value dstMember = Value::MakeNULL(memberType.type, 0);
		Value srcMember = Value::MakeNULL(memberType.type, 0);

		dstMember.data = (uint8*)dst.data + memberOffset + offset;
		srcMember.data = (uint8*)src.data + memberOffset + offset;

		Class* memberClass = GetClass(memberType.type);
		Function* copyConstructor = memberClass->GetCopyConstructor();
		if (copyConstructor)
		{
			m_PendingCopyConstructors.push_back(PendingCopyConstructor(dstMember, srcMember, copyConstructor));
		}
		else
		{
			AddCopyConstructorRecursive(dst, src, offset + memberOffset);
		}
	}
}

void Program::ExecutePendingCopyConstructors(uint32 offset)
{
	for (uint32 i = offset; i < m_PendingCopyConstructors.size(); i++)
	{
		const PendingCopyConstructor& cc = m_PendingCopyConstructors[i];
		const Value& dst = cc.dst;
		const Value& src = cc.src;
		Function* function = cc.function;

		m_Stack.push_back(src);
		m_ThisStack.push_back(dst);

		Scope* scope = GetScope(function->scope);
		AddFunctionParametersToScope(scope, function);

		CallFrame frame;
		frame.returnPC = m_ProgramCounter; // after this instruction
		frame.basePointer = (uint32)m_Stack.size(); // current stack top before call
		frame.marker = m_StackAllocator->GetMarker();
		frame.functionScope = function->scope;
		frame.usesReturnValue = false;
		frame.popThisStack = true;
		frame.scopeStackSize = m_ScopeStack.size();
		PushCallStack(frame);

		m_ProgramCounter = function->pc;
		while (m_ProgramCounter != frame.returnPC)
		{
			OpCode opcode = ReadOPCode();
			if (opcode == OpCode::END) break;
			ExecuteOpCode(opcode);
		}
	}

	m_PendingCopyConstructors.resize(offset);
}

void Program::ExecuteAssignFunction(const Value& thisValue, const Value& assignValue, Function* assignFunction)
{
	m_Stack.push_back(assignValue);
	Scope* scope = GetScope(assignFunction->scope);
	AddFunctionParametersToScope(scope, assignFunction);

	m_ThisStack.push_back(thisValue);

	CallFrame frame;
	frame.returnPC = m_ProgramCounter; // after this instruction
	frame.basePointer = (uint32)m_Stack.size(); // current stack top before call
	frame.marker = m_StackAllocator->GetMarker();
	frame.functionScope = assignFunction->scope;
	frame.usesReturnValue = false;
	frame.popThisStack = true;
	frame.scopeStackSize = m_ScopeStack.size();
	PushCallStack(frame);

	m_ProgramCounter = assignFunction->pc;
}

void Program::ExecuteArithmaticFunction(const Value& lhs, const Value& rhs, Function* function)
{
	m_Stack.push_back(rhs);
	m_ThisStack.push_back(lhs);

	Scope* scope = GetScope(function->scope);
	AddFunctionParametersToScope(scope, function);

	CallFrame frame;
	frame.returnPC = m_ProgramCounter; // after this instruction
	frame.basePointer = (uint32)m_Stack.size(); // current stack top before call
	frame.marker = m_StackAllocator->GetMarker();
	frame.functionScope = function->scope;
	frame.usesReturnValue = true;
	frame.popThisStack = true;
	frame.scopeStackSize = m_ScopeStack.size();
	PushCallStack(frame);

	m_ProgramCounter = function->pc;
}

void Program::WriteUInt64(uint64 value)
{
	uint8* bytes = reinterpret_cast<uint8*>(&value);
	m_Code.insert(m_Code.end(), bytes, bytes + sizeof(uint64));
}

void Program::WriteUInt32(uint32 value)
{
	uint8* bytes = reinterpret_cast<uint8*>(&value);
	m_Code.insert(m_Code.end(), bytes, bytes + sizeof(uint32));
}

void Program::WriteUInt16(uint16 value)
{
	uint8* bytes = reinterpret_cast<uint8*>(&value);
	m_Code.insert(m_Code.end(), bytes, bytes + sizeof(uint16));
}

void Program::WriteUInt8(uint8 value)
{
	m_Code.push_back(value);
}

void Program::WriteInt8(int8 value)
{
	m_Code.push_back(static_cast<uint8>(value));
}

void Program::WriteInt16(int16 value)
{
	uint8* bytes = reinterpret_cast<uint8*>(&value);
	m_Code.insert(m_Code.end(), bytes, bytes + sizeof(int16));
}

void Program::WriteInt32(int32 value)
{
	uint8* bytes = reinterpret_cast<uint8*>(&value);
	m_Code.insert(m_Code.end(), bytes, bytes + sizeof(int32));
}

void Program::WriteInt64(int64 value)
{
	uint8* bytes = reinterpret_cast<uint8*>(&value);
	m_Code.insert(m_Code.end(), bytes, bytes + sizeof(int64));
}

void Program::WriteReal32(real32 value)
{
	uint8* bytes = reinterpret_cast<uint8*>(&value);
	m_Code.insert(m_Code.end(), bytes, bytes + sizeof(real32));
}

void Program::WriteReal64(real64 value)
{
	uint8* bytes = reinterpret_cast<uint8*>(&value);
	m_Code.insert(m_Code.end(), bytes, bytes + sizeof(real64));
}

void Program::WriteOPCode(OpCode code)
{
	WriteUInt16((uint16)code);
}

void Program::WriteString(const std::string& str)
{
	WriteUInt32(static_cast<uint32>(str.size()));
	m_Code.insert(m_Code.end(), str.begin(), str.end());
}

void Program::PatchUInt32(uint32 pos, uint32 value)
{
	uint8* bytes = reinterpret_cast<uint8*>(&value);
	std::memcpy(&m_Code[pos], bytes, sizeof(uint32));
}

void Program::PatchPushLoopCommand(uint32 pos, uint32 start, uint32 end)
{
	uint8* startBytes = reinterpret_cast<uint8*>(&start);
	uint8* endBytes = reinterpret_cast<uint8*>(&end);
	std::memcpy(&m_Code[pos], startBytes, sizeof(uint32));
	std::memcpy(&m_Code[pos + sizeof(uint32)], endBytes, sizeof(uint32));
}

HeapAllocator* Program::GetHeapAllocator() const
{
	return m_HeapAllocator;
}

BumpAllocator* Program::GetStackAllocator() const
{
	return m_StackAllocator;
}

std::string Program::GetTypeName(uint16 type)
{
	switch ((ValueType)type)
	{
	case ValueType::BOOL:    return "bool";
	case ValueType::CHAR:    return "char";
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
	case ValueType::VOID_T:	 return "void";
	case ValueType::STRING:	 return "string";
	case ValueType::TEMPLATE_TYPE: return "template_type";
	default: return GetClass(type)->GetName();
	}
}

uint64 Program::ReadUInt64()
{
	uint64* value = (uint64*)&m_Code[m_ProgramCounter];
	m_ProgramCounter += sizeof(uint64);
	return *value;
}

uint32 Program::ReadUInt32() 
{
	uint32* value = (uint32*)&m_Code[m_ProgramCounter];
	m_ProgramCounter += sizeof(uint32);
	return *value;
}

uint16 Program::ReadUInt16()
{
	uint16* value = (uint16*)&m_Code[m_ProgramCounter];
	m_ProgramCounter += sizeof(uint16);
	return *value;
}

uint8 Program::ReadUInt8()
{
	return m_Code[m_ProgramCounter++];
}

int8 Program::ReadInt8()
{
	return static_cast<int8>(m_Code[m_ProgramCounter++]);
}

int16 Program::ReadInt16()
{
	int16* value = (int16*)&m_Code[m_ProgramCounter];
	m_ProgramCounter += sizeof(int16);
	return *value;
}

int32 Program::ReadInt32()
{
	int32* value = (int32*)&m_Code[m_ProgramCounter];
	m_ProgramCounter += sizeof(int32);
	return *value;
}

int64 Program::ReadInt64()
{
	int64* value = (int64*)&m_Code[m_ProgramCounter];
	m_ProgramCounter += sizeof(int64);
	return *value;
}

real32 Program::ReadReal32()
{
	real32* value = (real32*)&m_Code[m_ProgramCounter];
	m_ProgramCounter += sizeof(real32);
	return *value;
}

real64 Program::ReadReal64()
{
	real64* value = (real64*)&m_Code[m_ProgramCounter];
	m_ProgramCounter += sizeof(real64);
	return *value;
}

OpCode Program::ReadOPCode()
{
	return (OpCode)ReadUInt16();
}

std::string Program::ReadString()
{
	uint32 length = ReadUInt32();

	// Allocate string of proper size
	std::string str(length, '\0');

	std::memcpy(str.data(), &m_Code[m_ProgramCounter], length);
	m_ProgramCounter += length;

	return str;
}

void Program::AddModule(const std::string& name, ID id)
{
	m_ModuleNameMap[name] = id;
}

ID Program::GetModuleID(const std::string& name) const
{
	const auto&& it = m_ModuleNameMap.find(name);
	if (it == m_ModuleNameMap.end())
		return INVALID_ID;
	return it->second;
}

ID Program::AddClass(const std::string& name, Class* cls, ID generatedID)
{
	ID classID = (generatedID == INVALID_ID) ? GenClassID() : generatedID;
	cls->SetID(classID);
	m_ClassNameMap[name] = classID;
	m_Classes[classID] = cls;
	return classID;
}

void Program::AddClass2(Class* cls, ID generatedID)
{
	m_Classes[generatedID] = cls;
}

ID Program::GetClassID(const std::string& name) const
{
	const auto&& it = m_ClassNameMap.find(name);
	if (it == m_ClassNameMap.end())
		return INVALID_ID;
	return it->second;
}

Class* Program::GetClass(ID id)
{
	const auto&& it = m_Classes.find(id);
	if (it == m_Classes.end())
		return nullptr;

	return it->second;
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

uint16 Program::GetTypeID(const std::string& name) const
{
	ValueType primitiveType = PrimitiveTypeFromName(name);
	if (primitiveType != ValueType::LAST_TYPE) return (uint16)primitiveType;

	return GetClassID(name);
}

Program* Program::GetCompiledProgram()
{
	return g_CompiledProgram;
}
