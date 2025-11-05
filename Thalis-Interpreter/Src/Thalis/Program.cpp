#include "Program.h"
#include "Modules/IOModule.h"
#include "Modules/ModuleID.h"
#include "Class.h"
#include "Scope.h"
#include "Memory/Memory.h"
#include "ASTExpression.h"

static Program* g_CompiledProgram;

Program::Program() :
	m_ProgramCounter(0)
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

	for (uint32 i = 0; i < m_StringLiterals.size(); i++)
		m_HeapAllocator->Free(m_StringLiterals[i]);
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

void Program::AddPushMemberCommand(ID scope, ID variableID, uint64 offset, uint16 memberType, uint8 memberPointerLevel, uint32 index)
{
	WriteOPCode(OpCode::PUSH_MEMBER);
	WriteUInt32(scope);
	WriteUInt32(variableID);
	WriteUInt64(offset);
	WriteUInt16(memberType);
	WriteUInt8(memberPointerLevel);
	WriteUInt32(index);
}

void Program::AddEndCommand()
{
	WriteOPCode(OpCode::END);
}

void Program::AddReturnCommand(bool returnsValue)
{
	WriteOPCode(OpCode::RETURN);
	WriteUInt8(returnsValue);
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

void Program::AddDeclareObjectWithAssignCommand(uint16 type, ID scope, ID variableID)
{
	WriteOPCode(OpCode::DECLARE_OBJECT_WITH_ASSIGN);
	WriteUInt16(type);
	WriteUInt32(scope);
	WriteUInt32(variableID);
}

void Program::AddVariableSetCommand(ID scope, ID variableID)
{
	WriteOPCode(OpCode::VARIABLE_SET);
	WriteUInt32(scope);
	WriteUInt32(variableID);
}

void Program::AddMemberSetCommand(ID scope, ID variableID, uint64 offset, uint16 type, uint64 size, uint8 memberPointerLevel, uint32 arrayIndex)
{
	WriteOPCode(OpCode::MEMBER_SET);
	WriteUInt32(scope);
	WriteUInt32(variableID);
	WriteUInt64(offset);
	WriteUInt16(type);
	WriteUInt64(size);
	WriteUInt8(memberPointerLevel);
	WriteUInt32(arrayIndex);
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

void Program::AddDirectMemberAccessCommand(uint64 offset, uint16 memberType, uint8 memberPointerLevel)
{
	WriteOPCode(OpCode::DIRECT_MEMBER_ACCESS);
	WriteUInt64(offset);
	WriteUInt16(memberType);
	WriteUInt8(memberPointerLevel);
}

void Program::AddDirectMemberAssignCommand(uint64 offset, uint16 memberType, uint8 memberPointerLevel, uint64 memberTypeSize)
{
	WriteOPCode(OpCode::DIRECT_MEMBER_ASSIGN);
	WriteUInt64(offset);
	WriteUInt16(memberType);
	WriteUInt8(memberPointerLevel);
	WriteUInt64(memberTypeSize);
}

uint32 Program::GetCodeSize() const
{
	return m_Code.size();
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

void Program::InitStatics()
{
	for (auto&& it : m_Classes)
	{
		it.second->InitStatics(this);
	}
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

void Program::ExecuteOpCode(OpCode opcode)
{
	switch (opcode)
	{
	case OpCode::JUMP: {
		m_ProgramCounter = ReadUInt32();
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
		Value string = Value::MakeString(ReadString(), m_HeapAllocator);
		m_Stack.push_back(string);
		m_StringLiterals.push_back(string.data);
		//GetScope(scopeID)->AddTemp(m_Stack.back());
	} break;
	case OpCode::PUSH_VARIABLE: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		m_Stack.push_back(*GetScope(scopeID)->GetVariable(variableID));
	} break;
	case OpCode::PUSH_MEMBER: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		uint64 offset = ReadUInt64();
		uint16 memberType = ReadUInt16();
		uint8 memberPointerLevel = ReadUInt8();
		uint32 index = ReadUInt32();
		if (index > 0)
		{
			uint64 typeSize = GetTypeSize(memberType);
			offset = typeSize * index + offset;
		}

		Value* variable = GetScope(scopeID)->GetVariable(variableID);
		Value member;
		member.type = memberType;
		member.pointerLevel = memberPointerLevel;

		void* memberAddress = (uint8*)variable->data + offset;

		if ((ValueType)memberType == ValueType::STRING || memberPointerLevel > 0)
		{
			// Field stores a pointer to the string data
			member.data = *(void**)memberAddress;
		}
		else
		{
			// Field is direct value
			member.data = memberAddress;
		}

		m_Stack.push_back(member);
	} break;
	case OpCode::ADD: {
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		Value sum = lhs.Add(rhs, m_StackAllocator);
		m_Stack.push_back(sum);
	} break;
	case OpCode::SUBTRACT: {
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		Value sum = lhs.Sub(rhs, m_StackAllocator);
		m_Stack.push_back(sum);
	} break;
	case OpCode::MULTIPLY: {
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		Value sum = lhs.Mul(rhs, m_StackAllocator);
		m_Stack.push_back(sum);
	} break;
	case OpCode::DIVIDE: {
		Value rhs = m_Stack.back(); m_Stack.pop_back();
		Value lhs = m_Stack.back(); m_Stack.pop_back();
		Value sum = lhs.Div(rhs, m_StackAllocator);
		m_Stack.push_back(sum);
	} break;
	case OpCode::POP: {
		m_Stack.pop_back();
	} break;
	case OpCode::DUP: {
		Value dup = m_Stack.back();
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
		for (int32 i = argCount - 1; i >= 0; i--)
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

		// Save current frame
		CallFrame frame;
		frame.returnPC = m_ProgramCounter; // after this instruction
		frame.basePointer = (uint32)m_Stack.size(); // current stack top before call
		frame.marker = m_StackAllocator->GetMarker();
		frame.functionScope = function->scope;
		frame.usesReturnValue = usesReturnValue;
		frame.popThisStack = false;
		m_CallStack.push_back(frame);

		Scope* scope = GetScope(function->scope);
		for (int32 i = function->parameters.size() - 1; i >= 0 ; i--)
		{
			const FunctionParameter& param = function->parameters[i];
			Value arg = m_Stack.back();
			m_Stack.pop_back();
			if (arg.type != param.type.type)
				arg = arg.CastTo(this, param.type.type, param.type.pointerLevel, m_StackAllocator);

			scope->AddVariable(param.variableID, arg);
		}

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

		CallFrame frame;
		frame.returnPC = m_ProgramCounter; // after this instruction
		frame.basePointer = (uint32)m_Stack.size(); // current stack top before call
		frame.marker = m_StackAllocator->GetMarker();
		frame.functionScope = function->scope;
		frame.usesReturnValue = usesReturnValue;
		frame.popThisStack = true;
		m_CallStack.push_back(frame);

		Scope* scope = GetScope(function->scope);
		for (int32 i = function->parameters.size() - 1; i >= 0; i--)
		{
			const FunctionParameter& param = function->parameters[i];
			Value arg = m_Stack.back();
			m_Stack.pop_back();
			if (arg.type != param.type.type)
				arg = arg.CastTo(this, param.type.type, param.type.pointerLevel, m_StackAllocator);

			scope->AddVariable(param.variableID, arg);
		}

		// Jump to function bytecode
		uint32 functionPC = function->pc;
		m_ProgramCounter = functionPC;
	} break;
	case OpCode::RETURN: {
		bool hasReturnValue = ReadUInt8();

		CallFrame frame = m_CallStack.back();
		m_CallStack.pop_back();

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
					ret = ret.Clone(this, m_ReturnAllocator);
			}

			m_Stack.pop_back();
		}

		m_Stack.resize(frame.basePointer);

		GetScope(frame.functionScope)->Clear(this);
		m_StackAllocator->FreeToMarker(frame.marker);

		if (hasReturnValue && frame.usesReturnValue)
		{
			if (ret.type != (uint16)ValueType::STRING)
			{
				ret = ret.Clone(this, m_StackAllocator);
				m_ReturnAllocator->FreeToMarker(returnMarker);
			}

			m_Stack.push_back(ret);
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

		if (value.type != type || value.pointerLevel != pointerLevel) //Error
		{
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
			//TODO: Call constructor
		}

		GetScope(scopeID)->AddVariable(variableID, object);
	} break;
	case OpCode::DECLARE_OBJECT_WITH_ASSIGN: {
		uint16 type = ReadUInt16();
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();

		Value assignValue = m_Stack.back(); m_Stack.pop_back();

		Value object = Value::MakeObject(this, type, m_StackAllocator);
		object.Assign(assignValue);

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
		GetScope(scopeID)->GetVariable(variableID)->Assign(value);
	} break;
	case OpCode::MEMBER_SET: {
		ID scopeID = ReadUInt32();
		ID variableID = ReadUInt32();
		uint64 offset = ReadUInt64();
		uint16 type = ReadUInt16();
		uint64 typeSize = ReadUInt64();
		uint8 memberPointerLevel = ReadUInt8();
		uint32 index = ReadUInt32();
		if (index > 0)
		{
			offset = typeSize * index + offset;
		}

		Value* variable = GetScope(scopeID)->GetVariable(variableID);
		Value assignValue = m_Stack.back(); m_Stack.pop_back();
		variable->AssignMember(offset, type, memberPointerLevel, typeSize, assignValue);
	} break;
	case OpCode::INDEX: {
		Value index = m_Stack.back(); m_Stack.pop_back();
		Value base = m_Stack.back(); m_Stack.pop_back();
		uint32 elementSize = GetTypeSize(base.type);
		uint32 offset = index.GetUInt32();

		uint8* address = (uint8*)base.data + offset * elementSize;
		ArrayHeader* header = (ArrayHeader*)(address - sizeof(ArrayHeader));

		Value result;
		result.type = base.type;
		result.pointerLevel = header->elementPointerLevel;
		result.data = address;

		m_Stack.push_back(result);
	} break;
	case OpCode::INDEX_ASSIGN: {
		Value index = m_Stack.back(); m_Stack.pop_back();
		Value base = m_Stack.back(); m_Stack.pop_back();
		Value assign = m_Stack.back(); m_Stack.pop_back();
		uint32 elementSize = GetTypeSize(base.type);
		uint32 offset = index.GetUInt32();

		uint8* address = (uint8*)base.data + offset * elementSize;
		memcpy(address, assign.data, elementSize);
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
		Value& thisValue = m_ThisStack.back();
		Value member;
		member.type = memberType;
		member.pointerLevel = memberPointerLevel;
		member.data = (uint8*)thisValue.data + offset;

		m_Stack.push_back(member);
	} break;
	case OpCode::DIRECT_MEMBER_ASSIGN: {
		uint64 offset = ReadUInt64();
		uint16 memberType = ReadUInt16();
		uint8 memberPointerLevel = ReadUInt8();
		uint64 memberTypeSize = ReadUInt64();
		Value& thisValue = m_ThisStack.back();
		Value& assignValue = m_Stack.back(); m_Stack.pop_back();

		thisValue.AssignMember(offset, memberType, memberPointerLevel, memberTypeSize, assignValue);
	} break;
	} 
}

void Program::ExecuteModuleFunctionCall(ID moduleID, uint16 function, bool usesReturnValue)
{
	switch (moduleID)
	{
	case IO_MODULE_ID: {
		IOModule::CallFunction(function, m_ArgStorage);
	} break;
	}
}

void Program::ExecuteModuleConstant(ID moduleID, uint16 constant)
{
	switch (moduleID)
	{
	case IO_MODULE_ID: {
		m_Stack.push_back(IOModule::Constant(constant));
	} break;
	}
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

void Program::AddClass(const std::string& name, Class* cls)
{
	ID classID = GenClassID();
	cls->SetID(classID);
	m_ClassNameMap[name] = classID;
	m_Classes[classID] = cls;
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
	return m_Classes[id];
}

Program* Program::GetCompiledProgram()
{
	return g_CompiledProgram;
}
