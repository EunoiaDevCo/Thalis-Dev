#include "ASTExpression.h"
#include "Program.h"
#include "Scope.h"
#include "Class.h"
#include "Modules/ModuleID.h"

void* ASTExpression::operator new(std::size_t size) {
	void* ptr = ::operator new(size);
	Program::GetCompiledProgram()->AddCreatedExpression((ASTExpression*)ptr);
	return ptr;
}

void ASTExpression::operator delete(void* ptr) noexcept {
	::operator delete(ptr);
}

void ASTExpressionLiteral::EmitCode(Program* program)
{
	switch ((ValueType)value.type)
	{
	case ValueType::UINT8: program->AddPushConstantUInt8Command(value.GetUInt8()); break;
	case ValueType::UINT16: program->AddPushConstantUInt16Command(value.GetUInt16()); break;
	case ValueType::UINT32: program->AddPushConstantUInt32Command(value.GetUInt32()); break;
	case ValueType::UINT64: program->AddPushConstantUInt64Command(value.GetUInt64()); break;

	case ValueType::INT8: program->AddPushConstantInt8Command(value.GetInt8()); break;
	case ValueType::INT16: program->AddPushConstantInt16Command(value.GetInt16()); break;
	case ValueType::INT32: program->AddPushConstantInt32Command(value.GetInt32()); break;
	case ValueType::INT64: program->AddPushConstantInt64Command(value.GetInt64()); break;

	case ValueType::REAL32: program->AddPushConstantReal32Command(value.GetReal32()); break;
	case ValueType::REAL64: program->AddPushConstantReal64Command(value.GetReal64()); break;

	case ValueType::BOOL: program->AddPushConstantBoolCommand(value.GetBool()); break;
	case ValueType::CHAR: program->AddPushConstantCharCommand(value.GetChar()); break;
	case ValueType::STRING: program->AddPushConstantStringCommand(value.GetString(), scope); break;
	}
}

TypeInfo ASTExpressionLiteral::GetTypeInfo(Program* program)
{
	return TypeInfo(value.type, value.pointerLevel);
}

void ASTExpressionLiteral::CleanUp(Program* program)
{
	if (value.type == (uint16)ValueType::STRING)
		program->GetHeapAllocator()->Free(value.data);
}

void ASTExpressionModuleFunctionCall::EmitCode(Program* program)
{
	for (uint32 i = 0; i < argExprs.size(); i++)
	{
		argExprs[i]->EmitCode(program);
	}

	program->AddModuleFunctionCallCommand(moduleID, functionID, argExprs.size(), !isStatement);
}

TypeInfo ASTExpressionModuleFunctionCall::GetTypeInfo(Program* program)
{
	return Module::GetFunctionReturnInfo(moduleID, functionID);
}

void ASTExpressionModuleConstant::EmitCode(Program* program)
{
	if(!isStatement)
		program->AddModuleConstantCommand(moduleID, constantID);
}

TypeInfo ASTExpressionModuleConstant::GetTypeInfo(Program* program)
{
	return Module::GetConstantTypeInfo(moduleID, constantID);
}

void ASTExpressionBinary::EmitCode(Program* program)
{
	lhs->EmitCode(program);
	rhs->EmitCode(program);

	switch (op)
	{
	case ASTOperator::ADD: program->WriteOPCode(OpCode::ADD); break;
	case ASTOperator::MINUS: program->WriteOPCode(OpCode::SUBTRACT); break;
	case ASTOperator::MULTIPLY: program->WriteOPCode(OpCode::MULTIPLY); break;
	case ASTOperator::DIVIDE: program->WriteOPCode(OpCode::DIVIDE); break;
	}
}

TypeInfo ASTExpressionBinary::GetTypeInfo(Program* program)
{
	TypeInfo leftType = lhs->GetTypeInfo(program);
	TypeInfo rightType = rhs->GetTypeInfo(program);
	uint16 resultType = Value::PromoteType(leftType.type, rightType.type);
	return TypeInfo(resultType, 0);
}

void ASTExpressionDeclare::EmitCode(Program* program)
{
	if (assignExpr)
	{
		assignExpr->EmitCode(program);
	}
	else
	{
		switch ((ValueType)type)
		{
		case ValueType::UINT8: program->AddPushConstantUInt8Command(0); break;
		case ValueType::UINT16: program->AddPushConstantUInt16Command(0); break;
		case ValueType::UINT32: program->AddPushConstantUInt32Command(0); break;
		case ValueType::UINT64: program->AddPushConstantUInt64Command(0); break;
		case ValueType::INT8: program->AddPushConstantInt8Command(0); break;
		case ValueType::INT16: program->AddPushConstantInt16Command(0); break;
		case ValueType::INT32: program->AddPushConstantInt32Command(0); break;
		case ValueType::INT64: program->AddPushConstantInt64Command(0); break;
		case ValueType::REAL32: program->AddPushConstantReal32Command(0); break;
		case ValueType::REAL64: program->AddPushConstantReal64Command(0); break;
		case ValueType::CHAR: program->AddPushConstantCharCommand(0); break;
		case ValueType::BOOL: program->AddPushConstantBoolCommand(false); break;
		case ValueType::STRING: program->AddPushConstantStringCommand("", scope); break;
		}
	}

	program->AddDeclarePrimitiveCommand((ValueType)type, scope, variableID);
}

TypeInfo ASTExpressionDeclare::GetTypeInfo(Program* program)
{
	return program->GetScope(scope)->GetVariableTypeInfo(variableID);
}

void ASTExpressionVariable::EmitCode(Program* program)
{
	if(!isStatement)
		program->AddPushVariableCommand(scope, variableID);
}

TypeInfo ASTExpressionVariable::GetTypeInfo(Program* program)
{
	return program->GetScope(scope)->GetVariableTypeInfo(variableID);
}

void ASTExpressionDeclarePointer::EmitCode(Program* program)
{
	if (assignExpr)
	{
		assignExpr->EmitCode(program);
		program->AddDeclarePointerCommand(type, pointerLevel, scope, variableID);
	}
	else
	{
		program->AddDeclareNullPtrCommand(type, pointerLevel, scope, variableID);
	}
}

void ASTExpressionAddressOf::EmitCode(Program* program)
{
	expr->EmitCode(program);
	program->WriteOPCode(OpCode::ADDRESS_OF);
}

TypeInfo ASTExpressionAddressOf::GetTypeInfo(Program* program)
{
	TypeInfo exprType = expr->GetTypeInfo(program);
	return TypeInfo(exprType.type, exprType.pointerLevel + 1);
}

void ASTExpressionDereference::EmitCode(Program* program)
{
	expr->EmitCode(program);
	program->WriteOPCode(OpCode::DEREFERENCE);
}

TypeInfo ASTExpressionDereference::GetTypeInfo(Program* program)
{
	TypeInfo exprType = expr->GetTypeInfo(program);
	return TypeInfo(exprType.type, exprType.pointerLevel - 1);
}

void ASTExpressionVariableSet::EmitCode(Program* program)
{
	assignExpr->EmitCode(program);
	program->AddVariableSetCommand(scope, variableID);
}

TypeInfo ASTExpressionVariableSet::GetTypeInfo(Program* program)
{
	return program->GetScope(scope)->GetVariableTypeInfo(variableID);
}

void ASTExpressionStaticFunctionCall::EmitCode(Program* program)
{
	for (uint32 i = 0; i < argExprs.size(); i++)
	{
		argExprs[i]->EmitCode(program);
	}

	program->AddStaticFunctionCallCommand(classID, functionID, !isStatement);
}

TypeInfo ASTExpressionStaticFunctionCall::GetTypeInfo(Program* program)
{
	Class* cls = program->GetClass(classID);
	functionID = (functionID == INVALID_ID) ? cls->GetFunctionID(functionName, argExprs) : functionID;
	Function* function = cls->GetFunction(functionID);
	return function->returnInfo;
}

bool ASTExpressionStaticFunctionCall::Resolve(Program* program)
{
	if(functionID == INVALID_ID)
		functionID = program->GetClass(classID)->GetFunctionID(functionName, argExprs);

	return functionID != INVALID_ID;
}

void ASTExpressionReturn::EmitCode(Program* program)
{
	bool returnsValue = false;
	if (expr)
	{
		returnsValue = true;
		expr->EmitCode(program);
	}

	program->AddReturnCommand(returnsValue);
}

TypeInfo ASTExpressionReturn::GetTypeInfo(Program* program)
{
	return expr->GetTypeInfo(program);
}

void ASTExpressionStackArrayDeclare::EmitCode(Program* program)
{
	for (uint32 i = 0; i < initializeExprs.size(); i++)
	{
		initializeExprs[i]->EmitCode(program);
	}

	program->AddDeclareArrayCommand(type, pointerLevel, length, initializeExprs.size(), scope, variableID);
}

TypeInfo ASTExpressionStackArrayDeclare::GetTypeInfo(Program* program)
{
	return TypeInfo(type, 1);
}

void ASTExpressionIndex::EmitCode(Program* program)
{
	if (assignExpr)
		assignExpr->EmitCode(program);
	else
	{
		if (isStatement) return;
	}

	expr->EmitCode(program);
	indexExpr->EmitCode(program);
	if (assignExpr)
	{
		program->WriteOPCode(OpCode::INDEX_ASSIGN);
	}
	else
	{
		program->WriteOPCode(OpCode::INDEX);
	}
}

TypeInfo ASTExpressionIndex::GetTypeInfo(Program* program)
{
	TypeInfo exprTypeInfo = expr->GetTypeInfo(program);
	return TypeInfo(exprTypeInfo.type, exprTypeInfo.elementPointerLevel);
}

void ASTExpressionNewArray::EmitCode(Program* program)
{
	sizeExpr->EmitCode(program);
	program->AddNewArrayCommand(type, pointerLevel);
}

TypeInfo ASTExpressionNewArray::GetTypeInfo(Program* program)
{
	return TypeInfo(type, pointerLevel + 1);
}

void ASTExpressionDeclareObject::EmitCode(Program* program)
{
	for (uint32 i = 0; i < argExprs.size(); i++)
		argExprs[i]->EmitCode(program);

	program->AddDeclareObjectWithConstructorCommand(type, functionID, scope, variableID);
}

TypeInfo ASTExpressionDeclareObject::GetTypeInfo(Program* program)
{
	return TypeInfo(type, 0);
}

bool ASTExpressionDeclareObject::Resolve(Program* program)
{
	Class* cls = program->GetClass(type);
	functionID = cls->GetFunctionID(cls->GetName(), argExprs);
	return true;
}

void ASTExpressionDeclareObjectAssign::EmitCode(Program* program)
{
	assignExpr->EmitCode(program);
	program->AddDeclareObjectWithAssignCommand(type, scope, variableID);
}

TypeInfo ASTExpressionDeclareObjectAssign::GetTypeInfo(Program* program)
{
	return TypeInfo(type, 0);
}

void ASTExpressionMemberAccess::EmitCode(Program* program)
{
	if (!isStatement)
		program->AddPushMemberCommand(scope, variableID, offset, memberType, memberPointerLevel, arrayIndex);
}

TypeInfo ASTExpressionMemberAccess::GetTypeInfo(Program* program)
{
	return TypeInfo(memberType, memberPointerLevel);
}

void ASTExpressionMemberSet::EmitCode(Program* program)
{
	uint64 memberSize = program->GetTypeSize(memberType);
	assignExpr->EmitCode(program);
	program->AddMemberSetCommand(scope, variableID, offset, memberType, memberSize, memberPointerLevel, arrayIndex);
}

TypeInfo ASTExpressionMemberSet::GetTypeInfo(Program* program)
{
	return TypeInfo(memberType, memberPointerLevel);
}

void ASTExpressionMemberFunctionCall::EmitCode(Program* program)
{
	for (uint32 i = 0; i < argExprs.size(); i++)
		argExprs[i]->EmitCode(program);

	objExpr->EmitCode(program);
	TypeInfo objTypeInfo = objExpr->GetTypeInfo(program);
	program->AddMemberFunctionCallCommand(objTypeInfo.type, functionID, !isStatement);
}

TypeInfo ASTExpressionMemberFunctionCall::GetTypeInfo(Program* program)
{
	TypeInfo objTypeInfo = objExpr->GetTypeInfo(program);
	Class* objClass = program->GetClass(objTypeInfo.type);
	functionID = (functionID == INVALID_ID) ? objClass->GetFunctionID(functionName, argExprs) : functionID;
	return objClass->GetFunction(functionID)->returnInfo;
}

bool ASTExpressionMemberFunctionCall::Resolve(Program* program)
{
	TypeInfo objTypeInfo = objExpr->GetTypeInfo(program);
	Class* objClass = program->GetClass(objTypeInfo.type);
	functionID = (functionID == INVALID_ID) ? objClass->GetFunctionID(functionName, argExprs) : functionID;
	return true;
}

void ASTExpressionDirectMemberAccess::EmitCode(Program* program)
{
	if (assignExpr)
	{
		assignExpr->EmitCode(program);
		program->AddDirectMemberAssignCommand(offset, memberTypeInfo.type, memberTypeInfo.pointerLevel, program->GetTypeSize(memberTypeInfo.type));
	}
	else
	{
		program->AddDirectMemberAccessCommand(offset, memberTypeInfo.type, memberTypeInfo.pointerLevel);
	}
}

TypeInfo ASTExpressionDirectMemberAccess::GetTypeInfo(Program* program)
{
	return memberTypeInfo;
}
