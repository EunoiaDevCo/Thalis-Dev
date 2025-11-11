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

ASTExpression* ASTExpressionLiteral::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	Value v = value;
	if (value.type == (uint16)ValueType::STRING)
		v = value.Clone(program, program->GetHeapAllocator());

	return new ASTExpressionLiteral(newScope, v);
}

TypeInfo ASTExpressionTemplateLiteral::GetTypeInfo(Program* program)
{
	return TypeInfo((uint16)ValueType::UINT32, 0);
}

ASTExpression* ASTExpressionTemplateLiteral::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	uint32 index = cls->InstantiateTemplateGetIndex(program, literalName);
	uint32 value = instantiation.args[index].value;
	return new ASTExpressionLiteral(newScope, Value::MakeUInt32(value, program->GetStackAllocator()));
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

ASTExpression* ASTExpressionModuleFunctionCall::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	std::vector<ASTExpression*> injectedArgExprs;
	for (uint32 i = 0; i < argExprs.size(); i++)
		injectedArgExprs.push_back(argExprs[i]->InjectTemplateType(program, cls, newScope, instantiation, templatedClass));

	return new ASTExpressionModuleFunctionCall(newScope, moduleID, functionID, injectedArgExprs);
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

ASTExpression* ASTExpressionModuleConstant::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	return new ASTExpressionModuleConstant(newScope, moduleID, constantID);
}

void ASTExpressionBinary::EmitCode(Program* program)
{
	if (isStatement) return;

	lhs->EmitCode(program);
	rhs->EmitCode(program);

	switch (op)
	{
	case ASTOperator::ADD: program->AddAddCommand(aritmaticFunctionID); break;
	case ASTOperator::MINUS: program->AddSubCommand(aritmaticFunctionID); break;
	case ASTOperator::MULTIPLY: program->AddMulCommand(aritmaticFunctionID); break;
	case ASTOperator::DIVIDE: program->AddDivCommand(aritmaticFunctionID); break;
	case ASTOperator::MOD: program->AddModCommand(aritmaticFunctionID); break;
	case ASTOperator::LESS: program->WriteOPCode(OpCode::LESS); break;
	case ASTOperator::GREATER: program->WriteOPCode(OpCode::GREATER); break;
	case ASTOperator::LESS_EQUALS: program->WriteOPCode(OpCode::LESS_EQUAL); break;
	case ASTOperator::GREATER_EQUALS: program->WriteOPCode(OpCode::GREATER_EQUAL); break;
	case ASTOperator::EQUALS: program->WriteOPCode(OpCode::EQUALS); break;
	case ASTOperator::NOT_EQUALS: program->WriteOPCode(OpCode::NOT_EQUALS); break;
	}
}

TypeInfo ASTExpressionBinary::GetTypeInfo(Program* program)
{
	TypeInfo leftType = lhs->GetTypeInfo(program);
	TypeInfo rightType = rhs->GetTypeInfo(program);
	uint16 resultType = Value::PromoteType(leftType.type, rightType.type);
	return TypeInfo(resultType, 0);
}

ASTExpression* ASTExpressionBinary::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	ASTExpression* lhsInjected = lhs->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	ASTExpression* rhsInjected = rhs->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);

	return new ASTExpressionBinary(newScope, lhsInjected, rhsInjected, op);
}

bool ASTExpressionBinary::Resolve(Program* program)
{
	TypeInfo lhsType = lhs->GetTypeInfo(program);
	if (Value::IsPrimitiveType(lhsType.type)) return true;

	Class* cls = program->GetClass(lhsType.type);
	std::vector<ASTExpression*> args;
	args.push_back(rhs);

	switch (op)
	{
	case ASTOperator::ADD:		aritmaticFunctionID = cls->GetFunctionID("operator+", args); break;
	case ASTOperator::MINUS:	aritmaticFunctionID = cls->GetFunctionID("operator-", args); break;
	case ASTOperator::MULTIPLY: aritmaticFunctionID = cls->GetFunctionID("operator*", args); break;
	case ASTOperator::DIVIDE:	aritmaticFunctionID = cls->GetFunctionID("operator/", args); break;
	case ASTOperator::MOD:		aritmaticFunctionID = cls->GetFunctionID("operator%", args); break;
	}
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

ASTExpression* ASTExpressionDeclare::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	uint16 injectedType = type;
	uint8 injectedPointerLevel = 0;
	if (type == (uint16)ValueType::TEMPLATE_TYPE)
	{
		uint32 index = cls->InstantiateTemplateGetIndex(program, templateTypeName);
		injectedType = instantiation.args[index].value;
		injectedPointerLevel = instantiation.args[index].pointerLevel;
	}

	ASTExpression* injectedAssignExpr = assignExpr ? assignExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass) : nullptr;

	if (injectedPointerLevel == 0)
		return new ASTExpressionDeclare(newScope, variableID, injectedType, injectedAssignExpr);
	else
		return new ASTExpressionDeclarePointer(newScope, variableID, injectedType, injectedPointerLevel, injectedAssignExpr);
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

ASTExpression* ASTExpressionVariable::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	return new ASTExpressionVariable(newScope, variableID);
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

ASTExpression* ASTExpressionDeclarePointer::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	uint16 injectedType = type;
	uint8 injectedPointerLevel = pointerLevel;
	if (type == (uint16)ValueType::TEMPLATE_TYPE)
	{
		uint32 index = cls->InstantiateTemplateGetIndex(program, templateTypeName);
		injectedType = instantiation.args[index].value;
		injectedPointerLevel += instantiation.args[index].pointerLevel;
	}

	ASTExpression* injectedAssignExpr = assignExpr ? assignExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass) : assignExpr;
	return new ASTExpressionDeclarePointer(newScope, variableID, injectedType, injectedPointerLevel, injectedAssignExpr);
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

ASTExpression* ASTExpressionAddressOf::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	ASTExpression* injectedExpr = expr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	return new ASTExpressionAddressOf(newScope, injectedExpr);
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

ASTExpression* ASTExpressionDereference::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	ASTExpression* injectedExpr = expr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	return new ASTExpressionDereference(newScope, injectedExpr);
}

void ASTExpressionVariableSet::EmitCode(Program* program)
{
	uint16 variableType = GetTypeInfo(program).type;
	assignExpr->EmitCode(program);
	program->AddVariableSetCommand(scope, variableID, assignFunctionID, variableType);
}

TypeInfo ASTExpressionVariableSet::GetTypeInfo(Program* program)
{
	return program->GetScope(scope)->GetVariableTypeInfo(variableID);
}

ASTExpression* ASTExpressionVariableSet::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	ASTExpression* injectedAssignExpr = assignExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	return new ASTExpressionVariableSet(newScope, variableID, injectedAssignExpr);
}

bool ASTExpressionVariableSet::Resolve(Program* program)
{
	TypeInfo varTypeInfo = program->GetScope(scope)->GetVariableTypeInfo(variableID);
	if (Value::IsPrimitiveType(varTypeInfo.type) || varTypeInfo.pointerLevel > 0) return true;

	Class* cls = program->GetClass(varTypeInfo.type);
	std::vector<ASTExpression*> args;
	args.push_back(assignExpr);
	assignFunctionID = cls->GetFunctionID("operator=", args);
	return true;
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

ASTExpression* ASTExpressionStaticFunctionCall::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	std::vector<ASTExpression*> injectedArgs;
	for (uint32 i = 0; i < argExprs.size(); i++)
		injectedArgs.push_back(argExprs[i]->InjectTemplateType(program, cls, newScope, instantiation, templatedClass));

	return new ASTExpressionStaticFunctionCall(newScope, classID, functionName, injectedArgs);
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

ASTExpression* ASTExpressionReturn::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	ASTExpression* injectedExpr = expr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	return new ASTExpressionReturn(newScope, injectedExpr);
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

ASTExpression* ASTExpressionStackArrayDeclare::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	uint16 injectedType = type;
	uint8 injectedPointerLevel = pointerLevel;
	if (type == (uint16)ValueType::TEMPLATE_TYPE)
	{
		uint32 index = cls->InstantiateTemplateGetIndex(program, templateTypeName);
		injectedType = instantiation.args[index].value;
		injectedPointerLevel += instantiation.args[index].pointerLevel;
	}

	std::vector<ASTExpression*> injectedInitializeExprs;
	for (uint32 i = 0; i < initializeExprs.size(); i++)
		injectedInitializeExprs.push_back(initializeExprs[i]->InjectTemplateType(program, cls, newScope, instantiation, templatedClass));

	return new ASTExpressionStackArrayDeclare(newScope, injectedType, injectedPointerLevel, variableID, length, injectedInitializeExprs);
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
		program->AddIndexAssignCommand(assignFunctionID);
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

ASTExpression* ASTExpressionIndex::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	ASTExpression* injectedExpr = expr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	ASTExpression* injectedIndexExpr = indexExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	ASTExpression* injectedAssignExpr = assignExpr ? assignExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass) : nullptr;

	return new ASTExpressionIndex(newScope, injectedExpr, injectedIndexExpr, injectedAssignExpr);
}

bool ASTExpressionIndex::Resolve(Program* program)
{
	TypeInfo typeInfo = GetTypeInfo(program);
	if (assignExpr == nullptr || Value::IsPrimitiveType(typeInfo.type) || typeInfo.pointerLevel > 0) return true;
	std::vector<ASTExpression*> args;
	args.push_back(assignExpr);
	assignFunctionID = program->GetClass(typeInfo.type)->GetFunctionID("operator=", args);
	return true;
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

ASTExpression* ASTExpressionNewArray::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	uint16 injectedType = type;
	uint8 injectedPointerLevel = pointerLevel;
	if (type == (uint16)ValueType::TEMPLATE_TYPE)
	{
		uint32 index = cls->InstantiateTemplateGetIndex(program, templateTypeName);
		injectedType = instantiation.args[index].value;
		injectedPointerLevel += instantiation.args[index].pointerLevel;
	}

	ASTExpression* injectedSizeExpr = sizeExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	return new ASTExpressionNewArray(newScope, injectedType, injectedPointerLevel, injectedSizeExpr);
}

void ASTExpressionNew::EmitCode(Program* program)
{
	if (isStatement) return;//TODO: Still call constructor but dont push value on stack

	for (uint32 i = 0; i < argExprs.size(); i++)
	{
		argExprs[i]->EmitCode(program);
	}

	program->AddNewCommand(type, functionID);
}

TypeInfo ASTExpressionNew::GetTypeInfo(Program* program)
{
	return TypeInfo(type, 1);
}

bool ASTExpressionNew::Resolve(Program* program)
{
	if(!Value::IsPrimitiveType(type))
	{
		Class* cls = program->GetClass(type);
		functionID = cls->GetFunctionID(cls->GetName(), argExprs);
	}

	return true;
}

ASTExpression* ASTExpressionNew::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	uint16 injectedType = type;
	if (type == (uint16)ValueType::TEMPLATE_TYPE)
	{
		uint32 index = cls->InstantiateTemplateGetIndex(program, templateTypeName);
		injectedType = instantiation.args[index].value;
	}

	std::vector<ASTExpression*> injectedArgExprs;
	for (uint32 i = 0; i < argExprs.size(); i++)
		injectedArgExprs.push_back(argExprs[i]->InjectTemplateType(program, cls, newScope, instantiation, templatedClass));

	return new ASTExpressionNew(newScope, injectedType, injectedArgExprs);
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

ASTExpression* ASTExpressionDeclareObject::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	uint16 injectedType = type;
	uint8 injectedPointerLevel = 0;
	if (type == (uint16)ValueType::TEMPLATE_TYPE)
	{
		uint32 index = cls->InstantiateTemplateGetIndex(program, templateTypeName);
		injectedType = instantiation.args[index].value;
		injectedPointerLevel = instantiation.args[index].pointerLevel;
	}

	if (injectedPointerLevel > 0)
		return new ASTExpressionDeclarePointer(newScope, variableID, injectedType, injectedPointerLevel, nullptr);

	std::vector<ASTExpression*> injectedArgExprs;
	for (uint32 i = 0; i < argExprs.size(); i++)
		injectedArgExprs.push_back(argExprs[i]->InjectTemplateType(program, cls, newScope, instantiation, templatedClass));

	return new ASTExpressionDeclareObject(newScope, injectedType, variableID, injectedArgExprs);
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

ASTExpression* ASTExpressionDeclareObjectAssign::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	uint16 injectedType = type;
	if (type == (uint16)ValueType::TEMPLATE_TYPE)
	{
		uint32 index = cls->InstantiateTemplateGetIndex(program, templateTypeName);
		injectedType = instantiation.args[index].value;
	}

	ASTExpression* injectedAssignExpr = assignExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	return new ASTExpressionDeclareObjectAssign(newScope, injectedType, variableID, injectedAssignExpr);
}

void ASTExpressionMemberAccess::EmitCode(Program* program)
{
	if (isStatement) return;

	if (indexExpr)
		indexExpr->EmitCode(program);

	program->AddPushMemberCommand(scope, variableID, offset, memberType, memberPointerLevel, indexExpr != nullptr);
}

TypeInfo ASTExpressionMemberAccess::GetTypeInfo(Program* program)
{
	return TypeInfo(memberType, memberPointerLevel);
}

ASTExpression* ASTExpressionMemberAccess::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	uint64 injectedOffset = offset;
	uint16 injectedMemberType = memberType;
	uint8 injectedMemberPointerLevel = memberPointerLevel;
	if (!members.empty())
	{
		TypeInfo injectedMemberTypeInfo;
		bool templated = false;
		injectedOffset = templatedClass->CalculateOffset(members, &injectedMemberTypeInfo, &templated);
		injectedMemberType = injectedMemberTypeInfo.type;
		injectedMemberPointerLevel = injectedMemberTypeInfo.pointerLevel;
	}

	ASTExpression* injectedIndexExpr = indexExpr ? indexExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass) : nullptr;
	return new ASTExpressionMemberAccess(newScope, variableID, injectedOffset, injectedMemberType, injectedMemberPointerLevel, injectedIndexExpr);
}

void ASTExpressionMemberSet::EmitCode(Program* program)
{
	uint64 memberSize = program->GetTypeSize(memberType);
	assignExpr->EmitCode(program);
	if (indexExpr)
		indexExpr->EmitCode(program);

	program->AddMemberSetCommand(scope, variableID, offset, memberType, memberSize, memberPointerLevel, indexExpr != nullptr, assignFunctionID);
}

TypeInfo ASTExpressionMemberSet::GetTypeInfo(Program* program)
{
	return TypeInfo(memberType, memberPointerLevel);
}

ASTExpression* ASTExpressionMemberSet::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	uint64 injectedOffset = offset;
	uint16 injectedMemberType = memberType;
	uint8 injectedMemberPointerLevel = memberPointerLevel;
	if (!members.empty())
	{
		TypeInfo injectedMemberTypeInfo;
		bool templated = false;
		injectedOffset = templatedClass->CalculateOffset(members, &injectedMemberTypeInfo, &templated);
		injectedMemberType = injectedMemberTypeInfo.type;
		injectedMemberPointerLevel = injectedMemberTypeInfo.pointerLevel;
	}

	ASTExpression* injectedAssignExpr = assignExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	ASTExpression* injectedIndexExpr = indexExpr ? indexExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass) : nullptr;
	return new ASTExpressionMemberSet(newScope, variableID, injectedOffset, injectedMemberType, injectedMemberPointerLevel, injectedAssignExpr, injectedIndexExpr);
}

bool ASTExpressionMemberSet::Resolve(Program* program)
{
	if (Value::IsPrimitiveType(memberType) || memberPointerLevel > 0)
		return true;

	std::vector<ASTExpression*> args;
	args.push_back(assignExpr);
	assignFunctionID = program->GetClass(memberType)->GetFunctionID("operator=", args);
	return true;
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

ASTExpression* ASTExpressionMemberFunctionCall::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	ASTExpression* injectedObjExpr = objExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	std::vector<ASTExpression*> injectedArgExprs;
	for (uint32 i = 0; i < argExprs.size(); i++)
		injectedArgExprs.push_back(argExprs[i]->InjectTemplateType(program, cls, newScope, instantiation, templatedClass));

	return new ASTExpressionMemberFunctionCall(newScope, injectedObjExpr, functionName, injectedArgExprs);
}

void ASTExpressionDirectMemberAccess::EmitCode(Program* program)
{
	if (assignExpr)
	{
		assignExpr->EmitCode(program);
		program->AddDirectMemberAssignCommand(offset, memberTypeInfo.type, memberTypeInfo.pointerLevel, program->GetTypeSize(memberTypeInfo.type), assignFunctionID);
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

ASTExpression* ASTExpressionDirectMemberAccess::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	uint64 injectedOffset = offset;
	TypeInfo injectedTypeInfo = memberTypeInfo;
	if (!members.empty())
	{
		bool dummy = false;
		injectedOffset = templatedClass->CalculateOffset(members, &injectedTypeInfo, &dummy);
	}

	ASTExpression* injectedAssignExpr = assignExpr ? assignExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass) : nullptr;
	return new ASTExpressionDirectMemberAccess(newScope, injectedTypeInfo, injectedOffset, injectedAssignExpr);
}

bool ASTExpressionDirectMemberAccess::Resolve(Program* program)
{
	if (assignExpr == nullptr || Value::IsPrimitiveType(memberTypeInfo.type) || memberTypeInfo.pointerLevel > 0)
		return true;
	std::vector<ASTExpression*> args;
	args.push_back(assignExpr);
	assignFunctionID = program->GetClass(memberTypeInfo.type)->GetFunctionID("operator=", args);
	return true;
}

void ASTExpressionThis::EmitCode(Program* program)
{
	if (!isStatement)
		program->WriteOPCode(OpCode::PUSH_THIS);
}

TypeInfo ASTExpressionThis::GetTypeInfo(Program* program)
{
	return TypeInfo(thisType, 0);
}

ASTExpression* ASTExpressionThis::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	return new ASTExpressionThis(newScope, thisType);
}

void ASTExpressionIfElse::EmitCode(Program* program)
{
	// Emit condition first
	conditionExpr->EmitCode(program);

	// Reserve jump for false condition (we’ll patch offset later);
	program->WriteOPCode(OpCode::JUMP_IF_FALSE);
	uint32 jumpIfFalsePos = program->GetCodeSize();
	program->WriteUInt32(0); // placeholder for else jump target

	if (ifScope != INVALID_ID)
		program->AddPushScopeCommand(ifScope);

	// Emit "if" block
	for (uint32 i = 0; i < ifExprs.size(); i++)
		ifExprs[i]->EmitCode(program);

	if (ifScope != INVALID_ID)
		program->AddPopScopeCommand();

	// Reserve jump over else body
	program->WriteOPCode(OpCode::JUMP);
	uint32 jumpToEndPos = program->GetCodeSize();
	program->WriteUInt32(0); // placeholder for end jump target

	// Compute else label position (current code position)
	uint32 elseLabelPos = program->GetCodeSize();

	// Patch first jump (false -> else start)
	program->PatchUInt32(jumpIfFalsePos, elseLabelPos);

	if (elseScope != INVALID_ID)
		program->AddPushScopeCommand(elseScope);

	// Emit "else" block
	for (uint32 i = 0; i < elseExprs.size(); i++)
		elseExprs[i]->EmitCode(program);

	if (elseScope != INVALID_ID)
		program->AddPopScopeCommand();

	// Compute end label position (after else)
	uint32 endLabelPos = program->GetCodeSize();

	// Patch jump over else
	program->PatchUInt32(jumpToEndPos, endLabelPos);
}

TypeInfo ASTExpressionIfElse::GetTypeInfo(Program* program)
{
	return TypeInfo(INVALID_ID, 0);
}

ASTExpression* ASTExpressionIfElse::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	ASTExpression* injectedConditionExpr = conditionExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	std::vector<ASTExpression*> injectedIfBody;
	std::vector<ASTExpression*> injectedElseBody;
	ID newIfScope = ifScope;
	ID newElseScope = elseScope;
	if (newIfScope != INVALID_ID) newIfScope = program->CreateScope(newScope);
	if (newElseScope != INVALID_ID) newElseScope = program->CreateScope(newScope);

	ID ifs = (newIfScope == INVALID_ID) ? newScope : newIfScope;
	ID elses = (newElseScope == INVALID_ID) ? newScope : newElseScope;

	for (uint32 i = 0; i < ifExprs.size(); i++)
		injectedIfBody.push_back(ifExprs[i]->InjectTemplateType(program, cls, ifs, instantiation, templatedClass));

	for (uint32 i = 0; i < elseExprs.size(); i++)
		injectedElseBody.push_back(elseExprs[i]->InjectTemplateType(program, cls, elses, instantiation, templatedClass));

	return new ASTExpressionIfElse(newScope, newIfScope, newElseScope, injectedConditionExpr, injectedIfBody, injectedElseBody);
}

void ASTExpressionFor::EmitCode(Program* program)
{
	if (declareExpr)
		declareExpr->EmitCode(program);

	uint32 pushLoopPos = program->AddPushLoopCommand();
	uint32 conditionPos = program->GetCodeSize();
	program->AddPushScopeCommand(forScope);

	if (conditionExpr)
		conditionExpr->EmitCode(program);
	else
	{
		program->AddPushConstantBoolCommand(true);
	}

	program->WriteOPCode(OpCode::JUMP_IF_FALSE);
	uint32 jumpIfFalsePos = program->GetCodeSize();
	program->WriteUInt32(0);

	for (uint32 i = 0; i < forExprs.size(); i++)
		forExprs[i]->EmitCode(program);

	program->AddPopScopeCommand();

	uint32 incrPC = program->GetCodeSize();
	if (incrExpr)
		incrExpr->EmitCode(program);

	program->WriteOPCode(OpCode::JUMP);
	program->WriteUInt32(conditionPos);
	uint32 loopEndPos = program->GetCodeSize();
	program->AddPopLoopCommand();

	program->PatchUInt32(jumpIfFalsePos, loopEndPos);
	program->PatchPushLoopCommand(pushLoopPos, incrPC, loopEndPos);
}

TypeInfo ASTExpressionFor::GetTypeInfo(Program* program)
{
	return TypeInfo(INVALID_ID, 0);
}

ASTExpression* ASTExpressionFor::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	ID newBodyScope = forScope;
	if (newBodyScope != INVALID_ID) newBodyScope = program->CreateScope(newScope);

	ASTExpression* injectedDeclareExpr = declareExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	ASTExpression* injectedConditionExpr = conditionExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	ASTExpression* injectedIncrExpr = incrExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	std::vector<ASTExpression*> injectedBody;

	ID fors = (newBodyScope == INVALID_ID) ? newScope : newBodyScope;
	for (uint32 i = 0; i < forExprs.size(); i++)
		injectedBody.push_back(forExprs[i]->InjectTemplateType(program, cls, fors, instantiation, templatedClass));

	return new ASTExpressionFor(newScope, newBodyScope, injectedDeclareExpr, injectedConditionExpr, injectedIncrExpr, injectedBody);
}

void ASTExpressionUnaryUpdate::EmitCode(Program* program)
{
	expr->EmitCode(program); //pushes variable/member/etc onto stack
	program->AddUnaryUpdateCommand((uint8)op, !isStatement);
}

TypeInfo ASTExpressionUnaryUpdate::GetTypeInfo(Program* program)
{
	return expr->GetTypeInfo(program);
}

ASTExpression* ASTExpressionUnaryUpdate::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	ASTExpression* injectedExpr = expr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	return new ASTExpressionUnaryUpdate(newScope, injectedExpr, op);
}

void ASTExpressionWhile::EmitCode(Program* program)
{
	uint32 pushLoopPos = program->AddPushLoopCommand();
	uint32 conditionPos = program->GetCodeSize();
	program->AddPushScopeCommand(whileScope);
	conditionExpr->EmitCode(program);

	program->WriteOPCode(OpCode::JUMP_IF_FALSE);
	uint32 jumpIfFalsePos = program->GetCodeSize();
	program->WriteUInt32(0);

	for (uint32 i = 0; i < whileExprs.size(); i++)
		whileExprs[i]->EmitCode(program);

	program->AddPopScopeCommand();
	program->WriteOPCode(OpCode::JUMP);
	program->WriteUInt32(conditionPos);
	uint32 loopEndPos = program->GetCodeSize();
	program->AddPopLoopCommand();

	program->PatchUInt32(jumpIfFalsePos, loopEndPos);
}

TypeInfo ASTExpressionWhile::GetTypeInfo(Program* program)
{
	return TypeInfo(INVALID_ID, 0);
}

ASTExpression* ASTExpressionWhile::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	ID newBodyScope = whileScope;
	if (newBodyScope != INVALID_ID) newBodyScope = program->CreateScope(newScope);

	ASTExpression* injectedConditionExpr = conditionExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	std::vector<ASTExpression*> injectedBody;

	ID whiles = (newBodyScope == INVALID_ID) ? newScope : newBodyScope;
	for (uint32 i = 0; i < whileExprs.size(); i++)
		injectedBody.push_back(whileExprs[i]->InjectTemplateType(program, cls, whiles, instantiation, templatedClass));

	return new ASTExpressionWhile(newScope, newBodyScope, injectedConditionExpr, injectedBody);
}

void ASTExpressionConstructorCall::EmitCode(Program* program)
{
	if (isStatement)
		return;

	for (uint32 i = 0; i < argExprs.size(); i++)
		argExprs[i]->EmitCode(program);

	program->AddConstructorCallCommand(type, functionID);
}

TypeInfo ASTExpressionConstructorCall::GetTypeInfo(Program* program)
{
	return TypeInfo(type, 0);
}

bool ASTExpressionConstructorCall::Resolve(Program* program)
{
	Class* cls = program->GetClass(type);
	functionID = cls->GetFunctionID(cls->GetName(), argExprs);
	return functionID != INVALID_ID;
}

ASTExpression* ASTExpressionConstructorCall::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	uint16 injectedType = type;
	if (type == (uint16)ValueType::TEMPLATE_TYPE)
	{
		uint32 index = cls->InstantiateTemplateGetIndex(program, templateTypeName);
		injectedType = instantiation.args[index].value;
	}

	std::vector<ASTExpression*> injectedArgExprs;
	for (uint32 i = 0; i < argExprs.size(); i++)
		injectedArgExprs.push_back(argExprs[i]->InjectTemplateType(program, cls, newScope, instantiation, templatedClass));

	return new ASTExpressionConstructorCall(newScope, injectedType, injectedArgExprs);
}

void ASTExpressionBreak::EmitCode(Program* program)
{
	program->WriteOPCode(OpCode::BREAK);
}

TypeInfo ASTExpressionBreak::GetTypeInfo(Program* program)
{
	return TypeInfo(INVALID_ID, 0);
}

ASTExpression* ASTExpressionBreak::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	return new ASTExpressionBreak(newScope);
}

void ASTExpressionContinue::EmitCode(Program* program)
{
	program->WriteOPCode(OpCode::CONTINUE);
}

TypeInfo ASTExpressionContinue::GetTypeInfo(Program* program)
{
	return TypeInfo(INVALID_ID, 0);
}

ASTExpression* ASTExpressionContinue::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	return new ASTExpressionContinue(newScope);
}

void ASTExpressionDelete::EmitCode(Program* program)
{
	expr->EmitCode(program);
	if (deleteArray)
		program->WriteOPCode(OpCode::DELETE_ARRAY);
	else
		program->WriteOPCode(OpCode::DELETE);
}

TypeInfo ASTExpressionDelete::GetTypeInfo(Program* program)
{
	return TypeInfo(INVALID_ID, 0);
}

ASTExpression* ASTExpressionDelete::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	ASTExpression* injectedExpr = expr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	return new ASTExpressionDelete(newScope, injectedExpr, deleteArray);
}

void ASTExpressionVirtualFunctionCall::EmitCode(Program* program)
{
	for (uint32 i = 0; i < argExprs.size(); i++)
		argExprs[i]->EmitCode(program);

	objExpr->EmitCode(program);
	program->AddVirtualFunctionCallCommand(virtualFunctionID, !isStatement);
}

TypeInfo ASTExpressionVirtualFunctionCall::GetTypeInfo(Program* program)
{
	TypeInfo objTypeInfo = objExpr->GetTypeInfo(program);
	Class* objClass = program->GetClass(objTypeInfo.type);
	std::vector<TypeInfo> parameters;
	for (uint32 i = 0; i < argExprs.size(); i++)
		parameters.push_back(argExprs[i]->GetTypeInfo(program));

	virtualFunctionID = objClass->GetVTable()->FindSlot(functionName, parameters);
	return objClass->GetVTable()->GetFunction(virtualFunctionID)->returnInfo;
}

bool ASTExpressionVirtualFunctionCall::Resolve(Program* program)
{
	TypeInfo objTypeInfo = objExpr->GetTypeInfo(program);
	Class* objClass = program->GetClass(objTypeInfo.type);
	std::vector<TypeInfo> parameters;
	for (uint32 i = 0; i < argExprs.size(); i++)
		parameters.push_back(argExprs[i]->GetTypeInfo(program));

	virtualFunctionID = objClass->GetVTable()->FindSlot(functionName, parameters);
	return true;
}

ASTExpression* ASTExpressionVirtualFunctionCall::InjectTemplateType(Program* program, Class* cls, ID newScope, const TemplateInstantiation& instantiation, Class* templatedClass)
{
	ASTExpression* injectedObjExpr = objExpr->InjectTemplateType(program, cls, newScope, instantiation, templatedClass);
	std::vector<ASTExpression*> injectedArgExprs;
	for (uint32 i = 0; i < argExprs.size(); i++)
		injectedArgExprs.push_back(argExprs[i]->InjectTemplateType(program, cls, newScope, instantiation, templatedClass));

	return new ASTExpressionVirtualFunctionCall(newScope, injectedObjExpr, functionName, injectedArgExprs);
}
