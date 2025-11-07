#pragma once

#include "Value.h"
#include "ID.h"
#include "TypeInfo.h"
#include <vector>
#include <new>

enum class ASTOperator
{
	ADD, MINUS, MULTIPLY, DIVIDE, MOD,
	EQUALS, NOT_EQUALS, LESS, GREATER, LESS_EQUALS, GREATER_EQUALS,
	LOGICAL_AND, LOGICAL_OR,
	BITWISE_OR, BITWISE_AND, BITSHIFT_LEFT, BITSHIFT_RIGHT
};

class Program;
struct ASTExpression
{
	void* operator new(std::size_t size);
	void operator delete(void* ptr) noexcept;

	ASTExpression(ID scope) : scope(scope), isStatement(false) {}
	virtual void EmitCode(Program* program) = 0;
	virtual TypeInfo GetTypeInfo(Program* program) = 0;
	virtual bool Resolve(Program* program) { return true; }
	virtual void CleanUp(Program* program) {};

	ID scope;
	bool isStatement;
};

struct ASTExpressionLiteral : public ASTExpression
{
	Value value;

	ASTExpressionLiteral(ID scope, const Value& value) :
		ASTExpression(scope), value(value) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
	virtual void CleanUp(Program* program) override;
};

struct ASTExpressionModuleFunctionCall : public ASTExpression
{
	ID moduleID;
	uint16 functionID;
	std::vector<ASTExpression*> argExprs;

	ASTExpressionModuleFunctionCall(ID scope, ID moduleID, uint16 functionID, const std::vector<ASTExpression*> argExprs) :
		ASTExpression(scope), moduleID(moduleID), functionID(functionID), argExprs(argExprs) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionModuleConstant : public ASTExpression
{
	ID moduleID;
	uint16 constantID;

	ASTExpressionModuleConstant(ID scope, ID moduleID, uint16 constantID) :
		ASTExpression(scope), moduleID(moduleID), constantID(constantID) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionBinary : public ASTExpression
{
	ASTExpression* lhs;
	ASTExpression* rhs;
	ASTOperator op;

	ASTExpressionBinary(ID scope, ASTExpression* lhs, ASTExpression* rhs, ASTOperator op) :
		ASTExpression(scope), lhs(lhs), rhs(rhs), op(op) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionDeclare : public ASTExpression
{
	ID variableID;
	uint16 type;
	ASTExpression* assignExpr;

	ASTExpressionDeclare(ID scope, ID variableID, uint16 type, ASTExpression* assignExpr = nullptr) :
		ASTExpression(scope), variableID(variableID), type(type), assignExpr(assignExpr) {
	}

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionVariable : public ASTExpression
{
	ID variableID;

	ASTExpressionVariable(ID scope, ID variableID) :
		ASTExpression(scope), variableID(variableID) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionDeclarePointer : public ASTExpression
{
	ID variableID;
	uint16 type;
	uint8 pointerLevel;
	ASTExpression* assignExpr;

	ASTExpressionDeclarePointer(ID scope, ID variableID, uint16 type, uint8 pointerLevel, ASTExpression* assignExpr = nullptr) :
		ASTExpression(scope), variableID(variableID), type(type), pointerLevel(pointerLevel), assignExpr(assignExpr) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override { return TypeInfo(type, pointerLevel); }
};

struct ASTExpressionAddressOf : public ASTExpression
{
	ASTExpression* expr;

	ASTExpressionAddressOf(ID scope, ASTExpression* expr) :
		ASTExpression(scope), expr(expr) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionDereference : public ASTExpression
{
	ASTExpression* expr;

	ASTExpressionDereference(ID scope, ASTExpression* expr) :
		ASTExpression(scope), expr(expr) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionVariableSet : public ASTExpression
{
	ID variableID;
	ASTExpression* assignExpr;

	ASTExpressionVariableSet(ID scope, ID variableID, ASTExpression* assignExpr) :
		ASTExpression(scope), variableID(variableID), assignExpr(assignExpr) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionStaticFunctionCall : public ASTExpression
{
	ID classID;
	ID functionID;
	std::string functionName;
	std::vector<ASTExpression*> argExprs;

	ASTExpressionStaticFunctionCall(ID scope, ID classID, const std::string& functionName, const std::vector<ASTExpression*>& argExprs) :
		ASTExpression(scope), classID(classID), functionID(INVALID_ID), functionName(functionName), argExprs(argExprs) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
	virtual bool Resolve(Program* program) override;
};

struct ASTExpressionReturn : public ASTExpression
{
	ASTExpression* expr;

	ASTExpressionReturn(ID scope, ASTExpression* expr = nullptr) :
		ASTExpression(scope), expr(expr) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionStackArrayDeclare : public ASTExpression
{
	uint16 type;
	uint8 pointerLevel;
	ID variableID;
	uint32 length;
	std::vector<ASTExpression*> initializeExprs;

	ASTExpressionStackArrayDeclare(ID scope, uint16 type, uint8 pointerLevel, ID variableID, uint32 length,
		const std::vector<ASTExpression*>& initializeExprs) :
		ASTExpression(scope), type(type), pointerLevel(pointerLevel), variableID(variableID), length(length), initializeExprs(initializeExprs) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionIndex : public ASTExpression
{
	ASTExpression* expr;
	ASTExpression* indexExpr;
	ASTExpression* assignExpr;

	ASTExpressionIndex(ID scope, ASTExpression* expr, ASTExpression* indexExpr, ASTExpression* assignExpr = nullptr) :
		ASTExpression(scope), expr(expr), indexExpr(indexExpr), assignExpr(assignExpr) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionNewArray : public ASTExpression
{
	uint16 type;
	uint8 pointerLevel;
	ASTExpression* sizeExpr;

	ASTExpressionNewArray(ID scope, uint16 type, uint8 pointerLevel, ASTExpression* sizeExpr) :
		ASTExpression(scope), type(type), pointerLevel(pointerLevel), sizeExpr(sizeExpr) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionDeclareObject : public ASTExpression
{
	uint16 type;
	ID variableID;
	std::vector<ASTExpression*> argExprs;
	ID functionID;

	ASTExpressionDeclareObject(ID scope, uint16 type, ID variableID, const std::vector<ASTExpression*> argExprs) :
		ASTExpression(scope), type(type), variableID(variableID), argExprs(argExprs), functionID(INVALID_ID) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
	virtual bool Resolve(Program* program) override;
};

struct ASTExpressionDeclareObjectAssign : public ASTExpression
{
	uint16 type;
	ID variableID;
	ASTExpression* assignExpr;

	ASTExpressionDeclareObjectAssign(ID scope, uint16 type, ID variableID, ASTExpression* assignExpr) :
		ASTExpression(scope), type(type), variableID(variableID), assignExpr(assignExpr) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionMemberAccess : public ASTExpression
{
	ID variableID;
	uint64 offset;
	uint16 memberType;
	uint8 memberPointerLevel;
	ASTExpression* indexExpr;

	ASTExpressionMemberAccess(ID scope, ID variableID, uint64 offset, uint16 memberType, uint8 memberPointerLevel, ASTExpression* indexExpr = nullptr) :
		ASTExpression(scope), variableID(variableID), offset(offset), memberType(memberType), memberPointerLevel(memberPointerLevel), indexExpr(indexExpr) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionMemberSet : public ASTExpression
{
	ID variableID;
	uint64 offset;
	uint16 memberType;
	uint8 memberPointerLevel;
	ASTExpression* assignExpr;
	ASTExpression* indexExpr;

	ASTExpressionMemberSet(ID scope, ID variableID, uint64 offset, uint16 memberType, uint8 memberPointerLevel, ASTExpression* assignExpr, ASTExpression* indexExpr = nullptr) :
		ASTExpression(scope), variableID(variableID), offset(offset), memberType(memberType), memberPointerLevel(memberPointerLevel),
		assignExpr(assignExpr), indexExpr(indexExpr) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionMemberFunctionCall : public ASTExpression
{
	ASTExpression* objExpr;
	std::string functionName;
	std::vector<ASTExpression*> argExprs;
	uint16 functionID;

	ASTExpressionMemberFunctionCall(ID scope, ASTExpression* objExpr, const std::string& functionName, const std::vector<ASTExpression*> argExprs) :
		ASTExpression(scope), objExpr(objExpr), functionName(functionName), argExprs(argExprs), functionID(INVALID_ID) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
	virtual bool Resolve(Program* program) override;
};

struct ASTExpressionDirectMemberAccess : public ASTExpression
{
	TypeInfo memberTypeInfo;
	uint64 offset;
	ASTExpression* assignExpr;

	ASTExpressionDirectMemberAccess(ID scope, const TypeInfo& memberTypeInfo, uint64 offset, ASTExpression* assignExpr = nullptr) :
		ASTExpression(scope), memberTypeInfo(memberTypeInfo), offset(offset), assignExpr(assignExpr) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionThis : public ASTExpression
{
	uint16 thisType;

	ASTExpressionThis(ID scope, uint16 thisType) :
		ASTExpression(scope), thisType(thisType) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionIfElse : public ASTExpression
{
	ID ifScope;
	ID elseScope;
	ASTExpression* conditionExpr;
	std::vector<ASTExpression*> ifExprs;
	std::vector<ASTExpression*> elseExprs;

	ASTExpressionIfElse(ID scope, ID ifScope, ID elseScope, ASTExpression* conditionExpr, const std::vector<ASTExpression*> ifExprs, const std::vector<ASTExpression*> elseExprs) :
		ASTExpression(scope), ifScope(ifScope), elseScope(elseScope), conditionExpr(conditionExpr), ifExprs(ifExprs), elseExprs(elseExprs) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionFor : public ASTExpression
{
	ID forScope;
	ASTExpression* declareExpr;
	ASTExpression* conditionExpr;
	ASTExpression* incrExpr;
	std::vector<ASTExpression*> forExprs;

	ASTExpressionFor(ID scope, ID forScope, ASTExpression* declareExpr, ASTExpression* conditionExpr, ASTExpression* incrExpr, const std::vector<ASTExpression*> forExprs) :
		ASTExpression(scope), forScope(forScope), declareExpr(declareExpr), conditionExpr(conditionExpr), incrExpr(incrExpr), forExprs(forExprs) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

enum class ASTUnaryUpdateOp
{
	PRE_INC,
	PRE_DEC,
	POST_INC,
	POST_DEC
};

struct ASTExpressionUnaryUpdate : public ASTExpression
{
	ASTExpression* expr;
	ASTUnaryUpdateOp op;

	ASTExpressionUnaryUpdate(ID scope, ASTExpression* expr, ASTUnaryUpdateOp op) :
		ASTExpression(scope), expr(expr), op(op) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};

struct ASTExpressionWhile : public ASTExpression
{
	ID whileScope;
	ASTExpression* conditionExpr;
	std::vector<ASTExpression*> whileExprs;

	ASTExpressionWhile(ID scope, ID whileScope, ASTExpression* conditionExpr, const std::vector<ASTExpression*>& whileExprs) :
		ASTExpression(scope), whileScope(whileScope), conditionExpr(conditionExpr), whileExprs(whileExprs) { }

	virtual void EmitCode(Program* program) override;
	virtual TypeInfo GetTypeInfo(Program* program) override;
};