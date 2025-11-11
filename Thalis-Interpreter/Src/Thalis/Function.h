#pragma once

#include "TypeInfo.h"
#include <string>
#include <vector>

enum class AccessModifier : uint32
{
	PUBLIC, PRIVATE
};

struct FunctionParameter
{
	TypeInfo type;
	ID variableID;
	std::string templateTypeName;
	bool isReference;
};

struct ASTExpression;
class Program;
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
