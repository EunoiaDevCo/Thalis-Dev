#include "Function.h"
#include "Program.h"
#include "ASTExpression.h"

std::string Function::GenerateSignature() const
{
	Program* program = Program::GetCompiledProgram();

	std::string signature = name + "-";

	for (uint32 i = 0; i < parameters.size(); i++)
	{
		const FunctionParameter& param = parameters[i];
		std::string typeString = program->GetTypeName(param.type.type);
		if (param.type.pointerLevel > 0)
			typeString += std::to_string(param.type.pointerLevel);

		signature += typeString;
		if (i + 1 < parameters.size())
			signature += "_";
	}

	return signature;
}

std::string Function::GenerateSignatureFromArgs(Program* program, const std::string& name, const std::vector<ASTExpression*>& args)
{
	std::string signature = name + "-";

	for (uint32 i = 0; i < args.size(); i++)
	{
		TypeInfo typeInfo = args[i]->GetTypeInfo(program);
		std::string typeName = program->GetTypeName(typeInfo.type);

		if (typeInfo.pointerLevel > 0)
			typeName += std::to_string(typeInfo.pointerLevel);

		signature += typeName;
		if (i + 1 < args.size())
			signature += "_";
	}

	return signature;
}