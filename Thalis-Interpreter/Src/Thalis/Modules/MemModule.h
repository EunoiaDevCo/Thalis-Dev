#pragma once

#include "../FunctionArg.h"
#include "../TypeInfo.h"
#include "../ID.h"
#include <vector>

enum class MemModuleConstant : uint16
{

};

enum class MemModuleFunction : uint16
{
	COPY, ALLOC
};

class Program;
class MemModule
{
public:
	static bool Init();
	static Value CallFunction(Program* program, uint16 function, const std::vector<FunctionArg>& args);
	static Value Constant(Program* program, uint16 constant);

	static TypeInfo GetFunctionReturnInfo(uint16 function);
	static TypeInfo GetConstantTypeInfo(uint16 constant);
};
