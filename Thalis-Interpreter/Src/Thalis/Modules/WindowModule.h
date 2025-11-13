#pragma once

#include "../FunctionArg.h"
#include "../TypeInfo.h"
#include "../ID.h"
#include <vector>

enum class WindowModuleConstant
{
	CB_CREATE, CB_CLOSE, CB_RESIZE
};

enum class WindowModuleFunction
{
	CREATE, DESTROY,
	UPDATE, PRESENT,
	CHECK_FOR_EVENT,
	GET_SIZE
};

class Program;
class WindowModule
{
public:
	static bool Init();
	static Value CallFunction(Program* program, uint16 function, const std::vector<FunctionArg>& args);
	static Value Constant(Program* program, uint16 constant);

	static TypeInfo GetFunctionReturnInfo(uint16 function);
	static TypeInfo GetConstantTypeInfo(uint16 constant);
};