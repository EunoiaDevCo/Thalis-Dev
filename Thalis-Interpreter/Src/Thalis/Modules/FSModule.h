#pragma once

#include "../FunctionArg.h"
#include "../TypeInfo.h"
#include "../ID.h"
#include <vector>

enum class FSModuleConstant
{

};

enum class FSModuleFunction
{
	READ_TEXT_FILE, READ_BINARY_FILE,
	OPEN_FILE, CLOSE_FILE,
	READ_LINE
};

class Program;
class FSModule
{
public:
	static bool Init(Program* program);
	static Value CallFunction(Program* program, uint16 function, const std::vector<FunctionArg>& args);
	static Value Constant(Program* program, uint16 constant);

	static TypeInfo GetFunctionReturnInfo(uint16 function);
	static TypeInfo GetConstantTypeInfo(uint16 constant);
};