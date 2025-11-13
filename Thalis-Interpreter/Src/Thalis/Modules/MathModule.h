#pragma once

#include "../FunctionArg.h"
#include "../TypeInfo.h"
#include "../ID.h"
#include <vector>

enum class MathModuleConstant
{
	PI, E, TAU
};

enum class MathModuleFunction
{
	COS, SIN, TAN, ACOS, ASIN, ATAN, ATAN2,
	COSH, SINH, TANH, ACOSH, ASINH, ATANH,
	DEGTORAD, RADTODEG,
	FLOOR, CEIL, ROUND,
	MIN, MAX, CLAMP, LERP,
	ABS, SQRT, POW, EXP, LOG, LOG10, LOG2,
	MODF, MOD
};

class Program;
class MathModule
{
public:
	static bool Init();
	static Value CallFunction(Program* program, uint16 function, const std::vector<FunctionArg>& args);
	static Value Constant(Program* program, uint16 constant);

	static TypeInfo GetFunctionReturnInfo(uint16 function);
	static TypeInfo GetConstantTypeInfo(uint16 constant);
};