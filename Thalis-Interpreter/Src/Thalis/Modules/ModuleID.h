#pragma once

#include "../TypeInfo.h"

#define IO_MODULE_ID	1
#define MATH_MODULE_ID	2

class Module
{
public:
	static TypeInfo GetFunctionReturnInfo(ID moduleID, uint16 function);
	static TypeInfo GetConstantTypeInfo(ID moduleID, uint16 constant);
};