#pragma once

#include "../TypeInfo.h"

#define IO_MODULE_ID		1
#define MATH_MODULE_ID		2
#define WINDOW_MODULE_ID	3
#define OPENGL_MODULE_ID	4

class Module
{
public:
	static TypeInfo GetFunctionReturnInfo(ID moduleID, uint16 function);
	static TypeInfo GetConstantTypeInfo(ID moduleID, uint16 constant);
};