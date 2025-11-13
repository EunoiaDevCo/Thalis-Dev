#include "ModuleID.h"

#include "IOModule.h"
#include "MathModule.h"
#include "WindowModule.h"
#include "GLModule.h"

TypeInfo Module::GetFunctionReturnInfo(ID moduleID, uint16 function)
{
    switch (moduleID)
    {
    case IO_MODULE_ID: return IOModule::GetFunctionReturnInfo(function);
    case MATH_MODULE_ID: return MathModule::GetFunctionReturnInfo(function);
    case WINDOW_MODULE_ID: return WindowModule::GetFunctionReturnInfo(function);
    case OPENGL_MODULE_ID: return GLModule::GetFunctionReturnInfo(function);
    }

    return TypeInfo(INVALID_ID, 0);
}

TypeInfo Module::GetConstantTypeInfo(ID moduleID, uint16 constant)
{
    switch (moduleID)
    {
    case IO_MODULE_ID: return IOModule::GetConstantTypeInfo(constant);
    case MATH_MODULE_ID: return MathModule::GetConstantTypeInfo(constant);
    case WINDOW_MODULE_ID: return WindowModule::GetConstantTypeInfo(constant);
    case OPENGL_MODULE_ID: return GLModule::GetConstantTypeInfo(constant);
    }

    return TypeInfo(INVALID_ID, 0);
}
