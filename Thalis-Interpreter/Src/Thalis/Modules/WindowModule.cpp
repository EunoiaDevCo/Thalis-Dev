#include "WindowModule.h"
#include "../Program.h"

#include "../Window.h"

bool WindowModule::Init()
{
    return true;
}

Value WindowModule::CallFunction(Program* program, uint16 function, const std::vector<FunctionArg>& args)
{
    switch ((WindowModuleFunction)function)
    {
    case WindowModuleFunction::CREATE: return Value::MakeUInt32(Window::TLSCreateWindow(args[0].value.GetString(), args[1].value.GetUInt32(), args[2].value.GetUInt32()), program->GetStackAllocator());
    case WindowModuleFunction::DESTROY: Window::GetWindow(args[0].value.GetUInt32())->Destroy(); break;
    case WindowModuleFunction::UPDATE: Window::GetWindow(args[0].value.GetUInt32())->Update(); break;
    case WindowModuleFunction::PRESENT: Window::GetWindow(args[0].value.GetUInt32())->Present(); break;
    case WindowModuleFunction::CHECK_FOR_EVENT: return Value::MakeBool(Window::GetWindow(args[0].value.GetUInt32())->CheckForEvent((WindowEventType)args[1].value.GetUInt32()), program->GetStackAllocator());
    case WindowModuleFunction::GET_SIZE: {

        Window* window = Window::GetWindow(args[0].value.GetUInt32());
        Value widthOut = args[1].value;
        Value heightOut = args[2].value;

        uint32 width, height;
        window->GetSize(&width, &height);

        *(uint32*)widthOut.data = width;
        *(uint32*)heightOut.data = height;
        return Value::MakeNULL();
    }
    }

    return Value::MakeNULL();
}

Value WindowModule::Constant(Program* program, uint16 constant)
{
    switch ((WindowModuleConstant)constant)
    {
    case WindowModuleConstant::CB_CREATE: return Value::MakeUInt32((uint32)WindowEventType::CREATE, program->GetStackAllocator());
    case WindowModuleConstant::CB_CLOSE: return Value::MakeUInt32((uint32)WindowEventType::CLOSE, program->GetStackAllocator());
    case WindowModuleConstant::CB_RESIZE: return Value::MakeUInt32((uint32)WindowEventType::RESIZE, program->GetStackAllocator());
    }

    return Value::MakeNULL();
}

TypeInfo WindowModule::GetFunctionReturnInfo(uint16 function)
{
    switch ((WindowModuleFunction)function)
    {
    case WindowModuleFunction::CREATE: return { (uint16)ValueType::UINT32, 0 };
    case WindowModuleFunction::DESTROY: return { INVALID_ID, 0 };
    case WindowModuleFunction::UPDATE: return { INVALID_ID, 0 };
    case WindowModuleFunction::PRESENT: return { INVALID_ID, 0 };
    case WindowModuleFunction::CHECK_FOR_EVENT: return { (uint16)ValueType::BOOL, 0 };
    case WindowModuleFunction::GET_SIZE: return TypeInfo((uint16)ValueType::VOID_T, 0);
    }
}

TypeInfo WindowModule::GetConstantTypeInfo(uint16 constant)
{
    return TypeInfo((uint16)ValueType::UINT32, 0);
}
