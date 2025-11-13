#include "MathModule.h"
#include "../Program.h"

#define MATH_PI 3.141592653589793
#define MATH_E 2.718281828459045
#define MATH_TAU (MATH_PI * 2)

#define MATH_CLAMP(Value, Min, Max) ((Value) < (Min) ? (Min) : ((Value) > (Max) ? (Max) : (Value)))
#define MATH_LERP(A, B, T) ((A) + ((B) - (A)) * (T))

static real64 LogBase(real64 x, real64 base)
{
    return std::log(x) / std::log(base);
}

bool MathModule::Init()
{
    return true;
}

Value MathModule::CallFunction(Program* program, uint16 function, const std::vector<FunctionArg>& args)
{
    switch ((MathModuleFunction)function)
    {
    case MathModuleFunction::COS: return Value::MakeReal64(cos(args[0].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::SIN: return Value::MakeReal64(sin(args[0].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::TAN: return Value::MakeReal64(tan(args[0].value.GetReal64()), program->GetStackAllocator());

    case MathModuleFunction::ACOS: return Value::MakeReal64(acos(args[0].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::ASIN: return Value::MakeReal64(asin(args[0].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::ATAN: return Value::MakeReal64(atan(args[0].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::ATAN2: return Value::MakeReal64(atan2(args[0].value.GetReal64(), args[1].value.GetReal64()), program->GetStackAllocator());

    case MathModuleFunction::COSH: return Value::MakeReal64(cosh(args[0].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::SINH: return Value::MakeReal64(sinh(args[0].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::TANH: return Value::MakeReal64(tanh(args[0].value.GetReal64()), program->GetStackAllocator());

    case MathModuleFunction::ACOSH: return Value::MakeReal64(acosh(args[0].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::ASINH: return Value::MakeReal64(asinh(args[0].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::ATANH: return Value::MakeReal64(atanh(args[0].value.GetReal64()), program->GetStackAllocator());

    case MathModuleFunction::DEGTORAD: return Value::MakeReal64(args[0].value.GetReal64() * (MATH_PI / 180.0), program->GetStackAllocator());
    case MathModuleFunction::RADTODEG: return Value::MakeReal64(args[0].value.GetReal64() * (180.0 / MATH_PI), program->GetStackAllocator());

    case MathModuleFunction::FLOOR: return Value::MakeReal64(floor(args[0].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::CEIL: return Value::MakeReal64(ceil(args[0].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::ROUND: return Value::MakeReal64(round(args[0].value.GetReal64()), program->GetStackAllocator());

    case MathModuleFunction::MIN: return Value::MakeReal64(fmin(args[0].value.GetReal64(), args[1].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::MAX: return Value::MakeReal64(fmax(args[0].value.GetReal64(), args[1].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::CLAMP: return Value::MakeReal64(MATH_CLAMP(args[0].value.GetReal64(), args[1].value.GetReal64(), args[2].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::LERP: return Value::MakeReal64(MATH_LERP(args[0].value.GetReal64(), args[1].value.GetReal64(), args[2].value.GetReal64()), program->GetStackAllocator());

    case MathModuleFunction::ABS: return Value::MakeReal64(abs(args[0].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::SQRT: return Value::MakeReal64(sqrt(args[0].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::POW: return Value::MakeReal64(pow(args[0].value.GetReal64(), args[1].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::EXP: return Value::MakeReal64(exp(args[0].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::LOG: return  args.size() == 1 ? Value::MakeReal64(log(args[0].value.GetReal64()), program->GetStackAllocator()) : Value::MakeReal64(LogBase(args[0].value.GetReal64(), args[1].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::LOG10: return Value::MakeReal64(log10(args[0].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::LOG2: return Value::MakeReal64(log2(args[0].value.GetReal64()), program->GetStackAllocator());

    case MathModuleFunction::MOD: return Value::MakeReal64(fmod(args[0].value.GetReal64(), args[1].value.GetReal64()), program->GetStackAllocator());
    case MathModuleFunction::MODF: return Value::MakeReal32(fmodf(args[0].value.GetReal32(), args[1].value.GetReal32()), program->GetStackAllocator());
    default:
        throw std::runtime_error("Invalid MathModule Function");
    }
}

Value MathModule::Constant(Program* program, uint16 constant)
{
    switch ((MathModuleConstant)constant)
    {
    case MathModuleConstant::PI: return Value::MakeReal64(MATH_PI, program->GetStackAllocator());
    case MathModuleConstant::E: return Value::MakeReal64(MATH_E, program->GetStackAllocator());;
    case MathModuleConstant::TAU: return Value::MakeReal64(MATH_TAU, program->GetStackAllocator());;
    }

    return Value::MakeNULL();
}

TypeInfo MathModule::GetFunctionReturnInfo(uint16 function)
{
    switch ((MathModuleFunction)function)
    {
    case MathModuleFunction::COS:
    case MathModuleFunction::SIN:
    case MathModuleFunction::TAN:

    case MathModuleFunction::ACOS:
    case MathModuleFunction::ASIN:
    case MathModuleFunction::ATAN:
    case MathModuleFunction::ATAN2:

    case MathModuleFunction::COSH:
    case MathModuleFunction::SINH:
    case MathModuleFunction::TANH:

    case MathModuleFunction::ACOSH:
    case MathModuleFunction::ASINH:
    case MathModuleFunction::ATANH:

    case MathModuleFunction::DEGTORAD:
    case MathModuleFunction::RADTODEG:

    case MathModuleFunction::FLOOR:
    case MathModuleFunction::CEIL:
    case MathModuleFunction::ROUND:

    case MathModuleFunction::MIN:
    case MathModuleFunction::MAX:
    case MathModuleFunction::CLAMP:
    case MathModuleFunction::LERP:

    case MathModuleFunction::ABS:
    case MathModuleFunction::SQRT:
    case MathModuleFunction::POW:
    case MathModuleFunction::EXP:
    case MathModuleFunction::LOG:
    case MathModuleFunction::LOG10:
    case MathModuleFunction::LOG2:  return { (uint16)ValueType::REAL64, 0 };
    default:
        throw std::runtime_error("Invalid MathModule Function");
    }
}

TypeInfo MathModule::GetConstantTypeInfo(uint16 constant)
{
    switch ((MathModuleConstant)constant)
    {
    case MathModuleConstant::PI:
    case MathModuleConstant::E:
    case MathModuleConstant::TAU: return { (uint16)ValueType::REAL64, 0 };
    }

    return TypeInfo(INVALID_ID, 0);
}