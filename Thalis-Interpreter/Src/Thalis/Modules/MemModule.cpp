#include "MemModule.h"
#include "../Program.h"

bool MemModule::Init()
{
	return true;
}

Value MemModule::CallFunction(Program* program, uint16 function, const std::vector<FunctionArg>& args)
{
	switch ((MemModuleFunction)function)
	{
	case MemModuleFunction::COPY: {
		Value dst = args[0].value;
		Value src = args[1].value;
		uint64 size = args[2].value.GetUInt64();
		memcpy(dst.data, src.data, size);
		return Value::MakeNULL();
	} break;
	case MemModuleFunction::ALLOC: {
		uint64 size = args[0].value.GetUInt64() + sizeof(ArrayHeader);
		uint8* data = (uint8*)program->GetHeapAllocator()->Alloc(size);
		ArrayHeader* header = (ArrayHeader*)data;
		header->elementPointerLevel = 0;
		header->length = size;

		return Value::MakePointer(1, (uint16)ValueType::VOID_T, data + sizeof(ArrayHeader));
	} break;

	}

	return Value::MakeNULL();
}

Value MemModule::Constant(Program* program, uint16 constant)
{
	return Value::MakeNULL();
}

TypeInfo MemModule::GetFunctionReturnInfo(uint16 function)
{
	return TypeInfo((uint16)ValueType::VOID_T, 0);
}

TypeInfo MemModule::GetConstantTypeInfo(uint16 constant)
{
	return TypeInfo(INVALID_ID, 0);
}