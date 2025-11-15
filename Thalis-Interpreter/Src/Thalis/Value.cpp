#include "Value.h"
#include "Program.h"
#include "Class.h"
#include "VTable.h"

Value Value::CastTo(Program* program, uint16 newType, uint8 pointerLevel, Allocator* allocator) const
{
	Value value;
	value.type = newType;
	value.pointerLevel = pointerLevel;
	if (IsPointer())
	{
		value.data = data;
	}
	else
	{
		uint64 typeSize = program->GetTypeSize(newType);
		value.data = allocator->Alloc(typeSize);
		switch ((ValueType)newType)
		{
		case ValueType::UINT8:   *(uint8*)value.data = GetUInt8(); break;
		case ValueType::UINT16:  *(uint16*)value.data = GetUInt16(); break;
		case ValueType::UINT32:  *(uint32*)value.data = GetUInt32(); break;
		case ValueType::UINT64:  *(uint64*)value.data = GetUInt64(); break;
		case ValueType::INT8:    *(int8*)value.data = GetInt8(); break;
		case ValueType::INT16:   *(int16*)value.data = GetInt16(); break;
		case ValueType::INT32:   *(int32*)value.data = GetInt32(); break;
		case ValueType::INT64:   *(int64*)value.data = GetInt64(); break;
		case ValueType::REAL32:  *(real32*)value.data = GetReal32(); break;
		case ValueType::REAL64:  *(real64*)value.data = GetReal64(); break;
		case ValueType::BOOL:    *(bool*)value.data = GetBool(); break;
		case ValueType::CHAR:    *(char*)value.data = GetChar(); break;
		default: {
			memcpy(value.data, data, typeSize);
		} break;
		}
	}

	return value;
}

void Value::Assign(const Value& value)
{
	if (IsPointer())
	{
		if (pointerLevel != value.pointerLevel)
			return;

	}

	switch ((ValueType)type)
	{
	case ValueType::UINT8:   *(uint8*)data = value.GetUInt8(); break;
	case ValueType::UINT16:  *(uint16*)data = value.GetUInt16(); break;
	case ValueType::UINT32:  *(uint32*)data = value.GetUInt32(); break;
	case ValueType::UINT64:  *(uint64*)data = value.GetUInt64(); break;
	case ValueType::INT8:    *(int8*)data = value.GetInt8(); break;
	case ValueType::INT16:   *(int16*)data = value.GetInt16(); break;
	case ValueType::INT32:   *(int32*)data = value.GetInt32(); break;
	case ValueType::INT64:   *(int64*)data = value.GetInt64(); break;
	case ValueType::REAL32:  *(real32*)data = value.GetReal32(); break;
	case ValueType::REAL64:  *(real64*)data = value.GetReal64(); break;
	case ValueType::BOOL:    *(bool*)data = value.GetBool(); break;
	case ValueType::CHAR:    *(char*)data = value.GetChar(); break;
	case ValueType::STRING: {
		/*if (value.type != (uint16)ValueType::STRING) return;

		uint32 length = *(uint32*)data;
		char* str = (char*)((uint8*)data + sizeof(uint32));

		uint32 vlength = *(uint32*)value.data;
		char* vstr = (char*)((uint8*)value.data + sizeof(uint32));

		if (vlength <= length)
		{
			strcpy(str, vstr);
			*(uint32*)data = vlength;
			return;
		}

		HeapAllocator* allocator = Program::GetCompiledProgram()->GetHeapAllocator();
		allocator->Free(data);
		data = allocator->AllocAligned(vlength + 1 + sizeof(uint32), alignof(uint32));
		strcpy((char*)((uint8*)data + sizeof(uint32)), vstr);*/
		data = value.data;
	} break;
	default: {
		if (type != value.type)
		{
			return;
		}

		Program* program = Program::GetCompiledProgram();
		uint64 typeSize = program->GetTypeSize(type);
		memcpy(data, value.data, typeSize);
	} break;
	}

}

void Value::AssignMember(uint64 offset, uint16 type, uint8 pointerLevel, uint64 typeSize, const Value& value)
{
	uint8 pl = isArray ? 0 : pointerLevel;
	if (pl != value.pointerLevel) return;
	if (pl > 0)
	{
		*(void**)((uint8*)data + offset) = value.data;
	}
	else
	{
		switch ((ValueType)type)
		{
		case ValueType::UINT8:   *(uint8*)((uint8*)data + offset) = value.GetUInt8(); break;
		case ValueType::UINT16:  *(uint16*)((uint8*)data + offset) = value.GetUInt16(); break;
		case ValueType::UINT32:  *(uint32*)((uint8*)data + offset) = value.GetUInt32(); break;
		case ValueType::UINT64:  *(uint64*)((uint8*)data + offset) = value.GetUInt64(); break;
		case ValueType::INT8:    *(int8*)((uint8*)data + offset) = value.GetInt8(); break;
		case ValueType::INT16:   *(int16*)((uint8*)data + offset) = value.GetInt16(); break;
		case ValueType::INT32:   *(int32*)((uint8*)data + offset) = value.GetInt32(); break;
		case ValueType::INT64:   *(int64*)((uint8*)data + offset) = value.GetInt64(); break;
		case ValueType::REAL32:  *(real32*)((uint8*)data + offset) = value.GetReal32(); break;
		case ValueType::REAL64:  *(real64*)((uint8*)data + offset) = value.GetReal64(); break;
		case ValueType::BOOL:    *(bool*)((uint8*)data + offset) = value.GetBool(); break;
		case ValueType::CHAR:    *(char*)((uint8*)data + offset) = value.GetChar(); break;
		case ValueType::STRING: {
			*(void**)((uint8*)data + offset) = value.data;
		} break;
		default: {
			memcpy((uint8*)data + offset, value.data, typeSize);
		}
		}
	}
}

void Value::AssignMemberIndex(uint64 offset, uint16 type, uint8 pointerLevel, uint64 typeSize, const Value& value)
{
	uint8 pl = isArray ? 0 : pointerLevel;
	if (pl != value.pointerLevel) return;
	if (pl > 0)
	{
		*(void**)((uint8*)data + offset) = value.data;
	}
	else
	{
		switch ((ValueType)type)
		{
		case ValueType::UINT8:   *(uint8*)(*(void**)((uint8*)data + offset)) = value.GetUInt8(); break;
		case ValueType::UINT16:  *(uint16*)(*(void**)((uint8*)data + offset)) = value.GetUInt16(); break;
		case ValueType::UINT32:  *(uint32*)(*(void**)((uint8*)data + offset)) = value.GetUInt32(); break;
		case ValueType::UINT64:  *(uint64*)(*(void**)((uint8*)data + offset)) = value.GetUInt64(); break;
		case ValueType::INT8:    *(int8*)(*(void**)((uint8*)data + offset)) = value.GetInt8(); break;
		case ValueType::INT16:   *(int16*)(*(void**)((uint8*)data + offset)) = value.GetInt16(); break;
		case ValueType::INT32:   *(int32*)(*(void**)((uint8*)data + offset)) = value.GetInt32(); break;
		case ValueType::INT64:   *(int64*)(*(void**)((uint8*)data + offset)) = value.GetInt64(); break;
		case ValueType::REAL32:  *(real32*)(*(void**)((uint8*)data + offset)) = value.GetReal32(); break;
		case ValueType::REAL64:  *(real64*)(*(void**)((uint8*)data + offset)) = value.GetReal64(); break;
		case ValueType::BOOL:    *(bool*)(*(void**)((uint8*)data + offset)) = value.GetBool(); break;
		case ValueType::CHAR:    *(char*)(*(void**)((uint8*)data + offset)) = value.GetBool(); break;
		case ValueType::STRING: {
			*(void**)((uint8*)data + offset) = value.data;
		} break;
		default: {
			memcpy((uint8*)data + offset, value.data, typeSize);
		}
		}
	}
}

Value Value::Clone(Program* program, Allocator* allocator)
{
	Value value;
	value.type = type;
	value.pointerLevel = pointerLevel;
	value.isArray = isArray;
	if (IsPointer())
	{
		value.data = data;
	}
	else if (type == (uint16)ValueType::STRING)
	{
		uint32 length = *(uint32*)data;
		char* str = (char*)((uint8*)data + sizeof(uint32));
		value.data = allocator->AllocAligned(length + 1 + sizeof(uint32), alignof(uint32));
		*(uint32*)value.data = length;
		char* vstr = (char*)((uint8*)value.data + sizeof(uint32));
		strcpy(vstr, str);
	}
	else
	{
		uint64 typeSize = program->GetTypeSize(type);
		value.data = allocator->Alloc(program->GetTypeSize(type));
		memcpy(value.data, data, typeSize);
	}

	return value;
}

Value Value::MakeArray(Program* program, uint16 type, uint8 pointerLevel, uint32 length, Allocator* allocator)
{
	uint64 typeSize = pointerLevel == 0 ? program->GetTypeSize(type) : sizeof(void*);
	if(!Value::IsPrimitiveType(type) && pointerLevel == 0)
		typeSize += sizeof(VTable*);
	
	ArrayHeader header;
	header.elementPointerLevel = pointerLevel;
	header.length = length;

	uint64 arraySize = typeSize * length + sizeof(ArrayHeader);
	uint8* arrayData = (uint8*)allocator->Alloc(arraySize);
	memcpy(arrayData, &header, sizeof(ArrayHeader));

	uint8* elements = arrayData + sizeof(ArrayHeader);

	if (pointerLevel == 0 && !IsPrimitiveType(type))
	{
		Class* cls = program->GetClass(type);
		VTable* vtable = cls->GetVTable();

		for (uint32 i = 0; i < length; i++)
		{
			uint8* elementBase = elements + i * typeSize;
			*(VTable**)elementBase = vtable;
		}
	}

	Value value;
	value.type = type;
	value.pointerLevel = 1 + pointerLevel;
	value.data = elements; 
	value.isArray = true;

	return value;
}

static void InitializeArrayHeaders(Program* program, uint8* data, Class* cls)
{
	const std::vector<ClassField>& members = cls->GetMemberFields();
	for (uint32 i = 0; i < members.size(); i++)
	{
		const ClassField& member = members[i];
		if (member.arrayLength > 0)
		{
			ArrayHeader* header = (ArrayHeader*)(data + member.offset - sizeof(ArrayHeader));
			header->length = member.arrayLength;
			header->elementPointerLevel = member.type.pointerLevel - 1;

			if (!Value::IsPrimitiveType(member.type.type) && member.type.pointerLevel == 0)
			{
				Class* elementClass = program->GetClass(member.type.type);
				for (uint32 j = 0; j < member.arrayLength; j++)
				{
					InitializeArrayHeaders(program, data + member.offset + elementClass->GetSize() * i, elementClass);
				}
			}
		}
		else if (!Value::IsPrimitiveType(member.type.type) && member.type.pointerLevel == 0)
		{
			InitializeArrayHeaders(program, data + member.offset, program->GetClass(member.type.type));
		}
	}
}

Value Value::MakeObject(Program* program, uint16 type, Allocator* allocator)
{
	Class* cls = program->GetClass(type);
	uint64 typeSize = program->GetTypeSize(type);

	// Allocate enough for vtable pointer + object data
	uint64 totalSize = sizeof(VTable*) + typeSize;
	uint8* memory = (uint8*)allocator->Alloc(totalSize);

	// Initialize the vtable pointer to this class’s vtable
	*(VTable**)memory = cls->GetVTable(); // pointer to the class’s vtable

	// Create Value object
	Value value;
	value.type = type;
	value.pointerLevel = 0;
	value.data = memory + sizeof(VTable*); // point *after* the vtable pointer
	value.isArray = false;

	memset(value.data, 0, typeSize);
	InitializeArrayHeaders(program, (uint8*)value.data, cls);

	return value;
}
