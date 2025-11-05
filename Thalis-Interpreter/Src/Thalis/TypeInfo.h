#pragma once

#include "Common.h"
#include "ID.h"

struct TypeInfo
{
	TypeInfo(uint16 type = INVALID_ID, uint8 pointerLevel = 0, uint32 elementPointerLevel = 0) :
		type(type), pointerLevel(pointerLevel), elementPointerLevel(elementPointerLevel)
	{ }

	uint16 type;
	uint8 pointerLevel;
	uint8 elementPointerLevel;
};