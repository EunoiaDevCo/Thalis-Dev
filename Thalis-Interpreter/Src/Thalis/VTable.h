#pragma once

#include <vector>
#include <string>
#include "Common.h"
#include "TypeInfo.h"

struct Function;
struct VTable
{
	std::vector<Function*> functions;

	VTable& operator=(const VTable& other);

	int32 FindSlot(const std::string& name, const std::vector<TypeInfo>& parameters);
	inline Function* GetFunction(uint16 slot) { return functions[slot]; }
};