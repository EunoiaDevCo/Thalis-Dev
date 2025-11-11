#include "VTable.h"
#include "Function.h"

VTable& VTable::operator=(const VTable& other)
{
	if (this != &other)
	{
		functions = other.functions;
	}
	return *this;
}

int32 VTable::FindSlot(const std::string& name, const std::vector<TypeInfo>& parameters)
{
	for (uint32 i = 0; i < functions.size(); i++)
	{
		Function* function = functions[i];

		if (function->name != name)
			continue;

		if (function->parameters.size() != parameters.size())
			continue;

		bool matches = true;
		for (uint32 j = 0; j < function->parameters.size(); j++)
		{
			const FunctionParameter& parameter = function->parameters[j];
			if (parameter.type.type != parameters[j].type ||
				parameter.type.pointerLevel != parameters[j].pointerLevel)
			{
				matches = false;
				break;
			}
		}

		if (matches)
			return i;
	}

	return -1;
}
