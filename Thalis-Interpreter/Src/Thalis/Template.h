#pragma once

#include "Common.h"
#include <vector>
#include <string>

enum class TemplateParameterType
{
	TYPE, INT, TEMPLATE_TYPE
};

struct TemplateParameter
{
	TemplateParameterType type;
	std::string name;
};

struct TemplateDefinition
{
	std::vector<TemplateParameter> parameters;
	inline bool HasTemplate() const { return !parameters.empty(); }
};

struct TemplateArgument
{
	TemplateParameterType type;
	uint32 value; //integer value or typeID
	uint8 pointerLevel;
	std::string templateTypeName;
};

struct TemplateInstantiation
{
	std::vector<TemplateArgument> args;
	bool HasTemplatedType() const;
};

struct TemplateInstantiationCommand;
struct TemplateInstantiationCommandArg
{
	uint32 type; //0 for arg, 1 for sub command
	TemplateArgument arg;
	TemplateInstantiationCommand* command;
};

struct TemplateInstantiationCommand
{
	std::vector<TemplateInstantiationCommandArg> args;
	uint16 type;
};
