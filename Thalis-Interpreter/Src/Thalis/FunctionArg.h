#pragma once

#include "Value.h"

struct FunctionArg
{
	FunctionArg(const Value& value) :
		value(value)
	{ }

	FunctionArg() :
		value(Value::MakeNULL())
	{ }

	Value value;
};