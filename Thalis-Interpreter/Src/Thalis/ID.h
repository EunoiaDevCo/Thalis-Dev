#pragma once

#include "Common.h"
#include <cstdint>

#define INVALID_ID UINT16_MAX
#define VARIABLE_ID_THIS 128

typedef uint32 ID;

ID GenClassID();
ID GenVariableID();