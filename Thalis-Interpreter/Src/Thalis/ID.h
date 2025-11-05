#pragma once

#include "Common.h"
#include <cstdint>

#define INVALID_ID UINT16_MAX

typedef uint32 ID;

ID GenClassID();
ID GenVariableID();