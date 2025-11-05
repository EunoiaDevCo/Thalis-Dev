#include "ID.h"

static ID g_NextClassID = 1024;
static ID g_NextVariableID = 1024;
static ID g_NextScopeID = 0;

ID GenClassID()
{
    return g_NextClassID++;
}

ID GenVariableID()
{
    return g_NextVariableID++;
}

ID GenScopeID()
{
    return g_NextScopeID++;
}
