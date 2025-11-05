#include "Memory.h"
#include <malloc.h>

void* Memory::AlignedAlloc(uint64 size, uint64 alignment)
{
#if defined(_MSC_VER)
    return _aligned_malloc(size, alignment);
#else
    return aligned_alloc(alignment, (size + alignment - 1) / alignment * alignment);
#endif
}

void Memory::AlignedFree(void* data)
{
#if defined(_MSC_VER)
    _aligned_free(data);
#else
    free(data);
#endif
}
