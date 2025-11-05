#pragma once

#include <cstdint>
#include <cstddef>
#include <cassert>
#include <new>
#include "../Common.h"

class Allocator
{
public:
	virtual void* AllocAligned(uint64 size, uint64 alignment = alignof(std::max_align_t)) = 0;
	virtual void* Alloc(uint64 size) = 0;
	virtual void Free() = 0;
	virtual void Free(void* data) = 0;

	virtual uint64 GetMaxUsage() const= 0;

	virtual uint64 GetMarker() const = 0;
	virtual void FreeToMarker(uint64 marker) = 0;
};