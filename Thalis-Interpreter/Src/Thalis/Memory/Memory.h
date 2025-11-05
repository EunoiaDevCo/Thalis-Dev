#pragma once

#include "../Common.h"

class Memory
{
public:
	inline static uint64 KBToBytes(uint64 numKB) { return numKB * 1024; }
	inline static uint64 MBToBytes(uint64 numMB) { return numMB * 1024 * 1024; }

	inline static real32 BytesToKB(uint64 numBytes) { return (real32)numBytes / 1024.0f; }
	inline static real32 BytesToMB(uint64 numBytes) { return (real32)numBytes / (1024.0f * 1024.0f); }

	static void* AlignedAlloc(uint64 size, uint64 alignment);
	static void AlignedFree(void* data);
};