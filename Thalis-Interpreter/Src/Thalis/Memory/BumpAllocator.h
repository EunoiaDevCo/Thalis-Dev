#pragma once

#include "Allocator.h"

class BumpAllocator : public Allocator
{
public:
	BumpAllocator(uint64 size);
	virtual void* AllocAligned(uint64 size, uint64 alignment) override;
	virtual void* Alloc(uint64 size) override;
	virtual void Free() override;
	virtual void Free(void* data) override {}

	virtual uint64 GetMaxUsage() const override;
	virtual uint64 GetMaxUsageAfterFree() const override;

	virtual uint64 GetMarker() const override;
	virtual void FreeToMarker(uint64 marker) override;
private:
	uint8* m_Data;
	uint64 m_Size;
	uint64 m_Offset;
	uint64 m_MaxUsage;
	uint64 m_MaxUsageAfterFree;
};