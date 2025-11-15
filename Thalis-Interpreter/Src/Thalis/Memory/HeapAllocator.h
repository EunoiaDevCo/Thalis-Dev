#pragma once

#include "Allocator.h"

class HeapAllocator : public Allocator
{
public:
	virtual void* AllocAligned(uint64 size, uint64 alignment = alignof(std::max_align_t)) override;
	virtual void* Alloc(uint64 size);
	virtual void Free() override;

	virtual uint64 GetMaxUsage() const override;
	virtual uint64 GetMaxUsageAfterFree() const override;

	virtual void Free(void* data) override;

	uint64 GetNumAllocs() const;
	uint64 GetNumFrees() const;

	virtual uint64 GetMarker() const override;
	virtual void FreeToMarker(uint64 marker) override;
private:
	uint64 m_NumAllocs;
	uint64 m_NumFrees;
};