#include "BumpAllocator.h"
#include <cstdlib>
#include <algorithm>

BumpAllocator::BumpAllocator(uint64 size) :
	m_Size(size),
	m_Offset(0)
{
	m_Data = (uint8*)malloc(size);
    m_MaxUsage = 0;
}

void* BumpAllocator::AllocAligned(uint64 size, uint64 alignment)
{
    // Ensure alignment is at least 1
        if (alignment == 0) alignment = 1;

    // Calculate current pointer
    uintptr_t currentAddress = reinterpret_cast<uintptr_t>(m_Data) + m_Offset;

    // Align the address
    uintptr_t alignedAddress = (currentAddress + (alignment - 1)) & ~(alignment - 1);

    // Calculate how much padding we needed
    uint64 padding = alignedAddress - currentAddress;

    // Total size required
    uint64 totalSize = size + padding;

    // Out of memory
    if (m_Offset + totalSize > m_Size)
    {
        return nullptr;
    }

    // Update offset
    m_Offset += totalSize;

    m_MaxUsage = std::max(m_Offset, m_MaxUsage);
    m_MaxUsageAfterFree = std::max(m_Offset, m_MaxUsageAfterFree);

    // Return aligned pointer
    return reinterpret_cast<void*>(alignedAddress);
}

void* BumpAllocator::Alloc(uint64 size)
{
    uint8* data = m_Data + m_Offset;
    m_Offset += size;
    m_MaxUsage = std::max(m_Offset, m_MaxUsage);
    m_MaxUsageAfterFree = std::max(m_Offset, m_MaxUsageAfterFree);
    return data;
}

void BumpAllocator::Free()
{
    m_Offset = 0;
    m_MaxUsageAfterFree = 0;
}

uint64 BumpAllocator::GetMaxUsage() const
{
    return m_MaxUsage;
}

uint64 BumpAllocator::GetMaxUsageAfterFree() const
{
    return m_MaxUsageAfterFree;
}

uint64 BumpAllocator::GetMarker() const
{
    return m_Offset;
}

void BumpAllocator::FreeToMarker(uint64 marker)
{
    assert(marker <= m_Offset && "FreeToMarker(): marker beyond current offset!");
    m_Offset = marker;
}
