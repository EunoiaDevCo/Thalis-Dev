#include "HeapAllocator.h"
#include "Memory.h"

void* HeapAllocator::AllocAligned(uint64 size, uint64 alignment)
{
	m_NumAllocs++;
	void* data = malloc(size);
	return data;
}

void* HeapAllocator::Alloc(uint64 size)
{
	m_NumAllocs++;
	void* data = malloc(size);
	return data;
}

void HeapAllocator::Free()
{
	
}

uint64 HeapAllocator::GetMaxUsage() const
{
	return 0;
}

void HeapAllocator::Free(void* data)
{
	if (!data)
		return;

	m_NumFrees++;
	free(data);
}

uint64 HeapAllocator::GetNumAllocs() const
{
	return m_NumAllocs;
}

uint64 HeapAllocator::GetNumFrees() const
{
	return m_NumFrees;
}

uint64 HeapAllocator::GetMarker() const
{
	return 0;
}

void HeapAllocator::FreeToMarker(uint64 marker)
{

}
