#pragma once

#include <string>
#include "Common.h"

enum class WindowEventType
{
	CREATE, CLOSE, RESIZE, NUM_EVENT_TYPES
};

class Window
{
public:
	Window();

	virtual bool Init(const std::string& title, uint32 width, uint32 height) = 0;
	virtual void Destroy() = 0;
	virtual void Update() = 0;
	virtual void Present() = 0;

	virtual void GetSize(uint32* width, uint32* height) const = 0;

	bool CheckForEvent(WindowEventType event) const;
public:
	static uint32 TLSCreateWindow(const std::string& title, uint32 width, uint32 height);
	static Window* GetWindow(uint32 handle);
protected:
	bool m_Events[(uint32)WindowEventType::NUM_EVENT_TYPES];
};