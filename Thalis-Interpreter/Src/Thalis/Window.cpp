#include "Window.h"

#include <unordered_map>

#ifdef TLS_PLATFORM_WINDOWS
#include "Platform/Windows/Win32Window.h"
#endif

static std::unordered_map<uint32, Window*> g_Windows;
static uint32 g_NextWindowID = 1;

Window::Window()
{
	memset(m_Events, false, sizeof(bool) * (uint32)WindowEventType::NUM_EVENT_TYPES);
}

bool Window::CheckForEvent(WindowEventType event) const
{
	return m_Events[(uint32)event];
}

uint32 Window::TLSCreateWindow(const std::string& title, uint32 width, uint32 height)
{
	Window* window = NULL;
#ifdef TLS_PLATFORM_WINDOWS
	window = new Win32Window();
#endif

	if (!window->Init(title, width, height))
	{
		return 0;
	}

	uint32 windowID = g_NextWindowID++;
	g_Windows[windowID] = window;
	return windowID;
}

Window* Window::GetWindow(uint32 handle)
{
	return g_Windows[handle];
}
