#pragma once

#include "../../Window.h"

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

class Win32Window : public Window
{
public:
	virtual bool Init(const std::string& title, uint32 width, uint32 height);
	virtual void Destroy() override;
	virtual void Update() override;
	virtual void Present() override;

	virtual void GetSize(uint32* width, uint32* height) const;

	inline HDC GetHDC() const { return m_HDC; }
private:
	friend LRESULT CALLBACK WindowCallbackWin32(HWND windowHandle, UINT msg, WPARAM wparam, LPARAM lparam);
private:
	HWND	m_Handle;
	HDC		m_HDC;
	uint32	m_Width;
	uint32	m_Height;
};