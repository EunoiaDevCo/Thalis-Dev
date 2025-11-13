#include "Win32Window.h"

static bool g_ClassRegistered = false;
static LPCWSTR g_ClassName = L"Thalis";

static bool InitRawInputs(HWND handle)
{
	RAWINPUTDEVICE keyboard = {};
	keyboard.dwFlags = RIDEV_NOLEGACY;
	keyboard.usUsagePage = 0x01;
	keyboard.usUsage = 0x06;
	keyboard.hwndTarget = handle;

	RAWINPUTDEVICE mouse = {};
	mouse.dwFlags = 0;
	mouse.usUsagePage = 0x01;
	mouse.usUsage = 0x02;
	mouse.hwndTarget = handle;

	RAWINPUTDEVICE inputDevices[2] = { mouse, keyboard };

	BOOL result = RegisterRawInputDevices(inputDevices, 2, sizeof(RAWINPUTDEVICE));
	if (!result)
	{
		return false;
	}

	return true;
}

LRESULT WindowCallbackWin32(HWND windowHandle, UINT msg, WPARAM wparam, LPARAM lparam)
{
	Win32Window* window = 0;
	LRESULT result = 0;

	if (msg != WM_CREATE)
		window = (Win32Window*)GetWindowLongPtr(windowHandle, GWLP_USERDATA);

	switch (msg)
	{
	case WM_INPUT: {
		
	} break;
	case WM_CREATE: {
		SetWindowLongPtr(windowHandle, GWLP_USERDATA, (LONG_PTR)((CREATESTRUCT*)lparam)->lpCreateParams);
		window = (Win32Window*)((CREATESTRUCT*)lparam)->lpCreateParams;
		window->m_Events[(uint32)WindowEventType::CREATE] = true;
	} break;
	case WM_SIZE: {
		window->m_Width = LOWORD(lparam);
		window->m_Height = HIWORD(lparam);
		window->m_Events[(uint32)WindowEventType::RESIZE] = true;
	} break;
	case WM_CLOSE: {
		window->m_Events[(uint32)WindowEventType::CLOSE] = true;
	} break;
	case WM_MOUSEMOVE: {
		
	} break;
	case WM_SETFOCUS: {
		
	} break;
	
	default: {
		result = DefWindowProc(windowHandle, msg, wparam, lparam);
	}
	}

	return result;
}

bool Win32Window::Init(const std::string& title, uint32 width, uint32 height)
{
	HINSTANCE instance = GetModuleHandle(0);

	bool initRawInputs = !g_ClassRegistered;

	if (!g_ClassRegistered)
	{
		WNDCLASS wndclass = {};
		wndclass.style = CS_OWNDC;
		wndclass.hInstance = instance;
		wndclass.lpszClassName = g_ClassName;
		wndclass.lpfnWndProc = WindowCallbackWin32;
		wndclass.hCursor = LoadCursor(0, IDC_ARROW);

		if (!RegisterClass(&wndclass))
		{
			return false;
		}

		g_ClassRegistered = true;
	}

	wchar_t wStringTitle[1024];
	MultiByteToWideChar(CP_ACP, 0, title.c_str(), -1, wStringTitle, title.length());
	wStringTitle[title.length()] = 0;

	uint32 flags = WS_VISIBLE | WS_SYSMENU | WS_BORDER | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX;

	m_Handle = CreateWindowEx(0, g_ClassName, wStringTitle, flags,
		CW_USEDEFAULT, CW_USEDEFAULT, width, height, 0, 0, 0, (LPVOID)this);


	if (!m_Handle)
	{
		return false;
	}

	//if(initRawInputs)
	InitRawInputs(m_Handle);

	PIXELFORMATDESCRIPTOR pixel_format = {};

	pixel_format.dwFlags = PFD_DRAW_TO_WINDOW | PFD_DOUBLEBUFFER | PFD_SUPPORT_OPENGL;

	pixel_format.nVersion = 1;
	pixel_format.nSize = sizeof(PIXELFORMATDESCRIPTOR);
	pixel_format.cColorBits = 32;
	pixel_format.cRedBits = 8;
	pixel_format.cGreenBits = 8;
	pixel_format.cBlueBits = 8;
	pixel_format.cAlphaBits = 8;
	pixel_format.cDepthBits = 24;
	pixel_format.cStencilBits = 8;
	pixel_format.iPixelType = PFD_TYPE_RGBA;
	pixel_format.cAuxBuffers = 0;
	pixel_format.iLayerType = PFD_MAIN_PLANE;

	m_HDC = GetDC(m_Handle);
	int pixelFormat = ChoosePixelFormat(m_HDC, &pixel_format);
	if (pixelFormat)
	{
		if (!SetPixelFormat(m_HDC, pixelFormat, &pixel_format))
		{
			return false;
		}
	}
	else
	{
		return false;
	}

	RECT size;
	GetClientRect(m_Handle, &size);
	m_Width = size.right - size.left;
	m_Height = size.bottom - size.top;

	return true;
}

void Win32Window::Destroy()
{
	DestroyWindow(m_Handle);
}

void Win32Window::Update()
{
	memset(m_Events, false, sizeof(bool) * (uint32)WindowEventType::NUM_EVENT_TYPES);

	MSG msg = {};
	while (PeekMessage(&msg, m_Handle, 0, 0, PM_REMOVE))
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
}

void Win32Window::Present()
{
	SwapBuffers(m_HDC);
}

void Win32Window::GetSize(uint32* width, uint32* height) const
{
	*width = m_Width;
	*height = m_Height;
}
