workspace "Thalis-Dev"
	architecture "x64"

	configurations
	{
		"Debug",
		"Release",
		"Dist"
	}

outputdir = "%{cfg.buildcfg}-%{cfg.system}-%{cfg.architecture}"

project "Thalis-Interpreter"
	location "Thalis-Interpreter"
	kind "ConsoleApp"
	language "C++"

	targetdir ("Bin/"..outputdir.."/%{prj.name}")
	objdir ("Bin-Int/"..outputdir.."/%{prj.name}")

	includedirs
	{
		"%{prj.name}/Libs/GLEW/include"
	}

	links
	{
		"Thalis-Interpreter/Libs/GLEW/glew32s",
		"opengl32",
	}

	files
	{
		"%{prj.name}/Src/**.h",
		"%{prj.name}/Src/**.hpp",
		"%{prj.name}/Src/**.inl",
		"%{prj.name}/Src/**.cpp",
		"%{prj.name}/Src/**.c"
	}

	defines
	{
		"GLEW_STATIC"
	}

	filter "system:windows"
		cppdialect "C++17"
		staticruntime "On"
		systemversion "10.0"

		defines
		{
			"TLS_PLATFORM_WINDOWS"
		}

	filter "configurations:Debug"
		defines "TLS_DEBUG"
		symbols "On"
		buildoptions "/MDd"

	filter "configurations:Release"
		defines "TLS_RELEASE"
		optimize "On"
		symbols "On"
		buildoptions "/MD"

	filter "configurations:Dist"
		defines "TLS_DIST"
		optimize "On"
		buildoptions "/MD"