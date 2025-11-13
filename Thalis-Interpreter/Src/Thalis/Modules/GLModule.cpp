#include "GLModule.h"

#include "../Program.h"
#include "../Window.h"
#ifdef TLS_PLATFORM_WINDOWS
#include "../Platform/Windows/Win32Window.h"
#endif

#include <GL/glew.h>
#include <GL/wglew.h>

static HGLRC g_Context;

static std::vector<uint32> m_DrawBuffers;

bool GLModule::Init()
{
    m_DrawBuffers.reserve(32);
    return true;
}

Value GLModule::CallFunction(Program* program, uint16 function, const std::vector<FunctionArg>& args)
{
    switch ((GLModuleFunction)function)
    {
    case GLModuleFunction::TGL_INIT: {
        uint32 windowHandle = args[0].value.GetUInt32();
#ifdef TLS_PLATFORM_WINDOWS
        HDC hdc = ((Win32Window*)Window::GetWindow(windowHandle))->GetHDC();
        g_Context = wglCreateContext(hdc);
        if (!wglMakeCurrent(hdc, g_Context))
        {
            return Value::MakeBool(false, program->GetStackAllocator());
        }
#endif

        if (glewInit() != GLEW_OK)
        {
            return Value::MakeBool(false, program->GetStackAllocator());
        }

        return Value::MakeBool(true, program->GetStackAllocator());
    }
                                   // ------------------------------
                                   // Buffer Objects
                                   // ------------------------------
    case GLModuleFunction::TGL_GEN_BUFFERS: {
        GLuint id = 0;
        glGenBuffers(1, &id);
        return Value::MakeUInt32((int32_t)id, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_DELETE_BUFFERS: {
        GLuint id = (GLuint)args[0].value.GetUInt32();
        glDeleteBuffers(1, &id);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BIND_BUFFER: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLuint buffer = (GLuint)args[1].value.GetUInt32();
        glBindBuffer(target, buffer);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BUFFER_DATA: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLsizeiptr size = (GLsizeiptr)args[1].value.GetInt64();
        void* data = args[2].value.data;
        GLenum usage = (GLenum)args[3].value.GetInt32();
        glBufferData(target, size, data, usage);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BUFFER_SUB_DATA: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLintptr offset = (GLintptr)args[1].value.GetInt64();
        GLsizeiptr size = (GLsizeiptr)args[2].value.GetInt64();
        const void* data = args[3].value.data;
        glBufferSubData(target, offset, size, data);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_MAP_BUFFER: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLenum access = (GLuint)args[1].value.GetInt32();
        void* data = glMapBuffer(target, access);
        return Value::MakePointer(1, (uint16)ValueType::VOID_T, data);
    }

    case GLModuleFunction::TGL_UNMAP_BUFFER: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLboolean res = glUnmapBuffer(target);
        return Value::MakeBool(res == GL_TRUE, program->GetStackAllocator());
    }
                                           // ------------------------------
                                           // Vertex Arrays
                                           // ------------------------------
    case GLModuleFunction::TGL_GEN_VERTEX_ARRAYS: {
        GLuint id = 0;
        glGenVertexArrays(1, &id);
        return Value::MakeUInt32((int32_t)id, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_DELETE_VERTEX_ARRAYS: {
        GLuint id = (GLuint)args[0].value.GetInt32();
        glDeleteVertexArrays(1, &id);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BIND_VERTEX_ARRAY: {
        GLuint id = (GLuint)args[0].value.GetInt32();
        glBindVertexArray(id);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_ENABLE_VERTEX_ATTRIB_ARRAY: {
        GLuint index = (GLuint)args[0].value.GetInt32();
        glEnableVertexAttribArray(index);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_DISABLE_VERTEX_ATTRIB_ARRAY: {
        GLuint index = (GLuint)args[0].value.GetInt32();
        glDisableVertexAttribArray(index);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_VERTEX_ATTRIB_POINTER: {
        GLuint index = (GLuint)args[0].value.GetInt32();
        GLint size = args[1].value.GetInt32();
        GLenum type = (GLenum)args[2].value.GetInt32();
        GLboolean normalized = (GLboolean)(args[3].value.GetBool() ? GL_TRUE : GL_FALSE);
        GLsizei stride = args[4].value.GetInt32();
        uint64 offset = args[5].value.GetUInt64();
        glVertexAttribPointer(index, size, type, normalized, stride, (const GLvoid*)offset);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_VERTEX_ATTRIB_IPOINTER: {
        GLuint index = (GLuint)args[0].value.GetInt32();
        GLint size = args[1].value.GetInt32();
        GLenum type = (GLenum)args[2].value.GetInt32();
        GLsizei stride = args[3].value.GetInt32();
        uint64 offset = args[4].value.GetUInt64();
        glVertexAttribIPointer(index, size, type, stride, (GLvoid*)offset);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_VERTEX_ATTRIB_DIVISOR: {
        GLuint index = (GLuint)args[0].value.GetInt32();
        GLuint divisor = (GLuint)args[1].value.GetInt32();
        glVertexAttribDivisor(index, divisor);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BIND_VERTEX_BUFFER: {
        GLuint bindingIndex = (GLuint)args[0].value.GetInt32();
        GLuint buffer = (GLuint)args[1].value.GetInt32();
        GLintptr offset = (GLintptr)args[2].value.GetInt64();
        GLsizei stride = (GLsizei)args[3].value.GetInt32();
        glBindVertexBuffer(bindingIndex, buffer, offset, stride);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_VERTEX_ATTRIB_FORMAT: {
        GLuint attribIndex = (GLuint)args[0].value.GetInt32();
        GLint size = args[1].value.GetInt32();
        GLenum type = (GLenum)args[2].value.GetInt32();
        GLboolean normalized = (GLboolean)(args[3].value.GetBool() ? GL_TRUE : GL_FALSE);
        GLuint relativeOffset = (GLuint)args[4].value.GetInt32();
        glVertexAttribFormat(attribIndex, size, type, normalized, relativeOffset);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_VERTEX_ATTRIB_BINDING: {
        GLuint attribIndex = (GLuint)args[0].value.GetInt32();
        GLuint bindingIndex = (GLuint)args[1].value.GetInt32();
        glVertexAttribBinding(attribIndex, bindingIndex);
        return Value::MakeNULL();
    }

                                                    // ------------------------------
                                                    // Drawing Commands
                                                    // ------------------------------
    case GLModuleFunction::TGL_DRAW_ARRAYS: {
        GLenum mode = (GLenum)args[0].value.GetInt32();
        GLint first = args[1].value.GetInt32();
        GLsizei count = args[2].value.GetInt32();
        glDrawArrays(mode, first, count);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_DRAW_ELEMENTS: {
        GLenum mode = (GLenum)args[0].value.GetInt32();
        GLsizei count = args[1].value.GetInt32();
        GLenum type = (GLenum)args[2].value.GetInt32();
        uint64 offset = args[3].value.GetUInt64();
        glDrawElements(mode, count, type, (GLvoid*)offset);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_DRAW_ELEMENTS_BASE_VERTEX: {
        GLenum mode = (GLenum)args[0].value.GetInt32();
        GLsizei count = args[1].value.GetInt32();
        GLenum type = (GLenum)args[2].value.GetInt32();
        uint64 offset = args[3].value.GetUInt64();
        GLint basevertex = args[4].value.GetInt32();
        glDrawElementsBaseVertex(mode, count, type, (GLvoid*)offset, basevertex);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_DRAW_ELEMENTS_INSTANCED: {
        GLenum mode = (GLenum)args[0].value.GetInt32();
        GLsizei count = args[1].value.GetInt32();
        GLenum type = (GLenum)args[2].value.GetInt32();
        uint64 offset = args[3].value.GetUInt64();
        GLsizei instancecount = args[4].value.GetInt32();
        glDrawElementsInstanced(mode, count, type, (GLvoid*)offset, instancecount);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_DRAW_ARRAYS_INSTANCED: {
        GLenum mode = (GLenum)args[0].value.GetInt32();
        GLint first = args[1].value.GetInt32();
        GLsizei count = args[2].value.GetInt32();
        GLsizei instancecount = args[3].value.GetInt32();
        glDrawArraysInstanced(mode, first, count, instancecount);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_DRAW_RANGE_ELEMENTS: {
        GLenum mode = (GLenum)args[0].value.GetInt32();
        GLuint start = (GLuint)args[1].value.GetInt32();
        GLuint end = (GLuint)args[2].value.GetInt32();
        GLsizei count = args[3].value.GetInt32();
        GLenum type = (GLenum)args[4].value.GetInt32();
        uint64 offset = args[5].value.GetUInt64();
        glDrawRangeElements(mode, start, end, count, type, (GLvoid*)offset);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_DRAW_BUFFER: {
        GLenum buf = (GLenum)args[0].value.GetInt32();
        glDrawBuffer(buf);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_DRAW_BUFFERS: {
        uint32 count = args[0].value.GetUInt32();
        const void* data = args[1].value.data;
        glDrawBuffers(count, (const GLenum*)data);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_CLEAR: {
        GLbitfield mask = (GLbitfield)args[0].value.GetInt32();
        glClear(mask);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_CLEAR_COLOR: {
        GLfloat r = args[0].value.GetReal32();
        GLfloat g = args[1].value.GetReal32();
        GLfloat b = args[2].value.GetReal32();
        GLfloat a = args[3].value.GetReal32();
        glClearColor(r, g, b, a);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_CLEAR_DEPTH: {
        GLdouble depth = (GLdouble)args[0].value.GetReal32();
        glClearDepth(depth);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_CLEAR_STENCIL: {
        GLint s = args[0].value.GetInt32();
        glClearStencil(s);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_POLYGON_MODE: {
        GLenum face = (GLenum)args[0].value.GetInt32();
        GLenum mode = (GLenum)args[1].value.GetInt32();
        glPolygonMode(face, mode);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_LINE_WIDTH: {
        GLfloat width = args[0].value.GetReal32();
        glLineWidth(width);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_POINT_SIZE: {
        GLfloat size = args[0].value.GetReal32();
        glPointSize(size);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_CULL_FACE: {
        GLenum mode = (GLenum)args[0].value.GetInt32();
        if (mode) glCullFace(mode);
        else glCullFace(GL_BACK);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_FRONT_FACE: {
        GLenum mode = (GLenum)args[0].value.GetInt32();
        glFrontFace(mode);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_POLYGON_OFFSET: {
        GLfloat factor = args[0].value.GetReal32();
        GLfloat units = args[1].value.GetReal32();
        glPolygonOffset(factor, units);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_SCISSOR: {
        GLint x = args[0].value.GetInt32();
        GLint y = args[1].value.GetInt32();
        GLsizei w = args[2].value.GetInt32();
        GLsizei h = args[3].value.GetInt32();
        glScissor(x, y, w, h);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_VIEWPORT: {
        GLint x = args[0].value.GetInt32();
        GLint y = args[1].value.GetInt32();
        GLsizei w = args[2].value.GetInt32();
        GLsizei h = args[3].value.GetInt32();
        glViewport(x, y, w, h);
        return Value::MakeNULL();
    }

                                       // ------------------------------
                                       // Framebuffers / Renderbuffers
                                       // ------------------------------
    case GLModuleFunction::TGL_GEN_FRAMEBUFFERS: {
        GLuint id = 0;
        glGenFramebuffers(1, &id);
        return Value::MakeUInt32((int32_t)id, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_DELETE_FRAMEBUFFERS: {
        GLuint id = (GLuint)args[0].value.GetUInt32();
        glDeleteFramebuffers(1, &id);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BIND_FRAMEBUFFER: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLuint fb = (GLuint)args[1].value.GetInt32();
        glBindFramebuffer(target, fb);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_FRAMEBUFFER_TEXTURE: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLenum attachment = (GLenum)args[1].value.GetInt32();
        GLuint texture = (GLuint)args[2].value.GetInt32();
        GLint level = args[3].value.GetInt32();
        glFramebufferTexture(target, attachment, texture, level);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_FRAMEBUFFER_TEXTURE_2D: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLenum attachment = (GLenum)args[1].value.GetInt32();
        GLenum textarget = (GLenum)args[2].value.GetInt32();
        GLuint texture = (GLuint)args[3].value.GetInt32();
        GLint level = args[4].value.GetInt32();
        glFramebufferTexture2D(target, attachment, textarget, texture, level);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_FRAMEBUFFER_TEXTURE_LAYER: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLenum attachment = (GLenum)args[1].value.GetInt32();
        GLuint texture = (GLuint)args[2].value.GetInt32();
        GLint level = args[3].value.GetInt32();
        GLint layer = args[4].value.GetInt32();
        glFramebufferTextureLayer(target, attachment, texture, level, layer);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_FRAMEBUFFER_RENDERBUFFER: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLenum attachment = (GLenum)args[1].value.GetInt32();
        GLenum renderbuffertarget = (GLenum)args[2].value.GetInt32();
        GLuint renderbuffer = (GLuint)args[3].value.GetInt32();
        glFramebufferRenderbuffer(target, attachment, renderbuffertarget, renderbuffer);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_CHECK_FRAMEBUFFER_STATUS: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLenum status = glCheckFramebufferStatus(target);
        return Value::MakeInt32((int32_t)status, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_GEN_RENDERBUFFERS: {
        GLuint id = 0;
        glGenRenderbuffers(1, &id);
        return Value::MakeUInt32((int32_t)id, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_DELETE_RENDERBUFFERS: {
        GLuint id = (GLuint)args[0].value.GetInt32();
        glDeleteRenderbuffers(1, &id);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BIND_RENDERBUFFER: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLuint rb = (GLuint)args[1].value.GetUInt32();
        glBindRenderbuffer(target, rb);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_RENDERBUFFER_STORAGE: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLenum internalformat = (GLenum)args[1].value.GetInt32();
        GLsizei width = args[2].value.GetInt32();
        GLsizei height = args[3].value.GetInt32();
        glRenderbufferStorage(target, internalformat, width, height);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_RENDERBUFFER_STORAGE_MULTISAMPLE: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLsizei samples = args[1].value.GetInt32();
        GLenum internalformat = (GLenum)args[2].value.GetInt32();
        GLsizei width = args[3].value.GetInt32();
        GLsizei height = args[4].value.GetInt32();
        glRenderbufferStorageMultisample(target, samples, internalformat, width, height);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BLIT_FRAMEBUFFER: {
        GLint srcX0 = args[0].value.GetInt32();
        GLint srcY0 = args[1].value.GetInt32();
        GLint srcX1 = args[2].value.GetInt32();
        GLint srcY1 = args[3].value.GetInt32();
        GLint dstX0 = args[4].value.GetInt32();
        GLint dstY0 = args[5].value.GetInt32();
        GLint dstX1 = args[6].value.GetInt32();
        GLint dstY1 = args[7].value.GetInt32();
        GLbitfield mask = (GLbitfield)args[8].value.GetInt32();
        GLenum filter = (GLenum)args[9].value.GetInt32();
        glBlitFramebuffer(srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1, mask, filter);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_READ_BUFFER: {
        GLenum src = (GLenum)args[0].value.GetInt32();
        glReadBuffer(src);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_READ_PIXELS: {
        GLint x = args[0].value.GetInt32();
        GLint y = args[1].value.GetInt32();
        GLsizei width = args[2].value.GetInt32();
        GLsizei height = args[3].value.GetInt32();
        GLenum format = (GLenum)args[4].value.GetInt32();
        GLenum type = (GLenum)args[5].value.GetInt32();

        void* pixels;
        glReadPixels(x, y, width, height, format, type, pixels);

        
        return Value::MakePointer(1, (uint16)ValueType::VOID_T, pixels);
    }

                                          // ------------------------------
                                          // Shaders and Programs
                                          // ------------------------------
    case GLModuleFunction::TGL_CREATE_SHADER: {
        GLenum type = (GLenum)args[0].value.GetInt32();
        GLuint s = glCreateShader(type);
        return Value::MakeUInt32(s, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_SHADER_SOURCE: {
        GLuint shader = (GLuint)args[0].value.GetUInt32();
        const char* cstr = args[1].value.GetCString();
        glShaderSource(shader, 1, &cstr, nullptr);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_COMPILE_SHADER: {
        GLuint shader = (GLuint)args[0].value.GetInt32();
        glCompileShader(shader);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_DELETE_SHADER: {
        GLuint shader = (GLuint)args[0].value.GetInt32();
        glDeleteShader(shader);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_CREATE_PROGRAM: {
        GLuint p = glCreateProgram();
        return Value::MakeUInt32((int32_t)p, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_ATTACH_SHADER: {
        GLuint prog = (GLuint)args[0].value.GetUInt32();
        GLuint shader = (GLuint)args[1].value.GetUInt32();
        glAttachShader(prog, shader);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_DETACH_SHADER: {
        GLuint prog = (GLuint)args[0].value.GetUInt32();
        GLuint shader = (GLuint)args[1].value.GetUInt32();
        glDetachShader(prog, shader);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_LINK_PROGRAM: {
        GLuint prog = (GLuint)args[0].value.GetUInt32();
        glLinkProgram(prog);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_VALIDATE_PROGRAM: {
        GLuint prog = (GLuint)args[0].value.GetUInt32();
        glValidateProgram(prog);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_DELETE_PROGRAM: {
        GLuint prog = (GLuint)args[0].value.GetUInt32();
        glDeleteProgram(prog);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_USE_PROGRAM: {
        GLuint prog = (GLuint)args[0].value.GetUInt32();
        glUseProgram(prog);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_GET_SHADERIV: {
        GLuint shader = (GLuint)args[0].value.GetUInt32();
        GLenum pname = (GLenum)args[1].value.GetInt32();
        GLint value = 0;
        glGetShaderiv(shader, pname, &value);
        return Value::MakeInt32(value, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_GET_SHADER_INFO_LOG: {
        GLuint shader = (GLuint)args[0].value.GetUInt32();
        GLint len = 0;
        glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &len);
        if (len <= 0) return Value::MakeString("", program->GetStackAllocator());
        std::string log; log.resize(len);
        glGetShaderInfoLog(shader, len, nullptr, &log[0]);
        return Value::MakeString(log, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_GET_PROGRAMIV: {
        GLuint prog = (GLuint)args[0].value.GetUInt32();
        GLenum pname = (GLenum)args[1].value.GetInt32();
        GLint value = 0;
        glGetProgramiv(prog, pname, &value);
        return Value::MakeInt32(value, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_GET_PROGRAM_INFO_LOG: {
        GLuint prog = (GLuint)args[0].value.GetUInt32();
        GLint len = 0;
        glGetProgramiv(prog, GL_INFO_LOG_LENGTH, &len);
        if (len <= 0) return Value::MakeString("", program->GetStackAllocator());
        std::string log; log.resize(len);
        glGetProgramInfoLog(prog, len, nullptr, &log[0]);
        return Value::MakeString(log, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_GET_ACTIVE_UNIFORM: {
        GLuint prog = args[0].value.GetUInt32();
        GLuint index = args[1].value.GetUInt32();
        GLsizei maxLength = args[2].value.GetInt32();

        Value lengthOut = args[3].value.Dereference();
        Value sizeOut = args[4].value.Dereference();
        Value typeOut = args[5].value.Dereference();
        char* nameOut = (char*)args[6].value.data;

        GLsizei length = 0;
        GLint size = 0;
        GLenum type = 0;
        glGetActiveUniform(prog, index, maxLength, &length, &size, &type, nameOut);

        lengthOut.AssignUIntDirect(length);
        sizeOut.AssignUIntDirect(size);
        typeOut.AssignUIntDirect(type);

        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_GET_ACTIVE_ATTRIBUTE: {
        GLuint prog = args[0].value.GetUInt32();
        GLuint index = args[1].value.GetUInt32();
        GLsizei maxLength = args[2].value.GetInt32();

        Value lengthOut = args[3].value.Dereference();
        Value sizeOut = args[4].value.Dereference();
        Value typeOut = args[5].value.Dereference();
        char* nameOut = (char*)args[6].value.data;

        GLsizei length = 0;
        GLint size = 0;
        GLenum type = 0;
        glGetActiveAttrib(prog, index, maxLength, &length, &size, &type, nameOut);

        lengthOut.AssignUIntDirect(length);
        sizeOut.AssignUIntDirect(size);
        typeOut.AssignUIntDirect(type);

        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_GET_UNIFORM_LOCATION: {
        GLuint prog = (GLuint)args[0].value.GetInt32();
        const char* name = args[1].value.GetCString();
        GLint loc = glGetUniformLocation(prog, name);
        return Value::MakeInt32(loc, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_GET_ATTRIB_LOCATION: {
        GLuint prog = (GLuint)args[0].value.GetInt32();
        const char* name = args[1].value.GetCString();
        GLint loc = glGetAttribLocation(prog, name);
        return Value::MakeInt32(loc, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_UNIFORM_1I: {
        GLint loc = args[0].value.GetInt32();
        GLint v0 = args[1].value.GetInt32();
        glUniform1i(loc, v0);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_UNIFORM_1F: {
        GLint loc = args[0].value.GetInt32();
        GLfloat v0 = args[1].value.GetReal32();
        glUniform1f(loc, v0);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_UNIFORM_2F: {
        GLint loc = args[0].value.GetInt32();
        GLfloat v0 = args[1].value.GetReal32();
        GLfloat v1 = args[2].value.GetReal32();
        glUniform2f(loc, v0, v1);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_UNIFORM_3F: {
        GLint loc = args[0].value.GetInt32();
        GLfloat v0 = args[1].value.GetReal32();
        GLfloat v1 = args[2].value.GetReal32();
        GLfloat v2 = args[3].value.GetReal32();
        glUniform3f(loc, v0, v1, v2);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_UNIFORM_4F: {
        GLint loc = args[0].value.GetInt32();
        GLfloat v0 = args[1].value.GetReal32();
        GLfloat v1 = args[2].value.GetReal32();
        GLfloat v2 = args[3].value.GetReal32();
        GLfloat v3 = args[4].value.GetReal32();
        glUniform4f(loc, v0, v1, v2, v3);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_UNIFORM_MATRIX4FV: {
        GLint loc = args[0].value.GetInt32();
        GLsizei count = (GLsizei)args[1].value.GetInt32();
        GLboolean transpose = (GLboolean)(args[2].value.GetBool() ? GL_TRUE : GL_FALSE);
        real32* matrixValue = (real32*)args[3].value.data;
        glUniformMatrix4fv(loc, count, transpose, matrixValue);
        
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BIND_ATTRIB_LOCATION: {
        GLuint prog = (GLuint)args[0].value.GetInt32();
        GLuint index = (GLuint)args[1].value.GetInt32();
        const char* name = args[1].value.GetCString();
        glBindAttribLocation(prog, index, name);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_GET_PROGRAM_BINARY: {
        // TODO: implement program binary retrieval
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_PROGRAM_BINARY: {
        // TODO: program binary loading
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_PROGRAM_PARAMETERI: {
        // TODO: rarely used
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_GET_ACTIVE_UNIFORM_BLOCKIV: {
        GLuint prog = args[0].value.GetUInt32();
        GLuint uniformBlockIndex = args[1].value.GetUInt32();
        GLenum pname = args[2].value.GetInt32();
        Value paramsOut = args[3].value;

        glGetActiveUniformBlockiv(prog, uniformBlockIndex, pname, (GLint*)paramsOut.data);

        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_GET_UNIFORM_BLOCK_INDEX: {
        GLuint prog = (GLuint)args[0].value.GetInt32();
        const char* name = args[1].value.GetCString();
        GLuint idx = glGetUniformBlockIndex(prog, name);
        return Value::MakeInt32((int32_t)idx, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_UNIFORM_BLOCK_BINDING: {
        GLuint prog = (GLuint)args[0].value.GetInt32();
        GLuint blockIndex = (GLuint)args[1].value.GetInt32();
        GLuint bindingPoint = (GLuint)args[2].value.GetInt32();
        glUniformBlockBinding(prog, blockIndex, bindingPoint);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_DISPATCH_COMPUTE: {
        GLuint num_groups_x = (GLuint)args[0].value.GetInt32();
        GLuint num_groups_y = (GLuint)args[1].value.GetInt32();
        GLuint num_groups_z = (GLuint)args[2].value.GetInt32();
        glDispatchCompute(num_groups_x, num_groups_y, num_groups_z);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_DISPATCH_COMPUTE_INDIRECT: {
        // TODO: accept offset into bound buffer
        return Value::MakeNULL();
    }

                                                        // ------------------------------
                                                        // Textures
                                                        // ------------------------------
    case GLModuleFunction::TGL_GEN_TEXTURES: {
        GLuint id = 0;
        glGenTextures(1, &id);
        return Value::MakeUInt32((int32_t)id, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_DELETE_TEXTURES: {
        GLuint id = (GLuint)args[0].value.GetUInt32();
        glDeleteTextures(1, &id);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BIND_TEXTURE: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLuint tex = (GLuint)args[1].value.GetUInt32();
        glBindTexture(target, tex);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_ACTIVE_TEXTURE: {
        GLenum unit = (GLenum)args[0].value.GetInt32();
        glActiveTexture(unit);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_TEX_IMAGE_1D: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLint level = args[1].value.GetInt32();
        GLint internalFormat = args[2].value.GetInt32();
        GLsizei width = args[3].value.GetInt32();
        GLint border = args[4].value.GetInt32();
        GLenum format = args[5].value.GetInt32();
        GLenum type = args[6].value.GetInt32();
        void* data = args[7].value.data;

        glTexImage1D(target, level, internalFormat, width, border, format, type, data);
        return Value::MakeNULL();
    }
    case GLModuleFunction::TGL_TEX_IMAGE_2D: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLint level = args[1].value.GetInt32();
        GLint internalFormat = args[2].value.GetInt32();
        GLsizei width = args[3].value.GetInt32();
        GLsizei height = args[4].value.GetInt32();
        GLint border = args[5].value.GetInt32();
        GLenum format = args[6].value.GetInt32();
        GLenum type = args[7].value.GetInt32();
        void* data = args[8].value.data;

        glTexImage2D(target, level, internalFormat, width, height, border, format, type, data);
        return Value::MakeNULL();
    }
    case GLModuleFunction::TGL_TEX_IMAGE_3D: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLint level = args[1].value.GetInt32();
        GLint internalFormat = args[2].value.GetInt32();
        GLsizei width = args[3].value.GetInt32();
        GLsizei height = args[4].value.GetInt32();
        GLsizei depth = args[5].value.GetInt32();
        GLint border = args[6].value.GetInt32();
        GLenum format = args[7].value.GetInt32();
        GLenum type = args[8].value.GetInt32();
        void* data = args[9].value.data;

        glTexImage3D(target, level, internalFormat, width, height, depth, border, format, type, data);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_TEX_SUB_IMAGE_1D:
    case GLModuleFunction::TGL_TEX_SUB_IMAGE_2D:
    case GLModuleFunction::TGL_TEX_SUB_IMAGE_3D: {
        // TODO: implement subimage functions with data pointer
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_COMPRESSED_TEX_IMAGE_2D:
    case GLModuleFunction::TGL_COMPRESSED_TEX_SUB_IMAGE_2D: {
        // TODO: support compressed texture upload
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_COPY_TEX_SUB_IMAGE_2D: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLint level = args[1].value.GetInt32();
        GLint xoffset = args[2].value.GetInt32();
        GLint yoffset = args[3].value.GetInt32();
        GLint x = args[4].value.GetInt32();
        GLint y = args[5].value.GetInt32();
        GLsizei width = args[6].value.GetInt32();
        GLsizei height = args[7].value.GetInt32();
        glCopyTexSubImage2D(target, level, xoffset, yoffset, x, y, width, height);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_TEX_PARAMETERI: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLenum pname = (GLenum)args[1].value.GetInt32();
        GLint param = args[2].value.GetInt32();
        glTexParameteri(target, pname, param);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_TEX_PARAMETERF: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLenum pname = (GLenum)args[1].value.GetInt32();
        GLfloat param = args[2].value.GetReal32();
        glTexParameterf(target, pname, param);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_TEX_PARAMETERIV:
    case GLModuleFunction::TGL_TEX_PARAMETERFV:
        // TODO: accept array pointer
        return Value::MakeNULL();

    case GLModuleFunction::TGL_GENERATE_MIPMAP: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        glGenerateMipmap(target);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BIND_IMAGE_TEXTURE: {
        // glBindImageTexture(unit, texture, level, layered, layer, access, format)
        GLuint unit = (GLuint)args[0].value.GetInt32();
        GLuint texture = (GLuint)args[1].value.GetInt32();
        GLint level = args[2].value.GetInt32();
        GLboolean layered = args[3].value.GetBool() ? GL_TRUE : GL_FALSE;
        GLint layer = args[4].value.GetInt32();
        GLenum access = (GLenum)args[5].value.GetInt32();
        GLenum format = (GLenum)args[6].value.GetInt32();
        glBindImageTexture(unit, texture, level, layered, layer, access, format);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_TEX_STORAGE_1D: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLsizei levels = args[1].value.GetInt32();
        GLenum internalformat = (GLenum)args[2].value.GetInt32();
        GLsizei width = args[3].value.GetInt32();
        glTexStorage1D(target, levels, internalformat, width);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_TEX_STORAGE_2D: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLsizei levels = args[1].value.GetInt32();
        GLenum internalformat = (GLenum)args[2].value.GetInt32();
        GLsizei width = args[3].value.GetInt32();
        GLsizei height = args[4].value.GetInt32();
        glTexStorage2D(target, levels, internalformat, width, height);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_TEX_STORAGE_3D: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLsizei levels = args[1].value.GetInt32();
        GLenum internalformat = (GLenum)args[2].value.GetInt32();
        GLsizei width = args[3].value.GetInt32();
        GLsizei height = args[4].value.GetInt32();
        GLsizei depth = args[5].value.GetInt32();
        glTexStorage3D(target, levels, internalformat, width, height, depth);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_GET_TEX_IMAGE: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLint level = args[1].value.GetInt32();
        GLenum format = (GLenum)args[2].value.GetInt32();
        GLenum type = (GLenum)args[3].value.GetInt32();
        void* pixels = nullptr; // TODO: supply buffer pointer
        glGetTexImage(target, level, format, type, pixels);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_GET_TEX_PARAMETERIV:
    case GLModuleFunction::TGL_GET_TEX_LEVEL_PARAMETERIV:
        // TODO: implement with output pointer
        return Value::MakeNULL();

        // ------------------------------
        // Uniform Buffer Objects
        // ------------------------------
    case GLModuleFunction::TGL_GET_UNIFORM_INDICES:
    case GLModuleFunction::TGL_GET_ACTIVE_UNIFORMSIV:
    case GLModuleFunction::TGL_BIND_BUFFER_BASE:
    case GLModuleFunction::TGL_BIND_BUFFER_RANGE:
    case GLModuleFunction::TGL_GET_ACTIVE_UNIFORM_BLOCK_NAME:
        // TODO: implement these (some require arrays/pointers)
        return Value::MakeNULL();


        // ------------------------------
        // Queries
        // ------------------------------
    case GLModuleFunction::TGL_GEN_QUERIES: {
        GLuint id = 0;
        glGenQueries(1, &id);
        return Value::MakeUInt32((int32_t)id, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_DELETE_QUERIES: {
        GLuint id = (GLuint)args[0].value.GetInt32();
        glDeleteQueries(1, &id);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BEGIN_QUERY: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLuint id = (GLuint)args[1].value.GetInt32();
        glBeginQuery(target, id);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_END_QUERY: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        glEndQuery(target);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_GET_QUERY_OBJECTUIV: {
        GLuint id = (GLuint)args[0].value.GetInt32();
        GLenum pname = (GLenum)args[1].value.GetInt32();
        GLuint params = 0;
        glGetQueryObjectuiv(id, pname, &params);
        return Value::MakeInt32((int32_t)params, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_GET_QUERY_OBJECTI64V:
    case GLModuleFunction::TGL_GET_QUERY_OBJECTUI64V:
        // TODO: return 64-bit query results
        return Value::MakeNULL();

    case GLModuleFunction::TGL_QUERY_COUNTER: {
        GLuint id = (GLuint)args[0].value.GetInt32();
        GLenum target = (GLenum)args[1].value.GetInt32();
        glQueryCounter(id, target);
        return Value::MakeNULL();
    }

                                            // ------------------------------
                                            // Sync / Fences
                                            // ------------------------------
    case GLModuleFunction::TGL_FENCE_SYNC: {
        GLenum condition = (GLenum)args[0].value.GetInt32();
        GLbitfield flags = (GLbitfield)args[1].value.GetInt32();
        // returns GLsync (opaque pointer)  TODO: represent as int/ptr in Value
        GLsync sync = glFenceSync(condition, flags);
        // Represent as int64 pointer value for now:
        return Value::MakeInt64((int64_t)(uintptr_t)sync, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_DELETE_SYNC: {
        GLsync sync = (GLsync)(uintptr_t)args[0].value.GetInt64();
        glDeleteSync(sync);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_IS_SYNC: {
        GLsync sync = (GLsync)(uintptr_t)args[0].value.GetInt64();
        GLboolean res = glIsSync(sync);
        return Value::MakeBool(res == GL_TRUE, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_CLIENT_WAIT_SYNC: {
        GLsync sync = (GLsync)(uintptr_t)args[0].value.GetInt64();
        GLbitfield flags = (GLbitfield)args[1].value.GetInt32();
        GLuint64 timeout = (GLuint64)args[2].value.GetInt64();
        // returns GLbitfield or enum status  return as int32
        GLint res = (GLint)glClientWaitSync(sync, flags, timeout);
        return Value::MakeInt32(res, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_WAIT_SYNC: {
        GLsync sync = (GLsync)(uintptr_t)args[0].value.GetInt64();
        GLbitfield flags = (GLbitfield)args[1].value.GetInt32();
        GLuint64 timeout = (GLuint64)args[2].value.GetInt64();
        glWaitSync(sync, flags, timeout);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_GET_SYNC_IV: {
        GLsync sync = (GLsync)(uintptr_t)args[0].value.GetInt64();
        GLenum pname = (GLenum)args[1].value.GetInt32();
        // TODO: return integer/vector results; for now return single int
        GLsizei buffSize = args[2].value.GetInt32();
        GLsizei* length = (GLsizei*)args[3].value.data;
        GLint* values = (GLint*)args[4].value.data;
        glGetSynciv(sync, pname, buffSize, length, values);
    }

                                          // ------------------------------
                                          // State Management
                                          // ------------------------------
    case GLModuleFunction::TGL_ENABLE: {
        GLenum cap = (GLenum)args[0].value.GetInt32();
        glEnable(cap);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_DISABLE: {
        GLenum cap = (GLenum)args[0].value.GetInt32();
        glDisable(cap);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_IS_ENABLED: {
        GLenum cap = (GLenum)args[0].value.GetInt32();
        GLboolean r = glIsEnabled(cap);
        return Value::MakeBool(r == GL_TRUE, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_DEPTH_FUNC: {
        GLenum func = (GLenum)args[0].value.GetInt32();
        glDepthFunc(func);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BLEND_FUNC: {
        GLenum sf = (GLenum)args[0].value.GetInt32();
        GLenum df = (GLenum)args[1].value.GetInt32();
        glBlendFunc(sf, df);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BLEND_FUNC_SEPARATE: {
        GLenum srcRGB = (GLenum)args[0].value.GetInt32();
        GLenum dstRGB = (GLenum)args[1].value.GetInt32();
        GLenum srcAlpha = (GLenum)args[2].value.GetInt32();
        GLenum dstAlpha = (GLenum)args[3].value.GetInt32();
        glBlendFuncSeparate(srcRGB, dstRGB, srcAlpha, dstAlpha);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BLEND_EQUATION: {
        GLenum mode = (GLenum)args[0].value.GetInt32();
        glBlendEquation(mode);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BLEND_EQUATION_SEPARATE: {
        GLenum modeRGB = (GLenum)args[0].value.GetInt32();
        GLenum modeAlpha = (GLenum)args[1].value.GetInt32();
        glBlendEquationSeparate(modeRGB, modeAlpha);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_DEPTH_MASK: {
        GLboolean flag = args[0].value.GetBool() ? GL_TRUE : GL_FALSE;
        glDepthMask(flag);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_COLOR_MASK: {
        GLboolean r = args[0].value.GetBool() ? GL_TRUE : GL_FALSE;
        GLboolean g = args[1].value.GetBool() ? GL_TRUE : GL_FALSE;
        GLboolean b = args[2].value.GetBool() ? GL_TRUE : GL_FALSE;
        GLboolean a = args[3].value.GetBool() ? GL_TRUE : GL_FALSE;
        glColorMask(r, g, b, a);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_STENCIL_FUNC: {
        GLenum func = (GLenum)args[0].value.GetInt32();
        GLint ref = args[1].value.GetInt32();
        GLuint mask = (GLuint)args[2].value.GetInt32();
        glStencilFunc(func, ref, mask);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_STENCIL_FUNC_SEPARATE: {
        GLenum face = (GLenum)args[0].value.GetInt32();
        GLenum func = (GLenum)args[1].value.GetInt32();
        GLint ref = args[2].value.GetInt32();
        GLuint mask = (GLuint)args[3].value.GetInt32();
        glStencilFuncSeparate(face, func, ref, mask);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_STENCIL_MASK: {
        GLuint mask = (GLuint)args[0].value.GetInt32();
        glStencilMask(mask);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_STENCIL_MASK_SEPARATE: {
        GLenum face = (GLenum)args[0].value.GetInt32();
        GLuint mask = (GLuint)args[1].value.GetInt32();
        glStencilMaskSeparate(face, mask);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_STENCIL_OP: {
        GLenum sfail = (GLenum)args[0].value.GetInt32();
        GLenum dpfail = (GLenum)args[1].value.GetInt32();
        GLenum dppass = (GLenum)args[2].value.GetInt32();
        glStencilOp(sfail, dpfail, dppass);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_STENCIL_OP_SEPARATE: {
        GLenum face = (GLenum)args[0].value.GetInt32();
        GLenum sfail = (GLenum)args[1].value.GetInt32();
        GLenum dpfail = (GLenum)args[2].value.GetInt32();
        GLenum dppass = (GLenum)args[3].value.GetInt32();
        glStencilOpSeparate(face, sfail, dpfail, dppass);
        return Value::MakeNULL();
    }

                                                  // ------------------------------
                                                  // Sampler Objects
                                                  // ------------------------------
    case GLModuleFunction::TGL_GEN_SAMPLERS: {
        GLuint id = 0;
        glGenSamplers(1, &id);
        return Value::MakeInt32((int32_t)id, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_DELETE_SAMPLERS: {
        GLuint id = (GLuint)args[0].value.GetInt32();
        glDeleteSamplers(1, &id);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_BIND_SAMPLER: {
        GLuint unit = (GLuint)args[0].value.GetInt32();
        GLuint sampler = (GLuint)args[1].value.GetInt32();
        glBindSampler(unit, sampler);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_SAMPLER_PARAMETERI: {
        GLuint sampler = (GLuint)args[0].value.GetInt32();
        GLenum pname = (GLenum)args[1].value.GetInt32();
        GLint param = args[2].value.GetInt32();
        glSamplerParameteri(sampler, pname, param);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_SAMPLER_PARAMETERF: {
        GLuint sampler = (GLuint)args[0].value.GetInt32();
        GLenum pname = (GLenum)args[1].value.GetInt32();
        GLfloat param = args[2].value.GetReal32();
        glSamplerParameterf(sampler, pname, param);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_SAMPLER_PARAMETERIV:
    case GLModuleFunction::TGL_SAMPLER_PARAMETERFV:
        // TODO: accept pointer/array params
        return Value::MakeNULL();

        // ------------------------------
        // Compute Shaders / Barriers
        // ------------------------------
    case GLModuleFunction::TGL_MEMORY_BARRIER: {
        GLbitfield barriers = (GLbitfield)args[0].value.GetInt32();
        glMemoryBarrier(barriers);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_TEXTURE_BARRIER: {
        glTextureBarrier();
        return Value::MakeNULL();
    }

                                              // ------------------------------
                                              // Debug / Logging
                                              // ------------------------------
    case GLModuleFunction::TGL_DEBUG_MESSAGE_CALLBACK: {
        // TODO: registering a callback from script requires function pointer bridging
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_DEBUG_MESSAGE_CONTROL: {
        // TODO: implement properly (accept arrays)
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_PUSH_DEBUG_GROUP: {
        GLenum source = (GLenum)args[0].value.GetInt32();
        GLuint id = (GLuint)args[1].value.GetInt32();
        const char* message = args[2].value.GetCString();
        glPushDebugGroup(source, id, -1, message);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_POP_DEBUG_GROUP: {
        glPopDebugGroup();
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_OBJECT_LABEL: {
        GLenum identifier = (GLenum)args[0].value.GetInt32();
        GLuint name = (GLuint)args[1].value.GetInt32();
        uint32 labelLength = *(uint32*)args[2].value.data;
        const char* label = args[2].value.GetCString();
        glObjectLabel(identifier, name, labelLength, label);
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_OBJECT_PTR_LABEL:
    case GLModuleFunction::TGL_GET_OBJECT_LABEL:
    case GLModuleFunction::TGL_GET_OBJECT_PTR_LABEL:
        // TODO: implement object labeling retrieval and pointer-labeling
        return Value::MakeNULL();

        // ------------------------------
        // VAO / Buffer Introspection
        // ------------------------------
    case GLModuleFunction::TGL_GET_VERTEX_ATTRIB_IV: {
        GLuint index = (GLuint)args[0].value.GetInt32();
        GLenum pname = (GLenum)args[1].value.GetInt32();
        GLint param = 0;
        glGetVertexAttribiv(index, pname, &param);
        return Value::MakeInt32(param, program->GetStackAllocator());
    }

    case GLModuleFunction::TGL_GET_VERTEX_ATTRIB_POINTERV: {
        // TODO: return pointer / offset
        return Value::MakeNULL();
    }

    case GLModuleFunction::TGL_GET_BUFFER_PARAMETERI64V: {
        GLenum target = (GLenum)args[0].value.GetInt32();
        GLenum pname = (GLenum)args[1].value.GetInt32();
        GLint64 value = 0;
        glGetBufferParameteri64v(target, pname, &value);
        return Value::MakeInt64((int64_t)value, program->GetStackAllocator());
    }

                                                       // ------------------------------
                                                       // Fallback default
                                                       // ------------------------------
    default:
        return Value::MakeNULL();
    } // switch
}

Value GLModule::Constant(Program* program, uint16 constant)
{
    switch ((GLModuleConstant)constant)
    {
        // Basic values
    case GLModuleConstant::TGL_ZERO: return Value::MakeInt32(GL_ZERO, program->GetStackAllocator());
    case GLModuleConstant::TGL_ONE: return Value::MakeInt32(GL_ONE, program->GetStackAllocator());
    case GLModuleConstant::TGL_FALSE: return Value::MakeInt32(GL_FALSE, program->GetStackAllocator());
    case GLModuleConstant::TGL_TRUE: return Value::MakeInt32(GL_TRUE, program->GetStackAllocator());

    case GLModuleConstant::TGL_UNSIGNED_BYTE: return Value::MakeInt32(GL_UNSIGNED_BYTE, program->GetStackAllocator());
    case GLModuleConstant::TGL_UNSIGNED_SHORT: return Value::MakeInt32(GL_UNSIGNED_SHORT, program->GetStackAllocator());
    case GLModuleConstant::TGL_UNSIGNED_INT: return Value::MakeInt32(GL_UNSIGNED_INT, program->GetStackAllocator());
    case GLModuleConstant::TGL_UNSIGNED_INT_24_8: return Value::MakeInt32(GL_UNSIGNED_INT_24_8, program->GetStackAllocator());
    case GLModuleConstant::TGL_UNSIGNED_INT_2_10_10_10_REV: return Value::MakeInt32(GL_UNSIGNED_INT_2_10_10_10_REV, program->GetStackAllocator());
    case GLModuleConstant::TGL_FLOAT: return Value::MakeInt32(GL_FLOAT, program->GetStackAllocator());
    case GLModuleConstant::TGL_HALF_FLOAT: return Value::MakeInt32(GL_HALF_FLOAT, program->GetStackAllocator());
    case GLModuleConstant::TGL_INT: return Value::MakeInt32(GL_INT, program->GetStackAllocator());
    case GLModuleConstant::TGL_SHORT: return Value::MakeInt32(GL_SHORT, program->GetStackAllocator());
    case GLModuleConstant::TGL_BYTE: return Value::MakeInt32(GL_BYTE, program->GetStackAllocator());
    case GLModuleConstant::TGL_UNSIGNED_BYTE_3_3_2: return Value::MakeInt32(GL_UNSIGNED_BYTE_3_3_2, program->GetStackAllocator());
    case GLModuleConstant::TGL_UNSIGNED_BYTE_2_3_3_REV: return Value::MakeInt32(GL_UNSIGNED_BYTE_2_3_3_REV, program->GetStackAllocator());

        // Primitives / modes
    case GLModuleConstant::TGL_POINTS: return Value::MakeInt32(GL_POINTS, program->GetStackAllocator());
    case GLModuleConstant::TGL_LINES: return Value::MakeInt32(GL_LINES, program->GetStackAllocator());
    case GLModuleConstant::TGL_LINE_LOOP: return Value::MakeInt32(GL_LINE_LOOP, program->GetStackAllocator());
    case GLModuleConstant::TGL_LINE_STRIP: return Value::MakeInt32(GL_LINE_STRIP, program->GetStackAllocator());
    case GLModuleConstant::TGL_TRIANGLES: return Value::MakeInt32(GL_TRIANGLES, program->GetStackAllocator());
    case GLModuleConstant::TGL_TRIANGLE_STRIP: return Value::MakeInt32(GL_TRIANGLE_STRIP, program->GetStackAllocator());
    case GLModuleConstant::TGL_TRIANGLE_FAN: return Value::MakeInt32(GL_TRIANGLE_FAN, program->GetStackAllocator());
    case GLModuleConstant::TGL_LINES_ADJACENCY: return Value::MakeInt32(GL_LINES_ADJACENCY, program->GetStackAllocator());
    case GLModuleConstant::TGL_LINE_STRIP_ADJACENCY: return Value::MakeInt32(GL_LINE_STRIP_ADJACENCY, program->GetStackAllocator());
    case GLModuleConstant::TGL_TRIANGLES_ADJACENCY: return Value::MakeInt32(GL_TRIANGLES_ADJACENCY, program->GetStackAllocator());
    case GLModuleConstant::TGL_TRIANGLE_STRIP_ADJACENCY: return Value::MakeInt32(GL_TRIANGLE_STRIP_ADJACENCY, program->GetStackAllocator());
    case GLModuleConstant::TGL_PATCHES: return Value::MakeInt32(GL_PATCHES, program->GetStackAllocator());

        // Buffer binding targets
    case GLModuleConstant::TGL_ARRAY_BUFFER: return Value::MakeInt32(GL_ARRAY_BUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_ELEMENT_ARRAY_BUFFER: return Value::MakeInt32(GL_ELEMENT_ARRAY_BUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_COPY_READ_BUFFER: return Value::MakeInt32(GL_COPY_READ_BUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_COPY_WRITE_BUFFER: return Value::MakeInt32(GL_COPY_WRITE_BUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_PIXEL_PACK_BUFFER: return Value::MakeInt32(GL_PIXEL_PACK_BUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_PIXEL_UNPACK_BUFFER: return Value::MakeInt32(GL_PIXEL_UNPACK_BUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_TRANSFORM_FEEDBACK_BUFFER: return Value::MakeInt32(GL_TRANSFORM_FEEDBACK_BUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_UNIFORM_BUFFER: return Value::MakeInt32(GL_UNIFORM_BUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_SHADER_STORAGE_BUFFER: return Value::MakeInt32(GL_SHADER_STORAGE_BUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_DISPATCH_INDIRECT_BUFFER: return Value::MakeInt32(GL_DISPATCH_INDIRECT_BUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_DRAW_INDIRECT_BUFFER: return Value::MakeInt32(GL_DRAW_INDIRECT_BUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_ATOMIC_COUNTER_BUFFER: return Value::MakeInt32(GL_ATOMIC_COUNTER_BUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_QUERY_BUFFER: return Value::MakeInt32(GL_QUERY_BUFFER, program->GetStackAllocator());

        // Usage hints
    case GLModuleConstant::TGL_STATIC_DRAW: return Value::MakeInt32(GL_STATIC_DRAW, program->GetStackAllocator());
    case GLModuleConstant::TGL_DYNAMIC_DRAW: return Value::MakeInt32(GL_DYNAMIC_DRAW, program->GetStackAllocator());
    case GLModuleConstant::TGL_STREAM_DRAW: return Value::MakeInt32(GL_STREAM_DRAW, program->GetStackAllocator());
    case GLModuleConstant::TGL_STATIC_READ: return Value::MakeInt32(GL_STATIC_READ, program->GetStackAllocator());
    case GLModuleConstant::TGL_DYNAMIC_READ: return Value::MakeInt32(GL_DYNAMIC_READ, program->GetStackAllocator());
    case GLModuleConstant::TGL_STREAM_READ: return Value::MakeInt32(GL_STREAM_READ, program->GetStackAllocator());
    case GLModuleConstant::TGL_STATIC_COPY: return Value::MakeInt32(GL_STATIC_COPY, program->GetStackAllocator());
    case GLModuleConstant::TGL_DYNAMIC_COPY: return Value::MakeInt32(GL_DYNAMIC_COPY, program->GetStackAllocator());
    case GLModuleConstant::TGL_STREAM_COPY: return Value::MakeInt32(GL_STREAM_COPY, program->GetStackAllocator());
    case GLModuleConstant::TGL_READ_ONLY: return Value::MakeInt32(GL_READ_ONLY, program->GetStackAllocator());
    case GLModuleConstant::TGL_WRITE_ONLY: return Value::MakeInt32(GL_WRITE_ONLY, program->GetStackAllocator());
    case GLModuleConstant::TGL_READ_WRITE: return Value::MakeInt32(GL_READ_WRITE, program->GetStackAllocator());

        // Texture targets / types
    case GLModuleConstant::TGL_TEXTURE_1D: return Value::MakeInt32(GL_TEXTURE_1D, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE_2D: return Value::MakeInt32(GL_TEXTURE_2D, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE_3D: return Value::MakeInt32(GL_TEXTURE_3D, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE_1D_ARRAY: return Value::MakeInt32(GL_TEXTURE_1D_ARRAY, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE_2D_ARRAY: return Value::MakeInt32(GL_TEXTURE_2D_ARRAY, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE_RECTANGLE: return Value::MakeInt32(GL_TEXTURE_RECTANGLE, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE_CUBE_MAP: return Value::MakeInt32(GL_TEXTURE_CUBE_MAP, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE_CUBE_MAP_ARRAY: return Value::MakeInt32(GL_TEXTURE_CUBE_MAP_ARRAY, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE_BUFFER: return Value::MakeInt32(GL_TEXTURE_BUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE_2D_MULTISAMPLE: return Value::MakeInt32(GL_TEXTURE_2D_MULTISAMPLE, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE_2D_MULTISAMPLE_ARRAY: return Value::MakeInt32(GL_TEXTURE_2D_MULTISAMPLE_ARRAY, program->GetStackAllocator());

        // Texture filtering / wrapping
    case GLModuleConstant::TGL_NEAREST: return Value::MakeInt32(GL_NEAREST, program->GetStackAllocator());
    case GLModuleConstant::TGL_LINEAR: return Value::MakeInt32(GL_LINEAR, program->GetStackAllocator());
    case GLModuleConstant::TGL_NEAREST_MIPMAP_NEAREST: return Value::MakeInt32(GL_NEAREST_MIPMAP_NEAREST, program->GetStackAllocator());
    case GLModuleConstant::TGL_LINEAR_MIPMAP_NEAREST: return Value::MakeInt32(GL_LINEAR_MIPMAP_NEAREST, program->GetStackAllocator());
    case GLModuleConstant::TGL_NEAREST_MIPMAP_LINEAR: return Value::MakeInt32(GL_NEAREST_MIPMAP_LINEAR, program->GetStackAllocator());
    case GLModuleConstant::TGL_LINEAR_MIPMAP_LINEAR: return Value::MakeInt32(GL_LINEAR_MIPMAP_LINEAR, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE_MAG_FILTER: return Value::MakeInt32(GL_TEXTURE_MAG_FILTER, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE_MIN_FILTER: return Value::MakeInt32(GL_TEXTURE_MIN_FILTER, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE_WRAP_S: return Value::MakeInt32(GL_TEXTURE_WRAP_S, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE_WRAP_T: return Value::MakeInt32(GL_TEXTURE_WRAP_T, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE_WRAP_R: return Value::MakeInt32(GL_TEXTURE_WRAP_R, program->GetStackAllocator());
    case GLModuleConstant::TGL_REPEAT: return Value::MakeInt32(GL_REPEAT, program->GetStackAllocator());
    case GLModuleConstant::TGL_CLAMP_TO_EDGE: return Value::MakeInt32(GL_CLAMP_TO_EDGE, program->GetStackAllocator());
    case GLModuleConstant::TGL_MIRRORED_REPEAT: return Value::MakeInt32(GL_MIRRORED_REPEAT, program->GetStackAllocator());
    case GLModuleConstant::TGL_CLAMP_TO_BORDER: return Value::MakeInt32(GL_CLAMP_TO_BORDER, program->GetStackAllocator());

        // Shader types
    case GLModuleConstant::TGL_VERTEX_SHADER: return Value::MakeInt32(GL_VERTEX_SHADER, program->GetStackAllocator());
    case GLModuleConstant::TGL_FRAGMENT_SHADER: return Value::MakeInt32(GL_FRAGMENT_SHADER, program->GetStackAllocator());
    case GLModuleConstant::TGL_GEOMETRY_SHADER: return Value::MakeInt32(GL_GEOMETRY_SHADER, program->GetStackAllocator());
    case GLModuleConstant::TGL_TESS_CONTROL_SHADER: return Value::MakeInt32(GL_TESS_CONTROL_SHADER, program->GetStackAllocator());
    case GLModuleConstant::TGL_TESS_EVALUATION_SHADER: return Value::MakeInt32(GL_TESS_EVALUATION_SHADER, program->GetStackAllocator());
    case GLModuleConstant::TGL_COMPUTE_SHADER: return Value::MakeInt32(GL_COMPUTE_SHADER, program->GetStackAllocator());
    case GLModuleConstant::TGL_COMPILE_STATUS: return Value::MakeInt32(GL_COMPILE_STATUS, program->GetStackAllocator());
    case GLModuleConstant::TGL_LINK_STATUS: return Value::MakeInt32(GL_LINK_STATUS, program->GetStackAllocator());

        // Framebuffer attachments
    case GLModuleConstant::TGL_FRAMEBUFFER: return Value::MakeInt32(GL_FRAMEBUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_READ_FRAMEBUFFER: return Value::MakeInt32(GL_READ_FRAMEBUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_DRAW_FRAMEBUFFER: return Value::MakeInt32(GL_DRAW_FRAMEBUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_RENDERBUFFER: return Value::MakeInt32(GL_RENDERBUFFER, program->GetStackAllocator());
    case GLModuleConstant::TGL_COLOR_ATTACHMENT0: return Value::MakeInt32(GL_COLOR_ATTACHMENT0, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEPTH_ATTACHMENT: return Value::MakeInt32(GL_DEPTH_ATTACHMENT, program->GetStackAllocator());
    case GLModuleConstant::TGL_STENCIL_ATTACHMENT: return Value::MakeInt32(GL_STENCIL_ATTACHMENT, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEPTH_STENCIL_ATTACHMENT: return Value::MakeInt32(GL_DEPTH_STENCIL_ATTACHMENT, program->GetStackAllocator());
    case GLModuleConstant::TGL_FRAMEBUFFER_COMPLETE: return Value::MakeInt32(GL_FRAMEBUFFER_COMPLETE, program->GetStackAllocator());

        // Render states
    case GLModuleConstant::TGL_BLEND: return Value::MakeInt32(GL_BLEND, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEPTH_TEST: return Value::MakeInt32(GL_DEPTH_TEST, program->GetStackAllocator());
    case GLModuleConstant::TGL_CULL_FACE: return Value::MakeInt32(GL_CULL_FACE, program->GetStackAllocator());
    case GLModuleConstant::TGL_SCISSOR_TEST: return Value::MakeInt32(GL_SCISSOR_TEST, program->GetStackAllocator());
    case GLModuleConstant::TGL_STENCIL_TEST: return Value::MakeInt32(GL_STENCIL_TEST, program->GetStackAllocator());

        // Blend / depth / stencil equations
    case GLModuleConstant::TGL_FUNC_ADD: return Value::MakeInt32(GL_FUNC_ADD, program->GetStackAllocator());
    case GLModuleConstant::TGL_FUNC_SUBTRACT: return Value::MakeInt32(GL_FUNC_SUBTRACT, program->GetStackAllocator());
    case GLModuleConstant::TGL_FUNC_REVERSE_SUBTRACT: return Value::MakeInt32(GL_FUNC_REVERSE_SUBTRACT, program->GetStackAllocator());
    case GLModuleConstant::TGL_ONE_MINUS_SRC_ALPHA: return Value::MakeInt32(GL_ONE_MINUS_SRC_ALPHA, program->GetStackAllocator());
    case GLModuleConstant::TGL_ONE_MINUS_DST_ALPHA: return Value::MakeInt32(GL_ONE_MINUS_DST_ALPHA, program->GetStackAllocator());
    case GLModuleConstant::TGL_ONE_MINUS_SRC_COLOR: return Value::MakeInt32(GL_ONE_MINUS_SRC_COLOR, program->GetStackAllocator());
    case GLModuleConstant::TGL_ONE_MINUS_DST_COLOR: return Value::MakeInt32(GL_ONE_MINUS_DST_COLOR, program->GetStackAllocator());

        // Debug output
    case GLModuleConstant::TGL_DEBUG_OUTPUT: return Value::MakeInt32(GL_DEBUG_OUTPUT, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEBUG_OUTPUT_SYNCHRONOUS: return Value::MakeInt32(GL_DEBUG_OUTPUT_SYNCHRONOUS, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEBUG_SOURCE_API: return Value::MakeInt32(GL_DEBUG_SOURCE_API, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEBUG_SOURCE_SHADER_COMPILER: return Value::MakeInt32(GL_DEBUG_SOURCE_SHADER_COMPILER, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEBUG_TYPE_ERROR: return Value::MakeInt32(GL_DEBUG_TYPE_ERROR, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEBUG_SEVERITY_HIGH: return Value::MakeInt32(GL_DEBUG_SEVERITY_HIGH, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEBUG_SEVERITY_MEDIUM: return Value::MakeInt32(GL_DEBUG_SEVERITY_MEDIUM, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEBUG_SEVERITY_LOW: return Value::MakeInt32(GL_DEBUG_SEVERITY_LOW, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEBUG_SEVERITY_NOTIFICATION: return Value::MakeInt32(GL_DEBUG_SEVERITY_NOTIFICATION, program->GetStackAllocator());

    case GLModuleConstant::TGL_COLOR_BUFFER_BIT: return Value::MakeInt32(GL_COLOR_BUFFER_BIT, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEPTH_BUFFER_BIT: return Value::MakeInt32(GL_DEPTH_BUFFER_BIT, program->GetStackAllocator());
    case GLModuleConstant::TGL_STENCIL_BUFFER_BIT: return Value::MakeInt32(GL_STENCIL_BUFFER_BIT, program->GetStackAllocator());

    case GLModuleConstant::TGL_CW: return Value::MakeInt32(GL_CW, program->GetStackAllocator());
    case GLModuleConstant::TGL_CCW: return Value::MakeInt32(GL_CCW, program->GetStackAllocator());

    case GLModuleConstant::TGL_R8: return Value::MakeInt32(GL_R8, program->GetStackAllocator());
    case GLModuleConstant::TGL_R16: return Value::MakeInt32(GL_R16, program->GetStackAllocator());
    case GLModuleConstant::TGL_RG8: return Value::MakeInt32(GL_RG8, program->GetStackAllocator());
    case GLModuleConstant::TGL_RG16: return Value::MakeInt32(GL_RG16, program->GetStackAllocator());
    case GLModuleConstant::TGL_R16F: return Value::MakeInt32(GL_R16F, program->GetStackAllocator());
    case GLModuleConstant::TGL_R32F: return Value::MakeInt32(GL_R32F, program->GetStackAllocator());
    case GLModuleConstant::TGL_RG16F: return Value::MakeInt32(GL_RG16F, program->GetStackAllocator());
    case GLModuleConstant::TGL_RG32F: return Value::MakeInt32(GL_RG32F, program->GetStackAllocator());
    case GLModuleConstant::TGL_RGBA8: return Value::MakeInt32(GL_RGBA8, program->GetStackAllocator());
    case GLModuleConstant::TGL_RGBA16: return Value::MakeInt32(GL_RGBA16, program->GetStackAllocator());
    case GLModuleConstant::TGL_RGBA16F: return Value::MakeInt32(GL_RGBA16F, program->GetStackAllocator());
    case GLModuleConstant::TGL_RGBA32F: return Value::MakeInt32(GL_RGBA32F, program->GetStackAllocator());
    case GLModuleConstant::TGL_SRGB8_ALPHA8: return Value::MakeInt32(GL_SRGB8_ALPHA8, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEPTH_COMPONENT16: return Value::MakeInt32(GL_DEPTH_COMPONENT16, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEPTH_COMPONENT24: return Value::MakeInt32(GL_DEPTH_COMPONENT24, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEPTH_COMPONENT32F: return Value::MakeInt32(GL_DEPTH_COMPONENT32F, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEPTH24_STENCIL8: return Value::MakeInt32(GL_DEPTH24_STENCIL8, program->GetStackAllocator());
    case GLModuleConstant::TGL_DEPTH32F_STENCIL8: return Value::MakeInt32(GL_DEPTH32F_STENCIL8, program->GetStackAllocator());

    case GLModuleConstant::TGL_RGBA:    return Value::MakeInt32(GL_RGBA, program->GetStackAllocator());

    case GLModuleConstant::TGL_TEXTURE0: return Value::MakeInt32(GL_TEXTURE0, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE1: return Value::MakeInt32(GL_TEXTURE1, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE2: return Value::MakeInt32(GL_TEXTURE2, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE3: return Value::MakeInt32(GL_TEXTURE3, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE4: return Value::MakeInt32(GL_TEXTURE4, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE5: return Value::MakeInt32(GL_TEXTURE5, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE6: return Value::MakeInt32(GL_TEXTURE6, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE7: return Value::MakeInt32(GL_TEXTURE7, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE8: return Value::MakeInt32(GL_TEXTURE8, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE9: return Value::MakeInt32(GL_TEXTURE9, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE10: return Value::MakeInt32(GL_TEXTURE10, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE11: return Value::MakeInt32(GL_TEXTURE11, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE12: return Value::MakeInt32(GL_TEXTURE12, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE13: return Value::MakeInt32(GL_TEXTURE13, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE14: return Value::MakeInt32(GL_TEXTURE14, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE15: return Value::MakeInt32(GL_TEXTURE15, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE16: return Value::MakeInt32(GL_TEXTURE16, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE17: return Value::MakeInt32(GL_TEXTURE17, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE18: return Value::MakeInt32(GL_TEXTURE18, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE19: return Value::MakeInt32(GL_TEXTURE19, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE20: return Value::MakeInt32(GL_TEXTURE20, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE21: return Value::MakeInt32(GL_TEXTURE21, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE22: return Value::MakeInt32(GL_TEXTURE22, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE23: return Value::MakeInt32(GL_TEXTURE23, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE24: return Value::MakeInt32(GL_TEXTURE24, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE25: return Value::MakeInt32(GL_TEXTURE25, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE26: return Value::MakeInt32(GL_TEXTURE26, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE27: return Value::MakeInt32(GL_TEXTURE27, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE28: return Value::MakeInt32(GL_TEXTURE28, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE29: return Value::MakeInt32(GL_TEXTURE29, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE30: return Value::MakeInt32(GL_TEXTURE30, program->GetStackAllocator());
    case GLModuleConstant::TGL_TEXTURE31: return Value::MakeInt32(GL_TEXTURE31, program->GetStackAllocator());
    default:
        return Value::MakeInt32(0, program->GetStackAllocator());
    }
}

TypeInfo GLModule::GetFunctionReturnInfo(uint16 function)
{
    Program* program = Program::GetCompiledProgram();
    switch ((GLModuleFunction)function)
    {
    case GLModuleFunction::TGL_INIT: return { (uint16)ValueType::BOOL, 0 };

                                   // === Object Generators (return GLuint) ===
    case GLModuleFunction::TGL_GEN_BUFFERS:
    case GLModuleFunction::TGL_GEN_VERTEX_ARRAYS:
    case GLModuleFunction::TGL_GEN_FRAMEBUFFERS:
    case GLModuleFunction::TGL_GEN_RENDERBUFFERS:
    case GLModuleFunction::TGL_GEN_TEXTURES:
    case GLModuleFunction::TGL_GEN_QUERIES:
    case GLModuleFunction::TGL_GEN_SAMPLERS:
    case GLModuleFunction::TGL_GEN_PROGRAM_PIPELINES:
        return { (uint16)ValueType::UINT32, 0 };

        // === Object Creators ===
    case GLModuleFunction::TGL_CREATE_SHADER:
    case GLModuleFunction::TGL_CREATE_PROGRAM:
    case GLModuleFunction::TGL_GET_UNIFORM_LOCATION:
    case GLModuleFunction::TGL_GET_ATTRIB_LOCATION:
    case GLModuleFunction::TGL_GET_UNIFORM_BLOCK_INDEX:
        return { (uint16)ValueType::UINT32, 0 };

        // === Boolean / Pointer returns ===
    case GLModuleFunction::TGL_MAP_BUFFER:
        return { (uint16)ValueType::VOID_T, 0 };
    case GLModuleFunction::TGL_UNMAP_BUFFER:
    case GLModuleFunction::TGL_IS_ENABLED:
    case GLModuleFunction::TGL_IS_SYNC:
        return { (uint16)ValueType::BOOL, 0 };
    case GLModuleFunction::TGL_FENCE_SYNC:
        return { (uint16)ValueType::UINT8, 1 };

        // === Status / Integer results ===
    case GLModuleFunction::TGL_CHECK_FRAMEBUFFER_STATUS:
    case GLModuleFunction::TGL_CLIENT_WAIT_SYNC:
        return { (uint16)ValueType::INT32, 0 };

    case GLModuleFunction::TGL_READ_PIXELS: {
        return { (uint16)ValueType::VOID_T, 1 };
    }

    case GLModuleFunction::TGL_GET_SHADER_INFO_LOG:
    case GLModuleFunction::TGL_GET_PROGRAM_INFO_LOG:
    {
        return { (uint16)ValueType::STRING, 0 };
    }

    // === Everything else (void) ===
    default:
        return { (uint16)ValueType::VOID_T, 0 };
    }
}

TypeInfo GLModule::GetConstantTypeInfo(uint16 constant)
{
    return { (uint16)ValueType::INT32, 0 };
}