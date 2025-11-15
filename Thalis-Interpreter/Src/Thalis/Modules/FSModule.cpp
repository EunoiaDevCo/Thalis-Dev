#include "FSModule.h"
#include "../Program.h"
#include <fstream>

static std::ifstream g_OpenFiles[16];
static std::vector<uint32> g_FreeFileIDs;
static uint32 g_NexFileID = 1;

static char m_FileLine[4096];
static uint16 m_StringTypeID = INVALID_ID;

bool FSModule::Init(Program* program)
{
    m_StringTypeID = program->GetClassID("String");
    return m_StringTypeID != INVALID_ID;
}

Value FSModule::CallFunction(Program* program, uint16 function, const std::vector<FunctionArg>& args)
{
    switch ((FSModuleFunction)function)
    {
    case FSModuleFunction::READ_TEXT_FILE: {
        FILE* file = fopen(args[0].value.GetCString(), "rb");
        if (!file)
            return Value::MakeNULL((uint16)ValueType::CHAR, 1);

        fseek(file, 0, SEEK_END);
        uint64 size = ftell(file);
        fseek(file, 0, SEEK_SET);
        uint8* data = (uint8*)program->GetHeapAllocator()->Alloc(size + 1 + sizeof(ArrayHeader));
        ArrayHeader* header = (ArrayHeader*)data;
        header->elementPointerLevel = 0;
        header->length = size;

        char* characters = (char*)(data + sizeof(ArrayHeader));
        fread(characters, 1, size, file);
        characters[size] = 0;
        Value returnString = Value::MakeObject(program, m_StringTypeID, program->GetStackAllocator());
        *(void**)returnString.data = characters;
        *(uint32*)((uint8*)returnString.data + sizeof(char*)) = size;
        *(uint32*)((uint8*)returnString.data + sizeof(char*) + sizeof(uint32)) = size + 1;

        fclose(file);
        return returnString;
    } break;
    case FSModuleFunction::READ_BINARY_FILE: {
        FILE* file = fopen(args[0].value.GetCString(), "rb");
        if (!file)
            return Value::MakeNULL((uint16)ValueType::UINT8, 1);

        fseek(file, 0, SEEK_END);
        uint64 size = ftell(file);
        fseek(file, 0, SEEK_SET);
        uint8* bytes = (uint8*)program->GetHeapAllocator()->Alloc(size + sizeof(ArrayHeader));
        ArrayHeader* header = (ArrayHeader*)bytes;
        header->elementPointerLevel = 0;
        header->length = size;
        fread(bytes, 1, size, file);
        fclose(file);

        Value data = Value::MakePointer(1, (uint16)ValueType::UINT8, bytes + sizeof(ArrayHeader));
        return data;
    } break;
    case FSModuleFunction::OPEN_FILE: {
        std::string path = args[0].value.GetString();
        int32 lastSlashIndex = path.find_last_of("/");
        std::string relPath = lastSlashIndex == -1 ? "" : path.substr(0, lastSlashIndex + 1);

        uint32 fileID;
        if (!g_FreeFileIDs.empty())
        {
            fileID = g_FreeFileIDs.back();
            g_FreeFileIDs.pop_back();
        }
        else
        {
            fileID = g_NexFileID++;
        }

        g_OpenFiles[fileID - 1].open(path.c_str());

        if (!g_OpenFiles[fileID - 1].good())
        {
            g_FreeFileIDs.push_back(fileID);
            return Value::MakeUInt32(0, program->GetStackAllocator());
        }

        return Value::MakeUInt32(fileID, program->GetStackAllocator());
    } break;
    case FSModuleFunction::CLOSE_FILE: {
        uint32 fileID = args[0].value.GetUInt32();
        g_OpenFiles[fileID - 1].close();
        g_FreeFileIDs.push_back(fileID);
        return Value::MakeNULL();
    } break;
    case FSModuleFunction::READ_LINE: {
        uint32 fileID = args[0].value.GetUInt32();
        if (!g_OpenFiles[fileID - 1].getline(m_FileLine, 4096))
            return Value::MakeBool(false, program->GetStackAllocator());

        uint32 length = strlen(m_FileLine);
        uint8* data = (uint8*)program->GetHeapAllocator()->Alloc(length + 1 + sizeof(ArrayHeader));
        char* chars = (char*)(data + sizeof(ArrayHeader));
        memcpy(chars, m_FileLine, length);
        chars[length] = 0;

        Value outString = args[1].value;
        void* outChars = (void**)outString.data;
        delete((uint8*)outChars - sizeof(ArrayHeader));
        *(void**)outString.data = chars;
        *(uint32*)((uint8*)outString.data + sizeof(char*)) = length;
        *(uint32*)((uint8*)outString.data + sizeof(char*) + sizeof(uint32)) = length + 1;

        return Value::MakeBool(true, program->GetStackAllocator());
    } break;
    }

    return Value::MakeNULL();
}

Value FSModule::Constant(Program* program, uint16 constant)
{
    return Value::MakeNULL();
}

TypeInfo FSModule::GetFunctionReturnInfo(uint16 function)
{
    switch ((FSModuleFunction)function)
    {
    case FSModuleFunction::READ_TEXT_FILE: return TypeInfo(m_StringTypeID, 0);
    case FSModuleFunction::READ_BINARY_FILE: return TypeInfo((uint16)ValueType::UINT8, 1);
    case FSModuleFunction::OPEN_FILE: return TypeInfo((uint16)ValueType::UINT32, 0);
    case FSModuleFunction::CLOSE_FILE: return TypeInfo((uint16)ValueType::VOID_T, 0);
    case FSModuleFunction::READ_LINE: return TypeInfo((uint16)ValueType::BOOL, 0);
    }
}

TypeInfo FSModule::GetConstantTypeInfo(uint16 constant)
{
    return TypeInfo(INVALID_ID, 0);
}
