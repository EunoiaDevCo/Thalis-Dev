#include "Parser.h"
#include "Program.h"
#include "Class.h"
#include "Memory/Memory.h"

int main()
{
	uint32** array = new uint32*[100];
	delete[] array;

	Program program;
	Parser parser(&program);
	parser.Parse("Main.tls");

	uint32 pc = program.GetCodeSize();
	ID mainClassID = program.GetClassIDWithMainFunction();
	std::vector<ASTExpression*> args;
	program.AddStaticFunctionCallCommand(mainClassID, program.GetClass(mainClassID)->GetFunctionID("Main", args), false);
	program.AddEndCommand();

	program.ExecuteProgram(pc);

	uint64 marker = program.GetStackAllocator()->GetMarker();
	uint64 maxStackUsage = program.GetStackAllocator()->GetMaxUsage();
	uint64 numHeapAllocs = program.GetHeapAllocator()->GetNumAllocs();
	uint64 numHeapFrees = program.GetHeapAllocator()->GetNumFrees();
	uint32 stackSize = program.GetStackSize();

	std::cout << "Stack marker: " << Memory::BytesToKB(marker) << "KB" << std::endl;
	std::cout << "Max stack usage: " << Memory::BytesToKB(maxStackUsage) << "KB" << std::endl;
	std::cout << "Num heap allocs: " << numHeapAllocs << std::endl;
	std::cout << "Num heap frees: " << numHeapFrees << std::endl;
	std::cout << "Stack size: " << stackSize << std::endl;

	while (true);

	return 0;
}