#pragma once

#include "Tokenizer.h"
#include "ID.h"
#include"Template.h"
#include <string>
#include <vector>

class Program;
class Class;
class Function;
struct ASTExpression;
class Parser
{
public:
	Parser(Program* program);

	void Parse(const std::string& path);
private:
	bool ParseImport(Tokenizer* tokenizer);
	bool ParseClass(Tokenizer* tokenizer);
	bool ParseFunction(Tokenizer* tokenizer, Class* cls, ID parentScope);
	bool ParseClassVariable(Tokenizer* tokenizer, Class* cls, ID parentScope, uint64* offset);
	uint16 ParseType(const Token& token) const;
	uint8 ParsePointerLevel(Tokenizer* tokenizer) const;
	bool ParseStatement(Function* function, Tokenizer* tokenizer, ID currentScope);
	ASTExpression* ParseExpression(Tokenizer* tokenizer, ID currentScope);
	ASTExpression* ParseBinaryOpRHS(int32 exprPrec, ASTExpression* lhs, Tokenizer* tokenizer, ID currentScope);
	ASTExpression* ParseUnary(Tokenizer* tokenizer, ID currentScope);
	ASTExpression* ParsePostFix(Tokenizer* tokenizer, ID currentScope);
	ASTExpression* ParsePrimary(Tokenizer* tokenizer, ID currentScope);
	void ParseArguments(Tokenizer* tokenizer, ID currentScope, std::vector<ASTExpression*>& args);
	uint32 ParseArrayLength(Tokenizer* tokenizer);
	void ParseArrayInitializer(Tokenizer* tokenizer, ID currentScope, std::vector<ASTExpression*>& initializeExprs);
	TemplateInstantiation ParseTemplateInstantiation(Tokenizer* tokenizer, Class* cls, TemplateInstantiationCommand* command, bool* templatedType);
	uint32 AddTemplateInstantiationType(const std::string& baseName, const TemplateInstantiation& nested);
private:
	bool WasFileAlreadyParsed(const std::string& file);
private:
	Program* m_Program;
	std::vector<std::string> m_ParsedFiles;
};