#include "Tokenizer.h"

static bool IsEndOfLine(char C)
{
	return C == '\n' || C == '\r';
}

static bool IsWhitespace(char C)
{
	return C == ' ' || C == '\t' || IsEndOfLine(C);
}

void Tokenizer::EatWhitespace()
{
	while (true)
	{
		if (IsWhitespace(at[0]))
			at++;
		else if (at[0] == '/' && at[1] == '/')
		{
			at += 2;
			while (at[0] && !IsEndOfLine(at[0]))
				at++;
		}
		else if (at[0] == '/' && at[1] == '*')
		{
			at += 2;
			while (at[0] && !(at[0] == '*' && at[1] == '/'))
				at++;
			if (at[0] == '*')
				at += 2;
		}
		else
			break;
	}
}

static bool IsNumber(char C)
{
	bool IsDigit = (C >= '0') && (C <= '9');
	return IsDigit;
}

static bool IsAlpha(char C)
{
	return ((C >= 'a') && (C <= 'z')) ||
		((C >= 'A') && (C <= 'Z'));
}

static bool StringEqual(const char* s1, uint32 l1, const char* s2, uint32 l2)
{
	if (l1 != l2)
		return false;

	for (uint32 i = 0; i < l1; i++)
		if (s1[i] != s2[i])
			return false;

	return true;
}

Token Tokenizer::GetToken()
{
	EatWhitespace();

	Token token;
	token.length = 1;
	token.text = at;

	switch (at[0])
	{
	case '\0': { token.type = TokenTypeT::END; at++; } break;
	case '(': { token.type = TokenTypeT::OPEN_PAREN; at++; } break;
	case ')': { token.type = TokenTypeT::CLOSE_PAREN; at++; } break;
	case '{': { token.type = TokenTypeT::OPEN_BRACE; at++; } break;
	case '}': { token.type = TokenTypeT::CLOSE_BRACE; at++; } break;
	case '[': { token.type = TokenTypeT::OPEN_BRACKET; at++; } break;
	case ']': { token.type = TokenTypeT::CLOSE_BRACKET; at++; } break;
	case '*': {
		token.type = TokenTypeT::ASTERISK; at++;
		if (at[0] == '=')
		{
			token.type = TokenTypeT::TIMES_EQUALS;
			at++;
		}
	} break;
	case '+': {
		token.type = TokenTypeT::PLUS; at++;
		if (at[0] == '+')
		{
			token.type = TokenTypeT::PLUS_PLUS;
			at++;
		}
		else if (at[0] == '=')
		{
			token.type = TokenTypeT::PLUS_EQUALS;
			at++;
		}
	} break;
	case '%': {
		token.type = TokenTypeT::MOD; at++;
		if (at[0] == '=')
		{
			token.type = TokenTypeT::MOD_EQUALS;
			at++;
		}
	} break;
	case '/': {
		token.type = TokenTypeT::SLASH; at++;
		if (at[0] == '=')
		{
			token.type = TokenTypeT::DIVIDE_EQUALS;
			at++;
		}
	} break;
	case '-': {
		token.type = TokenTypeT::MINUS; at++;
		if (at[0] == '-')
		{
			token.type = TokenTypeT::MINUS_MINUS;
			at++;
		}
		else if (at[0] == '=')
		{
			token.type = TokenTypeT::MINUS_EQUALS;
			at++;
		}
		else if (at[0] == '>')
		{
			token.type = TokenTypeT::ARROW;
			at++;
		}
		else if (IsNumber(at[0]))
		{
			at++;
			token.type = TokenTypeT::NUMBER_LITERAL;
			bool HasDecimal = false;
			uint32 length = 2;
			while (true)
			{
				if (IsNumber(at[0]))
				{
					at++;
				}
				else if (at[0] == '.')
				{
					if (HasDecimal)
					{
						//ERROR
						at++;
					}

					HasDecimal = true;
					at++;
				}
				else
				{
					break;
				}

				length++;
			}

			token.length = length;
		}
	} break;
	case ':': { token.type = TokenTypeT::COLON; at++; } break;
	case ';': { token.type = TokenTypeT::SEMICOLON; at++; } break;
	case '.': { token.type = TokenTypeT::DOT; at++; } break;
	case ',': { token.type = TokenTypeT::COMMA; at++; } break;
	case '~': { token.type = TokenTypeT::TILDE; at++; } break;
	case '<': {
		token.type = TokenTypeT::LESS; at++;
		if (at[0] == '=')
		{
			token.type = TokenTypeT::LESS_EQUALS;
			at++;
		}
		else if (at[0] == '<')
		{
			token.type = TokenTypeT::BITSHIFT_LEFT;
			at++;
		}
	} break;
	case '>': {
		token.type = TokenTypeT::GREATER; at++;
		if (at[0] == '=')
		{
			token.type = TokenTypeT::GREATER_EQUALS;
			at++;
		}
		else if (at[0] == '>')
		{
			token.type = TokenTypeT::BITSHIFT_RIGHT;
			at++;
		}
	} break;
	case '=': {
		token.type = TokenTypeT::EQUALS; at++;
		if (at[0] == '=')
		{
			token.type = TokenTypeT::EQUALS_EQUALS;
			at++;
		}
	} break;
	case '!': {
		token.type = TokenTypeT::NOT; at++;
		if (at[0] == '=')
		{
			token.type = TokenTypeT::NOT_EQUAL;
			at++;
		}
	} break;
	case '&': {
		token.type = TokenTypeT::AND; at++;
		if (at[0] == '&')
		{
			token.type = TokenTypeT::LOGICAL_AND;
			at++;
		}
	} break;
	case '|': {
		token.type = TokenTypeT::PIPE; at++;
		if (at[0] == '|')
		{
			token.type = TokenTypeT::LOGICAL_OR;
			at++;
		}
	} break;
	case '\'': {
		at++; // skip opening '
		token.type = TokenTypeT::CHAR_LITERAL;
		token.text = at;

		// Handle escape characters
		if (at[0] == '\\' && at[1] != '\0') {
			token.length = 2;       // e.g. '\n', '\t', '\\', etc.
			at += 2;
		}
		else {
			token.length = 1;       // normal character like 'a'
			at++;
		}

		if (at[0] == '\'')
			at++; // skip closing '
	} break;
	case '"': {
		at++;
		token.type = TokenTypeT::STRING_LITERAL;
		token.text = at;
		while (at[0] != '"' &&
			at[0] != '\0')
		{
			if (at[0] == '\\' && at[1] != '\0')
				at++;

			at++;
		}
		token.length = at - token.text;
		if (at[0] == '"')
			at++;
	} break;
	default: {
		if (IsAlpha(at[0]) || at[0] == '_')
		{
			at++;
			token.type = TokenTypeT::IDENTIFIER;
			while (IsAlpha(at[0]) || IsNumber(at[0]) || at[0] == '_')
				at++;
			token.length = at - token.text;

			if (StringEqual(token.text, token.length, "class", 5))
				token.type = TokenTypeT::CLASS;
			else if (StringEqual(token.text, token.length, "enum", 4))
				token.type = TokenTypeT::ENUM;
			else if (StringEqual(token.text, token.length, "Import", 6))
				token.type = TokenTypeT::IMPORT;
			else if (StringEqual(token.text, token.length, "throw", 5))
				token.type = TokenTypeT::THROW;
			else if (StringEqual(token.text, token.length, "catch", 5))
				token.type = TokenTypeT::CATCH;
			else if (StringEqual(token.text, token.length, "void", 4))
				token.type = TokenTypeT::VOID_T;
			else if (StringEqual(token.text, token.length, "bool", 4))
				token.type = TokenTypeT::BOOL;
			else if (StringEqual(token.text, token.length, "char", 4))
				token.type = TokenTypeT::CHAR;
			else if (StringEqual(token.text, token.length, "uint8", 5))
				token.type = TokenTypeT::UINT8;
			else if (StringEqual(token.text, token.length, "uint16", 6))
				token.type = TokenTypeT::UINT16;
			else if (StringEqual(token.text, token.length, "uint32", 6))
				token.type = TokenTypeT::UINT32;
			else if (StringEqual(token.text, token.length, "uint64", 6))
				token.type = TokenTypeT::UINT64;
			else if (StringEqual(token.text, token.length, "int8", 4))
				token.type = TokenTypeT::INT8;
			else if (StringEqual(token.text, token.length, "int16", 5))
				token.type = TokenTypeT::INT16;
			else if (StringEqual(token.text, token.length, "int32", 5))
				token.type = TokenTypeT::INT32;
			else if (StringEqual(token.text, token.length, "int64", 5))
				token.type = TokenTypeT::INT64;
			else if (StringEqual(token.text, token.length, "real32", 6))
				token.type = TokenTypeT::REAL32;
			else if (StringEqual(token.text, token.length, "real64", 6))
				token.type = TokenTypeT::REAL64;
			else if (StringEqual(token.text, token.length, "public", 6))
				token.type = TokenTypeT::PUBLIC;
			else if (StringEqual(token.text, token.length, "private", 7))
				token.type = TokenTypeT::PRIVATE;
			else if (StringEqual(token.text, token.length, "static", 6))
				token.type = TokenTypeT::STATIC;
			else if (StringEqual(token.text, token.length, "return", 6))
				token.type = TokenTypeT::RETURN;
			else if (StringEqual(token.text, token.length, "operator", 8))
				token.type = TokenTypeT::OPERATOR;
			else if (StringEqual(token.text, token.length, "if", 2))
				token.type = TokenTypeT::IF;
			else if (StringEqual(token.text, token.length, "else", 4))
				token.type = TokenTypeT::ELSE;
			else if (StringEqual(token.text, token.length, "for", 3))
				token.type = TokenTypeT::FOR;
			else if (StringEqual(token.text, token.length, "while", 5))
				token.type = TokenTypeT::WHILE;
			else if (StringEqual(token.text, token.length, "sizeof", 6))
				token.type = TokenTypeT::SIZE_OF;
			else if (StringEqual(token.text, token.length, "new", 3))
				token.type = TokenTypeT::NEW;
			else if (StringEqual(token.text, token.length, "delete", 6))
				token.type = TokenTypeT::DELETE_T;
			else if (StringEqual(token.text, token.length, "string", 6))
				token.type = TokenTypeT::STRING;
			else if (StringEqual(token.text, token.length, "true", 4))
				token.type = TokenTypeT::TRUE_T;
			else if (StringEqual(token.text, token.length, "false", 5))
				token.type = TokenTypeT::FALSE_T;
			else if (StringEqual(token.text, token.length, "null", 4))
				token.type = TokenTypeT::NULLPTR;
			else if (StringEqual(token.text, token.length, "this", 4))
				token.type = TokenTypeT::THIS;
			else if (StringEqual(token.text, token.length, "native_offset", 13))
				token.type = TokenTypeT::NATIVE_OFFSET;
			else if (StringEqual(token.text, token.length, "native", 6))
				token.type = TokenTypeT::NATIVE;
			else if (StringEqual(token.text, token.length, "template", 8))
				token.type = TokenTypeT::TEMPLATE;
			else if (StringEqual(token.text, token.length, "native_size", 11))
				token.type = TokenTypeT::NATIVE_SIZE;
			else if (StringEqual(token.text, token.length, "strlen", 6))
				token.type = TokenTypeT::STRLEN;
			else if (StringEqual(token.text, token.length, "break", 5))
				token.type = TokenTypeT::BREAK;
			else if (StringEqual(token.text, token.length, "continue", 8))
				token.type = TokenTypeT::CONTINUE;
			else if (StringEqual(token.text, token.length, "inherit", 7))
				token.type = TokenTypeT::INHERIT;
			else if (StringEqual(token.text, token.length, "virtual", 7))
				token.type = TokenTypeT::VIRTUAL;
			else if (StringEqual(token.text, token.length, "str_to_int", 10))
				token.type = TokenTypeT::STR_TO_INT;
			else if (StringEqual(token.text, token.length, "int_to_str", 10))
				token.type = TokenTypeT::INT_TO_STR;
			else if (StringEqual(token.text, token.length, "offsetof", 8))
				token.type = TokenTypeT::OFFSETOF;
		}
		else if (IsNumber(at[0]))
		{
			at++;
			token.type = TokenTypeT::NUMBER_LITERAL;
			bool HasDecimal = false;
			while (true)
			{
				if (IsNumber(at[0]))
				{
					at++;
				}
				else if (at[0] == '.')
				{
					if (HasDecimal)
					{
						//ERROR
						at++;
					}

					HasDecimal = true;
					at++;
				}
				else
				{
					break;
				}
			}

			token.length = at - token.text;
		}
		else
		{
			at++;
			token.type = TokenTypeT::UNKNOWN;
		}
	} break;
	}

	return token;
}

bool Tokenizer::Expect(TokenTypeT type, Token* token)
{
	Token t = GetToken();
	if (token)*token = t;
	return t.type != type;
}

Token Tokenizer::PeekToken()
{
	char* prev = at;
	Token token = GetToken();
	at = prev;
	return token;
}

void Tokenizer::SetPeek(const Token& peek)
{
	at = peek.text;
}

bool Tokenizer::IsTokenPrimitiveType(const Token& token)
{
	switch (token.type)
	{
	case TokenTypeT::UINT8:
	case TokenTypeT::UINT16:
	case TokenTypeT::UINT32:
	case TokenTypeT::UINT64:
	case TokenTypeT::INT8:
	case TokenTypeT::INT16:
	case TokenTypeT::INT32:
	case TokenTypeT::INT64:
	case TokenTypeT::REAL32:
	case TokenTypeT::REAL64:
	case TokenTypeT::BOOL:
	case TokenTypeT::CHAR:
	case TokenTypeT::STRING:
	case TokenTypeT::VOID_T:
		return true;

	default:
		return false;
	}
}
