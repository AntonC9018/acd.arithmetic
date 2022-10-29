import std.stdio;


enum OperatorArity
{
	unary = 1,
	binary = 2,
}

enum OperatorAssociativity
{
	left,
	right,
}

struct Operator
{
	string name;
	OperatorArity arity;
	OperatorAssociativity associativity;
	int precedence;
}

struct StreamPosition
{
	size_t line;
	size_t column;
}

enum TokenKind
{
	operator,
	identifier,
	integerLiteral,
	floatLiteral,
	other,
}

struct Token
{
	StreamPosition startPosition;
	StreamPosition endPosition;
	string name;
	TokenKind kind;
	union 
	{
		size_t operatorIndex;
		long integer;
		double floating;
	}
}


struct DynamicArray(T)
{
	T[] data;
	size_t length;

	this(size_t initialCapacity)
	{
		data = new T[initialCapacity];
		length = 0;
	}

	void opOpAssign(string op)(auto ref T value) if (op == "~")
	{
		if (length == data.length)
			data.length *= 2;
		data[length++] = value;
	}

	// reroute indexing to the underlying array
	ref T opIndex(size_t index)
	{
		assert(index < length);
		return data[index];
	}

	ref T opIndexAssign(T value, size_t index)
	{
		assert(index < length);
		return data[index] = value;
	}

	void clear()
	{
		length = 0;
	}

	T[] sliceTemp()
	{
		return data[0 .. length];
	}	
}

/// Represents a range of tokens that lexes an input mathematical expression,
/// passed in as an input character range.
/// The token types are given as an array in the constructor,
/// and the token types are represented by indices into that array.
struct ArithmeticLexerRange(TRange)
{
	import std.uni;
	import std.range;
	import std.algorithm;

	private const(string)[] _operators;
	private TRange _input;
	private TRange _currentLine;
	private StreamPosition _currentPosition;
	private Token _currentToken = Token.init;
	private bool _empty = false;

	private struct MatchingOperator
	{
		size_t index;
		string name;
	}
	private DynamicArray!MatchingOperator _matchingOperatorsCache;

	bool empty() const
	{
		return _empty;
	}
	
	Token front() const
	{
		assert(!empty);
		return _currentToken;
	}

	/// Matches the next token in the stream.
	void popFront()
	{
		assert(!empty);
		if (_input.empty)
		{
			_empty = true;
			return;
		}
		void skipWhitespace()
		{
			while (!_input.empty)
			{
				switch (_input.front)
				{
					default:
						return;

					case '\r':
					case ' ':
					case '\t':
					{
						_currentPosition.column++;
						_input.popFront();
						break;
					}
					case '\n':
					{
						_currentPosition.line++;
						_currentPosition.column = 0;
						_input.popFront();
						_currentLine = _input.save;
						break;
					}
				}
			}
		}
		skipWhitespace();

		if (_input.empty)
			return;

		void popSingleCharacter()
		{
			_currentPosition.column++;
			_input.popFront();
		}

		size_t startColumn = _currentPosition.column;
		const f = _input.front;
		popSingleCharacter();
		_currentToken.startPosition = _currentPosition;

		if (isAlpha(f) || f == '_')
		{
			_currentToken.kind = TokenKind.identifier;
			writeln("Ident ", f);

			while (!_input.empty
				&& (isAlphaNum(_input.front) || _input.front == '_'))
			{
				popSingleCharacter();
			}
		}
		else if (isNumber(f))
		{
			long wholePart = f - '0';
			double fractionalPart = 0;
			// long numFractionalDigits = 0;
			while (!_input.empty && isNumber(_input.front))
			{
				wholePart *= 10;
				wholePart += _input.front - '0';
				popSingleCharacter();
			}
			if (!_input.empty && _input.front == '.')
			{
				popSingleCharacter();
				while (!_input.empty && isNumber(_input.front))
				{
					// numFractionalDigits++;
					fractionalPart += _input.front - '0';
					fractionalPart /= 10;
					popSingleCharacter();
				}
				_currentToken.kind = TokenKind.floatLiteral;
				_currentToken.floating = cast(double) wholePart + fractionalPart;
			}
			else
			{
				_currentToken.kind = TokenKind.integerLiteral;
				_currentToken.integer = wholePart;
			}
		}
		else
		{
			// Find the operator that fully matches the next characters of the input range.
			// If no operator matches, then the next character is an error.
			_matchingOperatorsCache.length = 0;
			foreach (opIndex, opName; _operators.enumerate.filter!(op => op.value[0] == f))
				_matchingOperatorsCache ~= MatchingOperator(opIndex, opName);

			if (_matchingOperatorsCache.length == 0)
			{
				_currentToken.kind = TokenKind.other;
			}
			else
			{
				size_t opIndex = _matchingOperatorsCache[0].index;
				{
					// Matches the longest possible operator.
					if (_operators[opIndex].length > 1)
					{
						size_t index = 1;
						while (!_input.empty)
						{
							auto data = _matchingOperatorsCache.sliceTemp;
							_matchingOperatorsCache.length = 0;
							foreach (op; data[])
							{
								if (op.name.length > index && op.name[index] == _input.front)
									_matchingOperatorsCache ~= op;
							}
							if (_matchingOperatorsCache.length == 0)
								break;
							index++;
							opIndex = _matchingOperatorsCache[0].index;
							popSingleCharacter();
						}
					}

					_currentToken.kind = TokenKind.operator;
					_currentToken.operatorIndex = opIndex;
				}
			}
		}
		_currentToken.endPosition = _currentPosition;
		_currentToken.name = _currentLine[startColumn .. _currentPosition.column];
		skipWhitespace();
	}
}

auto lexArithmetic(TRange)(
	TRange range,
	const(string[]) operators,
	StreamPosition startPosition = StreamPosition.init)
{
	auto lexer = ArithmeticLexerRange!(TRange)(operators, range, range, startPosition);
	lexer._matchingOperatorsCache = typeof(lexer._matchingOperatorsCache)(4);
	lexer.popFront();
	return lexer;
}

struct BufferingArithmeticLexer(TLexerRange)
{
	private TLexerRange _lexerRange;
	private Token[] _buffer = null;
	private ptrdiff_t _bufferPosition = -1;

	Token* currentToken()
	{
		if (_bufferPosition >= _buffer.length)
			return null;
		return &_buffer[_bufferPosition];
	}

	private Token* getToken(int lookahead)
	{
		if (_bufferPosition + lookahead >= _buffer.length)
			return null;
		return &_buffer[_bufferPosition + lookahead];
	}

	Token* next()
	{
		pop();
		return currentToken();
	}

	void pop()
	{
		popN(1);
	}

	void popN(size_t n)
	{
		bufferNextNTokens(n);
		_bufferPosition += n;
	}

	Token* peek(int lookahead = 1)
	{
		bufferNextNTokens(lookahead);
		return getToken(lookahead);
	}

	private void bufferNextNTokens(size_t tokenCount)
	{
		while (_buffer.length <= _bufferPosition + tokenCount
			&& !_lexerRange.empty)
		{
			void bufferNextToken()
			{
				_buffer ~= _lexerRange.front;
				_lexerRange.popFront();
			}
			bufferNextToken();
		}
	}
}

BufferingArithmeticLexer!TLexerRange bufferArithmeticLexer(TLexerRange)(TLexerRange lexerRange)
{
	auto lexer = BufferingArithmeticLexer!TLexerRange(lexerRange);
	return lexer;
}

void writeTokenInfo(Token* token, const(string)[] operators)
{
	writeln("Token name: ", token.name);
	writeln("Token kind: ", token.kind);
	final switch (token.kind)
	{
		case TokenKind.integerLiteral:
			writeln(token.integer);
			break;
		case TokenKind.floatLiteral:
			writeln(token.floating);
			break;
		case TokenKind.operator:
			writeln(operators[token.operatorIndex]);
			break;
		case TokenKind.identifier:
			writeln(token.name);
			break;
		case TokenKind.other:
			writeln(token.name);
			break;
	}
	writeln();
}

void main()
{
	const operators = ["+", "-", "*", "/"];
	const input = "(1 + 2) * [3 / a]";
	// foreach (token; lexArithmetic(input, operators))
	// 	writeTokenInfo(&token, operators);

	auto underlyingLexer = lexArithmetic(input, operators);
	auto lexer = bufferArithmeticLexer(underlyingLexer);
	{
		auto next = lexer.next();
		while (next !is null)
		{
			writeTokenInfo(next, operators);
			next = lexer.next();
		}
	}
}
