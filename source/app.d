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
	string text;
	TokenKind kind;
	union
	{
		Operator* operator;
		long integer;
		double floating;
	}
}

// TODO: use allocator here, oor prealloc statically.
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

private struct ArithmeticLexerRange(TRange)
{
	import std.uni;
	import std.range;
	import std.algorithm;

	private Operator[] _operators;
	private TRange _input;
	private TRange _currentLine;
	private StreamPosition _currentPosition;
	private Token _currentToken = Token.init;
	private bool _empty = false;
	private DynamicArray!(Operator*) _matchingOperatorsCache;

	bool empty() const
	{
		return _empty;
	}
	
	Token front()
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
			_matchingOperatorsCache.length = 0;
			foreach (ref op; _operators.filter!(op => op.name[0] == f))
				_matchingOperatorsCache ~= &op;

			if (_matchingOperatorsCache.length == 0)
			{
				_currentToken.kind = TokenKind.other;
			}
			else
			{
				Operator* op = _matchingOperatorsCache[0];
				{
					// Matches the longest possible operator.
					if (_matchingOperatorsCache.data[].any!(op => op.name.length > 1))
					{
						size_t index = 1;
						while (!_input.empty)
						{
							auto data = _matchingOperatorsCache.sliceTemp;
							_matchingOperatorsCache.length = 0;
							foreach (op1; data[])
							{
								if (op1.name.length > index && op1.name[index] == _input.front)
									_matchingOperatorsCache ~= op1;
							}
							if (_matchingOperatorsCache.length == 0)
								break;
							index++;
							op = _matchingOperatorsCache[0];
							popSingleCharacter();
						}
					}

					_currentToken.kind = TokenKind.operator;
					_currentToken.operator = op;
				}
			}
		}
		_currentToken.endPosition = _currentPosition;
		_currentToken.text = _currentLine[startColumn .. _currentPosition.column];
		skipWhitespace();
	}
}

auto lexArithmetic(TRange)(
	TRange range,
	Operator[] operators,
	StreamPosition startPosition = StreamPosition.init)
{
	auto lexer = ArithmeticLexerRange!(TRange)(operators, range, range, startPosition);
	lexer._matchingOperatorsCache = typeof(lexer._matchingOperatorsCache)(4);
	lexer.popFront();
	return lexer;
}

import containers.cyclicbuffer;
import std.experimental.allocator;
import std.experimental.allocator.mallocator;
import containers.internal.mixins : AllocatorState;
import std.experimental.allocator.common : stateSize;
import std.algorithm : min;

private struct BufferingArithmeticLexer(TLexerRange,
	TAllocator = Mallocator,
	TCyclicTokenPointerBuffer = CyclicBuffer!(Token*))
{
	private TLexerRange _lexerRange;
	private mixin AllocatorState!TAllocator;
	private TCyclicTokenPointerBuffer _buffer;

	Token* currentToken()
	{
		if (_buffer.empty)
			return null;
		return _buffer.front;
	}

	private Token* getToken(int lookahead)
	{
		assert(lookahead >= 1);
		if (_buffer.length < lookahead)
			return null;
		return _buffer[lookahead - 1];
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
		foreach (i; 0 .. min(_buffer.length, n))
			_buffer.removeFront();
		bufferNextNTokens(n);
	}

	Token* peek(int lookahead = 1)
	{
		_buffer.reserve(lookahead);
		bufferNextNTokens(lookahead);
		return getToken(lookahead);
	}

	private void bufferNextNTokens(size_t tokenCount)
	{
		while (_buffer.length < tokenCount
			&& !_lexerRange.empty)
		{
			void bufferNextToken()
			{
				auto t = _lexerRange.front;
				auto memory = cast(Token*) allocator.allocate(Token.sizeof).ptr;
				*memory = t;
				_buffer.insertBack(memory);
				_lexerRange.popFront();
			}
			bufferNextToken();
		}
	}
}

template perhapsNotAllocator(alias T)
{
	import std.meta;
	static if (stateSize!(typeof(T)) > 0)
		alias perhapsNotAllocator = T;
	else
		alias perhapsNotAllocator = AliasSeq!();
}

auto bufferArithmeticLexer
(
	TLexerRange,
	TTokenListAllocator = Mallocator,
	TTokenAllocator = Mallocator
)
(
	TLexerRange lexerRange,
	TTokenListAllocator tokenListAllocator = TTokenListAllocator.instance,
	TTokenAllocator tokenAllocator = TTokenAllocator.instance
)
{
	alias _TCList = CyclicBuffer!(Token*, TTokenListAllocator);
	alias _BufferingArithmeticLexer = BufferingArithmeticLexer!(TLexerRange, TTokenAllocator, _TCList);

	auto clist() { return _TCList(perhapsNotAllocator!tokenListAllocator); }
	return _BufferingArithmeticLexer(lexerRange, perhapsNotAllocator!tokenAllocator, clist);
}


void writeTokenInfo(Token* token, const(string)[] operators)
{
	writeln("Token name: ", token.text);
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
			writeln(token.operator.name);
			break;
		case TokenKind.identifier:
			writeln(token.text);
			break;
		case TokenKind.other:
			writeln(token.text);
			break;
	}
	writeln();
}


enum ExpressionNodeKind
{
	operator,
	identifier,
	invocation,
	parenthesizedExpression,
	integerLiteral,
	floatLiteral,
}

struct ExpressionSyntaxNode
{
	ExpressionNodeKind kind;
	Token* token;
}

struct OperatorNode(size_t arity)
{
	ExpressionSyntaxNode expression;
	alias expression this;

	ExpressionSyntaxNode*[arity] operands;

	inout(Operator*) operator() inout return
	{
		return operatorToken.operator;
	}
}

struct IdentifierNode
{
	ExpressionSyntaxNode expression;
	alias expression this;
	
	string name() const
	{
		return token.text;
	}
}

struct InvocationNode
{
	ExpressionSyntaxNode expression;
	alias expression this;

	IdentifierNode* identifier;
	Token*[] delimiters;
	ExpressionSyntaxNode*[] arguments;
	Token*[2] parentheses;

	ref inout(Token*) openParenthesis() inout return
	{
		return parentheses[0];
	}

	ref inout(Token*) closeParenthesis() inout return
	{
		return parentheses[1];
	}
}

struct ParenthesizedExpressionSyntaxNode
{
	ExpressionSyntaxNode expression;
	alias expression this;

	ExpressionSyntaxNode* innerExpression;
	Token*[2] parentheses;

	ref inout(Token*) openParenthesis() inout return
	{
		return parentheses[0];
	}

	ref inout(Token*) closeParenthesis() inout return
	{
		return parentheses[1];
	}
}

struct IntegerLiteralNode
{
	ExpressionSyntaxNode expression;
	alias expression this;

	long integer() const
	{
		return token.integer;
	}
}

struct FloatLiteralNode
{
	ExpressionSyntaxNode expression;
	alias expression this;

	double floating() const
	{
		return token.floating;
	}
}

OperatorNode!arity* operatorNode(arity, TAllocator)(TAllocator allocator, ExpressionSyntaxNode*[arity] operands, Token* operatorToken)
{
	auto node = cast(OperatorNode!arity*) allocator.allocate(OperatorNode!arity.sizeof).ptr;
	node.operands = operands;
	node.token = operatorToken;
	node.kind = ExpressionNodeKind.operator;
	return ptr;
}

IdentifierNode* identifierNode(TAllocator)(TAllocator allocator, Token* token)
{
	auto node = cast(IdentifierNode*) allocator.allocate(IdentifierNode.sizeof).ptr;
	node.token = token;
	node.kind = ExpressionNodeKind.identifier;
	return node;
}

InvocationNode* invocationNode(TAllocator)(TAllocator allocator, IdentifierNode* identifier, Token*[] delimiters, ExpressionSyntaxNode*[] arguments, Token*[2] parentheses)
{
	auto node = cast(InvocationNode*) allocator.allocate(InvocationNode.sizeof).ptr;
	node.identifier = identifier;
	node.delimiters = delimiters;
	node.arguments = arguments;
	node.parentheses = parentheses;
	node.kind = ExpressionNodeKind.invocation;
	return node;
}

ParenthesizedExpressionSyntaxNode* parenthesizedExpressionNode(TAllocator)(TAllocator allocator, ExpressionSyntaxNode* innerExpression, Token*[2] parentheses)
{
	auto node = cast(ParenthesizedExpressionSyntaxNode*) allocator.allocate(ParenthesizedExpressionSyntaxNode.sizeof).ptr;
	node.innerExpression = innerExpression;
	node.parentheses = parentheses;
	node.kind = ExpressionNodeKind.other;
	return node;
}

IntegerLiteralNode* integerLiteralNode(TAllocator)(TAllocator allocator, Token* token)
{
	auto node = cast(IntegerLiteralNode*) allocator.allocate(IntegerLiteralNode.sizeof).ptr;
	node.token = token;
	node.kind = ExpressionNodeKind.integerLiteral;
	return node;
}

FloatLiteralNode* floatLiteralNode(TAllocator)(TAllocator allocator, Token* token)
{
	auto node = cast(FloatLiteralNode*) allocator.allocate(FloatLiteralNode.sizeof).ptr;
	node.token = token;
	node.kind = ExpressionNodeKind.floatLiteral;
	return node;
}

bool check(Token* token, string otherExpectedText)
{
	return token.kind == TokenKind.other && token.text == otherExpectedText;
}

struct ExpressionSyntaxTree
{
	ExpressionSyntaxNode* root;
}

struct Parser(TLexer, TAllocator)
{
	private TLexer* _lexer;
	private mixin AllocatorState!TAllocator;

	ExpressionSyntaxTree parse()
	{
		auto root = parseExpression();
		return ExpressionSyntaxTree(root);
	}

	// TODO: check for null when reading tokens.
	private ExpressionSyntaxNode* parseExpression()
	{
		auto token = _lexer.peek();
		switch (token.kind)
		{
			default:
				assert(false, "Asserting for now");

			case TokenKind.identifier:
			{
				auto maybeParen = _lexer.peek(2);
				if (check(maybeParen, "("))
				{
					_lexer.popN(2);
					ExpressionSyntaxNode*[] arguments;
					Token*[] delimiters;
					while (true)
					{
						auto arg = parseExpression();
						arguments ~= arg;
						auto maybeComma = _lexer.peek();

						if (!check(maybeComma, ","))
							break;
						_lexer.pop();
						delimiters ~= maybeComma;
					}
					auto maybeCloseParen = _lexer.peek();
					if (!check(maybeCloseParen, ")"))
						assert(false, "Asserting for now");
					_lexer.pop();

					return allocator.invocationNode(
						identifierNode(allocator, token),
						delimiters,
						arguments,
						[token, maybeCloseParen]);
				}

				_lexer.pop();
				auto lhs = allocator.identifierNode(token);
				return parseOperationOrReturnLHS(parent, lhs);
			}
			case TokenKind.other:
			{
				if (token.text == "(")
				{
					_lexer.pop();
					auto innerExpression = parseExpression();
					auto maybeCloseParen = _lexer.peek();
					if (!check(maybeCloseParen, ")"))
						assert(false, "Asserting for now");
					
					_lexer.pop();
					auto lhs = allocator.parenthesizedExpressionNode(innerExpression, [token, maybeCloseParen]);
					return parseOperationOrReturnLHS(parent, lhs);
				}
				assert(false, "Asserting for now");
			}
			case TokenKind.integerLiteral:
			{
				_lexer.pop();
				auto lhs = allocator.integerLiteralNode(token);
				return parseOperationOrReturnLHS(parent, lhs);
			}
			case TokenKind.floatLiteral:
			{
				_lexer.pop();
				auto lhs = allocator.floatLiteralNode(token);
				return parseOperationOrReturnLHS(parent, lhs);
			}
			case TokenKind.operator:
			{
				if (token.operator.arity == OperatorArity.unary)
				{
					_lexer.pop();
					auto rhs = parseExpression();
					auto opLhs = allocator.unaryOperationNode(token, rhs);
					return parseOperationOrReturnLHS(parent, opLhs);
				}				
			}
		}
	}

	// Try parse binary operation.
	OperatorNode!2* parseOperationOrReturnLHS(ExpressionSyntaxNode* left)
	{
		// TODO: introduce precedence conform to the the paper
		// https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
		auto maybeOperator = _lexer.peek();
		if (maybeOperator.kind != TokenKind.operator)
			return null;
		{
			_lexer.pop();
			auto right = parseExpression();
			return allocator.operatorNode([left, right][0..2], maybeOperator);
		}
	}
}

void main()
{
	auto operators =
	[
		Operator("+", OperatorArity.binary, OperatorAssociativity.left, 1),
		Operator("-", OperatorArity.binary, OperatorAssociativity.left, 1),
		Operator("*", OperatorArity.binary, OperatorAssociativity.left, 2),
		Operator("/", OperatorArity.binary, OperatorAssociativity.left, 2),
		Operator("-", OperatorArity.unary, OperatorAssociativity.right, 3),
	];
	const input = "(1 + 2) * (3 / a)";
	// foreach (token; lexArithmetic(input, operators))
	// 	writeTokenInfo(&token, operators);

	auto underlyingLexer = lexArithmetic(input, operators);
	auto lexer = bufferArithmeticLexer(underlyingLexer);
	// BufferingArithmeticLexer!(typeof(underlyingLexer), Mallocator, CyclicBuffer!(Token*, Mallocator))(underlyingLexer, CyclicBuffer!(Token*, Mallocator)());
	// auto lexer = bufferArithmeticLexer(underlyingLexer);
	auto parser = Parser!(typeof(lexer), Mallocator)(lexer);
	auto tree = parser.parse();

	void writeTreeRecursively(ExpressionSyntaxNode* node, int indent)
	{
		if (node is null)
			return;
		foreach (i; 0 .. indent)
			write("  ");
		write(mode.kind, ": ", node.text);
		writeln();
		foreach (child; node.children)
			writeTreeRecursively(child, indent + 1);
	}
	writeTreeRecursively(tree.root, 0);
}
