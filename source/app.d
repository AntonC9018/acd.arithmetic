import std.stdio;


enum OperatorArity
{
	unary = 1,
	binary = 2,
	unaryOrBinary = unary | binary,
}

bool isUnary(OperatorArity arity)
{
	return (arity & OperatorArity.unary) != 0;
}

bool isBinary(OperatorArity arity)
{
	return (arity & OperatorArity.binary) != 0;
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
	size_t line = 0;
	size_t column = 0;
}

struct StreamSpan
{
	StreamPosition start;
	StreamPosition end;
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
	StreamSpan span;
	ref inout(StreamPosition) startPosition() inout return{ return span.start; }
	ref inout(StreamPosition) endPosition() inout return{ return span.end; }

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
	private T[] data;
	private size_t length;

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
	
	StreamPosition streamPosition() const
	{
		return _currentPosition;
	}

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
		_currentToken.startPosition = _currentPosition;

		const f = _input.front;
		popSingleCharacter();

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
			_matchingOperatorsCache.clear();
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
					if (_matchingOperatorsCache.sliceTemp[].any!(op => op.name.length > 1))
					{
						size_t index = 1;
						while (!_input.empty)
						{
							auto data = _matchingOperatorsCache.sliceTemp;
							_matchingOperatorsCache.clear();
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

	StreamPosition streamPosition() const
	{
		return _lexerRange.streamPosition();
	}

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


enum SyntaxNodeKind
{
	operator,
	identifier,
	invocation,
	parenthesizedExpression,
	integerLiteral,
	floatLiteral,
}

struct SyntaxNode
{
	SyntaxNodeKind kind;

	SyntaxNode* asSyntaxNode() return
	{
		return &this;
	}
}

struct ExpressionSyntaxNode
{
	SyntaxNode node;
	alias node this;
}

struct OperatorNode
{
	SyntaxNode node;
	alias node this;

	SyntaxNode*[] operands;
	Token* operatorToken;

	inout(Operator*) operator() inout return
	{
		return operatorToken.operator;
	}
}

struct IdentifierNode
{
	SyntaxNode node;
	alias node this;

	Token* token;
	
	string name() const
	{
		return token.text;
	}
}

struct InvocationNode
{
	SyntaxNode node;
	alias node this;

	IdentifierNode* identifier;
	Token*[] delimiters;
	SyntaxNode*[] arguments;
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

struct ParenthesizedExpressionNode
{
	SyntaxNode node;
	alias node this;

	SyntaxNode* innerExpression;
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

struct LiteralNode
{
	SyntaxNode node;
	alias node this;

	Token* token;
}

struct IntegerLiteralNode
{
	LiteralNode literal;
	alias literal this;

	long integer() const
	{
		return token.integer;
	}
}

struct FloatLiteralNode
{
	LiteralNode literal;
	alias literal this;

	double floating() const
	{
		return token.floating;
	}
}

OperatorNode* operatorNode(TAllocator)(TAllocator allocator, SyntaxNode*[] operands, Token* operatorToken)
{
	auto node = cast(OperatorNode*) allocator.allocate(OperatorNode.sizeof).ptr;
	node.operands = operands;
	node.operatorToken = operatorToken;
	node.kind = SyntaxNodeKind.operator;
	return node;
}

IdentifierNode* identifierNode(TAllocator)(TAllocator allocator, Token* token)
{
	auto node = cast(IdentifierNode*) allocator.allocate(IdentifierNode.sizeof).ptr;
	node.token = token;
	node.kind = SyntaxNodeKind.identifier;
	return node;
}

InvocationNode* invocationNode(TAllocator)(TAllocator allocator, IdentifierNode* identifier, Token*[] delimiters, SyntaxNode*[] arguments, Token*[2] parentheses)
{
	auto node = cast(InvocationNode*) allocator.allocate(InvocationNode.sizeof).ptr;
	node.identifier = identifier;
	node.delimiters = delimiters;
	node.arguments = arguments;
	node.parentheses = parentheses;
	node.kind = SyntaxNodeKind.invocation;
	return node;
}

ParenthesizedExpressionNode* parenthesizedExpressionNode(TAllocator)(TAllocator allocator, SyntaxNode* innerExpression, Token*[2] parentheses)
{
	auto node = cast(ParenthesizedExpressionNode*) allocator.allocate(ParenthesizedExpressionNode.sizeof).ptr;
	node.innerExpression = innerExpression;
	node.parentheses = parentheses;
	node.kind = SyntaxNodeKind.parenthesizedExpression;
	return node;
}

IntegerLiteralNode* integerLiteralNode(TAllocator)(TAllocator allocator, Token* token)
{
	auto node = cast(IntegerLiteralNode*) allocator.allocate(IntegerLiteralNode.sizeof).ptr;
	node.token = token;
	node.kind = SyntaxNodeKind.integerLiteral;
	return node;
}

FloatLiteralNode* floatLiteralNode(TAllocator)(TAllocator allocator, Token* token)
{
	auto node = cast(FloatLiteralNode*) allocator.allocate(FloatLiteralNode.sizeof).ptr;
	node.token = token;
	node.kind = SyntaxNodeKind.floatLiteral;
	return node;
}

StreamSpan getWholeSpan(SyntaxNode* node)
{
	final switch (node.kind)
	{
		case SyntaxNodeKind.operator:
		{
			auto op = cast(OperatorNode*) node;
			auto first = getWholeSpan(op.operands[0]);
			auto last = getWholeSpan(op.operands[$ - 1]);
			return StreamSpan(first.start, last.end);
		}
		case SyntaxNodeKind.identifier:
		{
			auto id = cast(IdentifierNode*) node;
			return id.token.span;
		}
		case SyntaxNodeKind.invocation:
		{
			auto inv = cast(InvocationNode*) node;
			return StreamSpan(inv.openParenthesis.span.start, inv.closeParenthesis.span.end);
		}
		case SyntaxNodeKind.parenthesizedExpression:
		{
			auto paren = cast(ParenthesizedExpressionNode*) node;
			return StreamSpan(paren.openParenthesis.span.start, paren.closeParenthesis.span.end);
		}
		case SyntaxNodeKind.integerLiteral:
		case SyntaxNodeKind.floatLiteral:
		{
			auto lit = cast(LiteralNode*) node;
			return lit.token.span;
		}
	}
}


bool check(Token* token, string otherExpectedText)
{
	return token.kind == TokenKind.other && token.text == otherExpectedText;
}

struct ExpressionSyntaxTree
{
	SyntaxNode* root;
}

struct Parser(TLexer, TAllocator, alias ErrorHandler = writeln)
{
	private TLexer* _lexer;
	private mixin AllocatorState!TAllocator;
	private size_t _errorCount;

	static if (is(ErrorHandler : T, T))
		private ErrorHandler _errorHandler;
	else
		private alias _errorHandler = ErrorHandler;

	private struct Location
	{
		StreamPosition position;
		
		void toString(scope void delegate(const(char)[]) sink) const
		{
			import std.format;
			import std.range;
			sink("at ");
			formattedWrite!"%d"(sink, position.line);
			sink(":");
			formattedWrite!"%d"(sink, position.column + 1);
		}
	}

	private void error(T...)(auto ref T args)
	{
		_errorCount++;
		_errorHandler(args);
	}

	private void errorUnclosedParens(Token* openParen)
	{
		StreamPosition expectedPosition = _lexer.streamPosition;
		error("Unclosed parenthesis ", Location(openParen.startPosition), " (expected one at ", expectedPosition, ")");
	}

	ExpressionSyntaxTree parse()
	{
		int precedence = -1;
		auto root = parseExpression(precedence);

		auto nextToken = _lexer.peek();
		if (nextToken !is null)
			error("Unexpected token ", nextToken.text, " at ", Location(nextToken.startPosition));
		return ExpressionSyntaxTree(root);
	}

	private SyntaxNode* parseTerm()
	{
		auto token = _lexer.peek();
		if (token is null)
			return null;

		final switch (token.kind)
		{
			case TokenKind.identifier:
			{
				auto maybeParen = _lexer.peek(2);

				// Simple case, just an identifier.
				if (maybeParen is null || !check(maybeParen, "("))
				{
					_lexer.pop();
					return allocator.identifierNode(token).asSyntaxNode;
				}

				// Otherwise, we have an invocation.
				_lexer.popN(2);
				SyntaxNode*[] arguments;
				Token*[] delimiters;
				while (true)
				{
					// Separating arguments by commas resets their precedence.
					int argPrecedence = -1;
					auto arg = parseExpression(argPrecedence);
					if (arg is null)
					{
						auto reportedLocation = delimiters.length > 0 ? delimiters[$ - 1].endPosition : maybeParen.startPosition;
						reportedLocation.column += 1;
						error("Expecting an argument to the invocation of ", token.text, " at ", Location(reportedLocation));
						return null;
					}
					arguments ~= arg;

					auto maybeComma = _lexer.peek();
					if (maybeComma is null || !check(maybeComma, ","))
						break;

					_lexer.pop();
					delimiters ~= maybeComma;
				}
				auto maybeCloseParen = _lexer.peek();
				if (maybeCloseParen is null || !check(maybeCloseParen, ")"))
				{
					errorUnclosedParens(token);
					return null;
				}
				_lexer.pop();

				return allocator.invocationNode(
					identifierNode(allocator, token),
					delimiters,
					arguments,
					[token, maybeCloseParen])
						.asSyntaxNode;
			}
			case TokenKind.other:
			{
				if (token.text == "(")
				{
					_lexer.pop();

					int precedence = -1;
					auto innerExpression = parseExpression(precedence);
					auto maybeCloseParen = _lexer.peek();
					if (maybeCloseParen is null || !check(maybeCloseParen, ")"))
					{
						errorUnclosedParens(token);
						return null;
					}
					
					_lexer.pop();
					return allocator.parenthesizedExpressionNode(
						innerExpression, [token, maybeCloseParen])
							.asSyntaxNode;
				}
				error("Unrecognized kind of text ", token.text);
				return null;
			}
			case TokenKind.integerLiteral:
			{
				_lexer.pop();
				return allocator.integerLiteralNode(token).asSyntaxNode;
			}
			case TokenKind.floatLiteral:
			{
				_lexer.pop();
				return allocator.floatLiteralNode(token).asSyntaxNode;
			}
			case TokenKind.operator:
			{
				if (token.operator.arity.isUnary)
				{
					_lexer.pop();
					int precedence = -1;
					auto rhs = parseExpression(precedence);
					if (rhs is null)
					{
						error("Expected an expression after the unary operator ", token.text);
						return null;
					}
					return allocator.operatorNode([rhs], token).asSyntaxNode;
				}	
			}
		}
		assert(false);
	}

	// TODO: introduce precedence conform to the the paper
	// https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
	private OperatorNode* parseBinaryOperation(SyntaxNode* lhs, ref int precedence)
	{
		auto operatorToken = _lexer.peek();
		if (operatorToken.kind != TokenKind.operator)
			return null;
		if (!operatorToken.operator.arity.isBinary)
		{
			error("Expected a binary operator but found ", operatorToken.text, " at ", Location(operatorToken.startPosition));
			return null;
		}
		_lexer.pop();

		SyntaxNode* rhs;
		if (precedence <= operatorToken.operator.precedence)
			rhs = parseExpression(precedence);
		else
			rhs = parseTerm();
		
		if (rhs is null)
		{
			error("Expected an expression after the binary operator ", operatorToken.text, " at ", Location(operatorToken.startPosition));
			return null;
		}
		return allocator.operatorNode([lhs, rhs], operatorToken);
	}

	private SyntaxNode* parseExpression(ref int precedence)
	{
		auto lhs = parseTerm();
		auto binaryOperation = parseBinaryOperation(lhs, precedence);
		if (binaryOperation is null)
			return lhs.asSyntaxNode;
		return binaryOperation.asSyntaxNode;
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
	const input = "a + b * c - d";
	// foreach (token; lexArithmetic(input, operators))
	// 	writeTokenInfo(&token, operators);

	auto underlyingLexer = lexArithmetic(input, operators);
	auto lexer = bufferArithmeticLexer(underlyingLexer);
	// BufferingArithmeticLexer!(typeof(underlyingLexer), Mallocator, CyclicBuffer!(Token*, Mallocator))(underlyingLexer, CyclicBuffer!(Token*, Mallocator)());
	// auto lexer = bufferArithmeticLexer(underlyingLexer);
	auto parser = Parser!(typeof(lexer), Mallocator)(&lexer);
	auto tree = parser.parse();

	void writeTreeRecursively(SyntaxNode* node, int indent)
	{
		if (node is null)
			return;
		foreach (i; 0 .. indent)
			write("  ");
		write(node.kind);
		final switch (node.kind)
		{
			case SyntaxNodeKind.identifier:
			{
				auto ident = cast(IdentifierNode*) node;
				writeln(": ", ident.token.text);
				break;
			}
			case SyntaxNodeKind.integerLiteral:
			case SyntaxNodeKind.floatLiteral:
			{
				auto literal = cast(LiteralNode*) node;
				writeln(": ", literal.token.text);
				break;
			}
			case SyntaxNodeKind.invocation:
			{
				auto invocation = cast(InvocationNode*) node;
				writeln(": ", invocation.identifier.token.text);
				foreach (arg; invocation.arguments)
					writeTreeRecursively(arg, indent + 1);
				break;
			}
			case SyntaxNodeKind.parenthesizedExpression:
			{
				auto expr = cast(ParenthesizedExpressionNode*) node;
				writeln();
				writeTreeRecursively(expr.innerExpression, indent + 1);
				break;
			}
			case SyntaxNodeKind.operator:
			{
				auto op = cast(OperatorNode*) node;
				writeln(": ", op.operator.name);
				foreach (operand; op.operands)
					writeTreeRecursively(operand, indent + 1);
				break;
			}
		}
	}
	writeTreeRecursively(tree.root, 0);
}
