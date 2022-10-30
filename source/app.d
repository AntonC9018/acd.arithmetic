import std.stdio;
import std.meta : Repeat;
import std.typecons : Nullable, nullable;

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
    int arity;
    OperatorAssociativity associativity;
    int precedence;
}

struct OperatorGroup
{
    string fullName;
    Operator[] operators;
    string name() const { return operators[0].name; }
}

Operator* findOperatorByArity(OperatorGroup* group, OperatorArity arity)
{
    foreach (ref op; group.operators)
    {
        if (op.arity == cast(int) arity)
            return &op;
    }
    return null;
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
        OperatorGroup* operatorGroup;
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

    private OperatorGroup[] _operatorGroups;
    private TRange _input;
    private TRange _currentLine;
    private StreamPosition _currentPosition;
    private Token _currentToken = Token.init;
    private bool _empty = false;
    private DynamicArray!(OperatorGroup*) _matchingOperatorGroupsCache;
    
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
            _matchingOperatorGroupsCache.clear();
            foreach (ref op; _operatorGroups.filter!(op => op.name[0] == f))
                _matchingOperatorGroupsCache ~= &op;

            if (_matchingOperatorGroupsCache.length == 0)
            {
                _currentToken.kind = TokenKind.other;
            }
            else
            {
                OperatorGroup* op = _matchingOperatorGroupsCache[0];
                {
                    // Matches the longest possible operator.
                    if (_matchingOperatorGroupsCache.sliceTemp[].any!(op => op.name.length > 1))
                    {
                        size_t index = 1;
                        while (!_input.empty)
                        {
                            auto data = _matchingOperatorGroupsCache.sliceTemp;
                            _matchingOperatorGroupsCache.clear();
                            foreach (op1; data[])
                            {
                                if (op1.name.length > index && op1.name[index] == _input.front)
                                    _matchingOperatorGroupsCache ~= op1;
                            }
                            if (_matchingOperatorGroupsCache.length == 0)
                                break;
                            index++;
                            op = _matchingOperatorGroupsCache[0];
                            popSingleCharacter();
                        }
                    }

                    _currentToken.kind = TokenKind.operator;
                    _currentToken.operatorGroup = op;
                }
            }
        }
        _currentToken.endPosition = _currentPosition;
        _currentToken.text = _currentLine[startColumn .. _currentPosition.column];
        skipWhitespace();
    }
}

auto createArithmeticLexer(TRange)(
    TRange range,
    OperatorGroup[] operatorGroups,
    StreamPosition startPosition = StreamPosition.init)
{
    auto lexer = ArithmeticLexerRange!(TRange)(operatorGroups, range, range, startPosition);
    lexer._matchingOperatorGroupsCache = typeof(lexer._matchingOperatorGroupsCache)(4);
    lexer.popFront();
    return lexer;
}

import containers.cyclicbuffer;
import std.experimental.allocator;
import std.experimental.allocator.mallocator;
import containers.internal.mixins : AllocatorState;
import std.experimental.allocator.common : stateSize;
import std.algorithm : min;

private struct BufferedLexer(TLexerRange,
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

auto createBufferedLexer
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
    alias _BufferedLexer = BufferedLexer!(TLexerRange, TTokenAllocator, _TCList);

    auto clist() { return _TCList(perhapsNotAllocator!tokenListAllocator); }
    return _BufferedLexer(lexerRange, perhapsNotAllocator!tokenAllocator, clist);
}


void writeTokenInfo(Token* token, const(OperatorGroup)[] operatorGroups)
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
            writeln(token.operatorGroup.name);
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
    Operator* operator;
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

    long constantValue() const
    {
        return token.integer;
    }
}

struct FloatLiteralNode
{
    LiteralNode literal;
    alias literal this;

    double constantValue() const
    {
        return token.floating;
    }
}

OperatorNode* operatorNode(TAllocator)(TAllocator allocator, SyntaxNode*[] operands, Token* operatorToken, Operator* operator)
{
    auto node = cast(OperatorNode*) allocator.allocate(OperatorNode.sizeof).ptr;
    node.operands = operands;
    node.operatorToken = operatorToken;
    node.operator = operator;
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

StreamSpan getWholeSpan(OperatorNode* operatorNode)
{
    auto first = getWholeSpan(operatorNode.operands[0]);
    auto last = getWholeSpan(operatorNode.operands[$ - 1]);
    return StreamSpan(first.start, last.end);
}

StreamSpan getWholeSpan(SyntaxNode* node)
{
    final switch (node.kind)
    {
        case SyntaxNodeKind.operator:
        {
            auto op = cast(OperatorNode*) node;
            return getWholeSpan(op);
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
    private TLexer _lexer;
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
                auto unaryOperator = token.operatorGroup.findOperatorByArity(OperatorArity.unary);
                if (unaryOperator is null)
                {
                    error("Found non-unary operator ", token.text,
                        " while expecting a term, possibly prefixed by a unary operator at ",
                        Location(token.startPosition));
                    return null;
                }
                {
                    _lexer.pop();
                    int precedence = -1;
                    auto rhs = parseExpression(precedence);
                    if (rhs is null)
                    {
                        error("Expected an expression after the unary operator ", token.text);
                        return null;
                    }
                    return allocator.operatorNode([rhs], token, unaryOperator).asSyntaxNode;
                }	
            }
        }
        assert(false);
    }

    private OperatorNode* parseBinaryOperation(SyntaxNode* lhs, int precedence)
    {
        auto operatorToken = _lexer.peek();
        if (operatorToken is null
            || operatorToken.kind != TokenKind.operator)
        {
            return null;
        }

        auto binaryOperator = operatorToken.operatorGroup.findOperatorByArity(OperatorArity.binary);
        if (binaryOperator is null)
        {
            error("Expected a binary operator but found ",
                binaryOperator.name, " of arity ", binaryOperator.arity,
                " at ", Location(operatorToken.startPosition));
            return null;
        }

        if (binaryOperator.precedence < precedence)
            return null;

        _lexer.pop();
        int rhsPrecedence = binaryOperator.precedence;
        if (binaryOperator.associativity == OperatorAssociativity.left)
            rhsPrecedence += 1;

        auto rhs = parseExpression(rhsPrecedence);
        if (rhs is null)
        {
            error("Expected an expression after the binary operator ", operatorToken.text, " at ", Location(operatorToken.startPosition));
            return null;
        }

        auto binaryOperation = allocator.operatorNode([lhs, rhs], operatorToken, binaryOperator);
        return binaryOperation;
    }

    // TODO: introduce precedence conform the paper
    // https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
    private SyntaxNode* parseExpression(int precedence)
    {
        auto lhs = parseTerm();
        if (lhs is null)
            return null;

        auto binaryOperation = parseBinaryOperation(lhs, precedence);
        if (binaryOperation is null)
            return lhs;

        while (true)
        {
            lhs = binaryOperation.asSyntaxNode;
            binaryOperation = parseBinaryOperation(lhs, precedence);
            if (binaryOperation is null)
                return lhs;
        }
    }
}

auto createParser
(
    TLexer,
    TAllocator = Mallocator,
    alias ErrorHandler = writeln
)
(
    TLexer lexer,
    TAllocator allocator = TAllocator.instance
)
{
    return Parser!(TLexer, TAllocator, ErrorHandler)(lexer, perhapsNotAllocator!allocator, 0);
}

enum SymbolKind
{
    variable,
    function_,
}

TAs reinterpret(TAs, TValue)(TValue value)
{
    assert(TValue.sizeof == TAs.sizeof);
    return *cast(TAs*) &value;
}

struct Function(TNumber)
{
    int arity;
    void*[2] delegate_;

    enum maxArity = 8;
    Nullable!TNumber exec(TNumber[maxArity] arguments)
    {
        enum null_ = Nullable!TNumber.init;
        if (arguments.length != arity)
            return null_;

        switch (arity)
        {
            default:
                return null_;
            static foreach (i; 0 .. 8)
            {
                case i:
                {
                    alias TDelegate = TNumber delegate(Repeat!(i, TNumber));
                    import std.array;
                    return nullable(reinterpret!TDelegate(arguments[0 .. i].staticArray!i.tupleof));
                }
            }
        }
    }
}

Function!TNumber createFunction(TDelegate : TNumber delegate(TArgs), TNumber, TArgs...)(TDelegate delegate_)
    if (is(TArgs == Repeat!(TArgs.length, TNumber)))
{
    assert(TArgs.length <= Function!TNumber.maxArity, "Too many arguments");
    auto delegateData = reinterpret!(void*[2])(delegate_);
    return Function!TNumber(TArgs.length, delegateData);
}

struct FunctionOverloadGroup(TNumber)
{
    Function!TNumber[] functions;
}

struct Symbol(TNumber)
{
    SymbolKind kind;
    union
    {
        TNumber value;
        FunctionOverloadGroup!TNumber functions_;
    }
}

Symbol!TNumber createSymbol(TNumber)(TNumber value)
    if (__traits(isArithmetic, TNumber))
{
    Symbol!TNumber s;
    s.kind = SymbolKind.variable;
    s.value = value;
    return s;
}

Symbol!TNumber createSymbol(TNumber)(FunctionOverloadGroup!TNumber functions_)
    if (__traits(isArithmetic, TNumber))
{
    Symbol!TNumber s;
    s.kind = SymbolKind.function_;
    s.functions_ = functions_;
    return s;
}

struct FunctionOverloadGroupBuilder(TNumber)
{
    FunctionOverloadGroup!TNumber group;

    ref FunctionOverloadGroupBuilder!TNumber add(TDelegate : TNumber delegate(TArgs), TArgs...)(TDelegate delegate_)
        return
    {
        import std.conv : text;
        import std.algorithm;
        assert(!group.functions[].any!(f => f.arity == TArgs.length),
            text("Function with arity ", TArgs.length, " already exists"));

        group.functions ~= createFunction(delegate_);
        return this;
    }

    FunctionOverloadGroup!TNumber build()
    {
        return group;
    }
}

struct SymbolTable(TNumber)
{
    Symbol!TNumber[string] table;

    auto opBinaryRight(string op)(TNumber rhs) if (op == "in")
    {
        return rhs in table;
    }

    // Support assigning symbols, values or function groups via indexer
    ref Symbol!TNumber opIndex(string name)
    {
        return table[name];
    }

    void opIndexAssign(TNumber variableValue, string variableName)
    {
        table[variableName] = createSymbol(variableValue);
    }

    void opIndexAssign(
        FunctionOverloadGroup!TNumber functionGroup, string functionName)
    {
        table[functionName] = createSymbol(functionGroup);
    }

    void opIndexAssign
    (
        TDelegate : TNumber delegate(TArgs),
        TArgs...
    )
    (
        TDelegate dg,
        string functionName
    )
    {
        table[functionName] = createSymbol(createFunctionGroup!TNumber(dg));
    }

    void add(string functionName, TDelegate : TNumber delegate(TArgs), TArgs...)(TDelegate dg)
    {
        table[functionName] = createSymbol(createFunctionGroup!TNumber(dg));
    }
}

// TODO: move away from aa to a pointer based or index based symbol table.
TNumber eval(TNumber, TSymbolTable : TT!TNumber, TT, alias error = writeln)(
    TSymbolTable symbolTable, SyntaxNode* node)
{
    TNumber execFunc(string funcName, SyntaxNode*[] parameters, Location invocationLocation)
    {
        auto symbol = funcName in symbolTable;
        if (!symbol)
        {
            error("No function ", funcName, " found at ", invocationLocation);
            return TNumber.init;
        }

        auto funcs = symbol.functions_.functions[]
            .find!(f => f.arity == parameters.length);
        if (funcs.empty)
        {
            error("No function overload found for ", funcName,
                " at ", invocationLocation);
            return TNumber.init;
        }

        auto func = funcs.front;
        auto arguments = operatorNode.arguments
            .map!(a => eval(symbolTable, a))
            .staticArray!(func.maxArity);
        auto result = func.exec(arguments);
        if (result.isNull)
        {
            error("Invalid number of arguments for ", funcName,
                " at ", invocationLocation);
            return TNumber.init;
        }
    }

    final switch (node.kind)
    {
        case SyntaxNodeKind.integerLiteral:
        {
            auto integerLiteral = cast(IntegerLiteralNode*) node;
            return cast(TNumber) integerLiteral.constantValue;
        }

        case SyntaxNodeKind.floatLiteral:
        {
            auto floatLiteral = cast(FloatLiteralNode*) node;
            return cast(TNumber) floatLiteral.constantValue;
        }

        case SyntaxNodeKind.identifier:
        {
            auto ident = cast(IdentifierNode*) node;

            auto symbol = funcName in symbolTable;
            if (!symbol)
            {
                error("Unknown identifier ", ident.name, " at ", ident.location);
                return TNumber.init;
            }

            if (symbol.kind == SymbolKind.variable)
                return symbol.value;

            error("Expected a variable but found a function for ", ident.name);
            return TNumber.init;
        }

        case SyntaxNodeKind.operator:
        {
            auto operatorNode = cast(OperatorNode*) node;
            auto operator = operatorNode.operator;
            assert(operator.arity == operatorNode.arguments.length);
            return execFunc(
                operator.name,
                operatorNode.arguments,
                Location(operatorNode.operatorToken.startPosition));
        }

        case SyntaxNodeKind.invocation:
        {
            auto invocation = cast(InvocationNode*) node;
            return execFunc(
                invocation.name,
                invocation.arguments,
                Location(invocation.openParenthesis.token.startPosition));
        }

        case SyntaxNodeKind.parenthesizedExpression:
        {
            auto parenthesizedExpression = cast(ParenthesizedExpressionNode*) node;
            return eval(symbolTable, parenthesizedExpression.expression);
        }
    }
}

// TODO: use a container and an allocator.
struct OperatorGroupBuilder
{
    string fullName;
    string name;
    OperatorGroup group;

    ref OperatorGroupBuilder add(OperatorArity arity, OperatorAssociativity associativity, int precedence) return
    {
        import std.algorithm;
        import std.range;
        assert(group.operators[].find!((ref op) => op.arity == arity).empty,
            "Trying to add an operator variant with the same arity");

        group.operators ~= Operator(name, cast(int) arity, associativity, precedence);
        return this;
    }

    OperatorGroup build()
    {
        return group;
    }
}

OperatorGroupBuilder operatorGroup(string fullName, string name)
{
    return OperatorGroupBuilder(fullName, name, OperatorGroup());
}

auto functionGroup(TDelegates...)(TDelegates delegates)
{
    assert(is(TDelegates[0] : TNumber delegate(TArgs), TNumber, TArgs...));
    return createFunctionGroup!TNumber(delegates);
}

auto createFunctionGroup(TNumber, TDelegates...)(TDelegates delegates)
{
    auto functionOverloadGroupBuilder = FunctionOverloadGroupBuilder!TNumber();
    foreach (dg; delegates)
        functionOverloadGroupBuilder.add(dg);
    return functionOverloadGroupBuilder.build();
}

void main()
{
    OperatorGroup[] operatorGroups =
    [
        operatorGroup("addition", "+")
            .add(OperatorArity.binary, OperatorAssociativity.left, 1)
            .add(OperatorArity.unary, OperatorAssociativity.right, 2)
            .build(),

        operatorGroup("subtraction", "-")
            .add(OperatorArity.binary, OperatorAssociativity.left, 1)
            .add(OperatorArity.unary, OperatorAssociativity.right, 2)
            .build(),

        operatorGroup("multiplication", "*")
            .add(OperatorArity.binary, OperatorAssociativity.left, 3)
            .build(),

        operatorGroup("division", "/")
            .add(OperatorArity.binary, OperatorAssociativity.left, 3)
            .build(),

        operatorGroup("exponentiation", "^")
            .add(OperatorArity.binary, OperatorAssociativity.right, 4)
            .build(),
    ];

    // construct the symbol table
    import std.math;
    SymbolTable!double symbolTable;
    symbolTable["pi"] = PI;
    symbolTable["e"] = E;
    double t(double x) { return sin(x); }
    symbolTable.add("sin", (double x) => sin(x)); 
    symbolTable["sin"] = &t;
    symbolTable["cos"] = x => cos(x);
    symbolTable["+"] = createFunctionGroup!double(
        (x) => x,
        (x, y) => x + y
    );
    symbolTable["-"] = createFunctionGroup!double(
        (x) => -x,
        (x, y) => x - y
    );
    symbolTable["*"] = (x, y) => x * y;
    symbolTable["/"] = (x, y) => x / y;
    symbolTable["^"] = (x, y) => pow(x, y);



    const input = "1 + 2 ^ 2 * 3";
    // foreach (token; createArithmeticLexer(input, operators))
    // 	writeTokenInfo(&token, operators);

    auto lexerRange = createArithmeticLexer(input, operatorGroups);
    auto lexer = createBufferedLexer(lexerRange);
    auto parser = createParser(&lexer);
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
    
    void writeExpressionRecursively(SyntaxNode* node)
    {
        if (node is null)
            return;
        final switch (node.kind)
        {
            case SyntaxNodeKind.identifier:
            {
                auto ident = cast(IdentifierNode*) node;
                write(ident.token.text);
                break;
            }
            case SyntaxNodeKind.integerLiteral:
            case SyntaxNodeKind.floatLiteral:
            {
                auto literal = cast(LiteralNode*) node;
                write(literal.token.text);
                break;
            }
            case SyntaxNodeKind.invocation:
            {
                auto invocation = cast(InvocationNode*) node;
                write(invocation.identifier.token.text);
                write("(");
                foreach (arg; invocation.arguments)
                {
                    writeExpressionRecursively(arg);
                    write(", ");
                }
                write(")");
                break;
            }
            case SyntaxNodeKind.parenthesizedExpression:
            {
                auto expr = cast(ParenthesizedExpressionNode*) node;
                write("(");
                writeExpressionRecursively(expr.innerExpression);
                write(")");
                break;
            }
            case SyntaxNodeKind.operator:
            {
                auto op = cast(OperatorNode*) node;
                write("(");
                foreach (i, operand; op.operands)
                {
                    writeExpressionRecursively(operand);
                    if (i < op.operands.length - 1)
                        write(" ", op.operator.name, " ");
                }
                write(")");
                break;
            }
        }
    }
    writeTreeRecursively(tree.root, 0);
    writeExpressionRecursively(tree.root);
}
