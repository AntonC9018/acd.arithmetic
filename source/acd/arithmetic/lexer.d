module acd.arithmetic.lexer;

import acd.arithmetic.operators;
import acd.arithmetic.internal;

import std.stdio;
import std.algorithm;
import std.range;

enum TokenKind
{
    operator,
    identifier,
    integerLiteral,
    floatLiteral,

    /// Includes parentheses and any symbols
    /// that weren't recognized as one of the other kinds.
    other,
}

struct Token
{
    StreamSpan span;
    ref inout(StreamPosition) startPosition() inout return { return span.start; }
    ref inout(StreamPosition) endPosition() inout return { return span.end; }

    string text;
    TokenKind kind;
    union
    {
        OperatorGroup* operatorGroup;
        long integer;
        double floating;
    }
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

struct Location
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

import containers.internal.mixins : AllocatorState;
import std.experimental.allocator.common : stateSize;
import containers.cyclicbuffer;
import std.experimental.allocator;
import std.experimental.allocator.mallocator;

private struct ArithmeticLexerRange(TRange)
{
    import std.uni;

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
