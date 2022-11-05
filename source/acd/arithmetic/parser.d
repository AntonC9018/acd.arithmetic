module acd.arithmetic.parser;

import acd.arithmetic.syntax;
import acd.arithmetic.lexer;
import acd.arithmetic.operators;
import acd.arithmetic.internal;

import std.stdio;
import std.algorithm;
import std.range;

import std.experimental.allocator.mallocator;
import std.experimental.allocator.common : stateSize;
import containers.internal.mixins : AllocatorState;

private bool checkOther(Token* token, string otherExpectedText)
{
    return token.kind == TokenKind.other && token.text == otherExpectedText;
}

struct ExpressionSyntaxTree
{
    bool thereHaveBeenErrors;
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

    ExpressionSyntaxTree parse()
    {
        int precedence = -1;
        auto root = parseExpression(precedence);

        auto nextToken = _lexer.peek();
        if (nextToken !is null)
            error("Unexpected token ", nextToken.text," at ", Location(nextToken.startPosition));
        return ExpressionSyntaxTree(_errorCount > 0, root);
    }

    private void error(T...)(auto ref T args)
    {
        _errorCount++;
        _errorHandler(args);
    }

    private void errorUnclosedParens(Token* openParen)
    {
        StreamPosition expectedPosition = _lexer.streamPosition;
        error("Unclosed parenthesis ", Location(openParen.startPosition),
            " (expected one at ", Location(expectedPosition), ")");
    }

    private SyntaxNode* parseTermLhs()
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
                if (maybeParen is null || !checkOther(maybeParen, "("))
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
                    if (maybeComma is null || !checkOther(maybeComma, ","))
                        break;

                    _lexer.pop();
                    delimiters ~= maybeComma;
                }
                auto maybeCloseParen = _lexer.peek();
                if (maybeCloseParen is null || !checkOther(maybeCloseParen, ")"))
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
                    if (maybeCloseParen is null || !checkOther(maybeCloseParen, ")"))
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
                error("Not expecting an operator ", token.text, " at ", Location(token.startPosition));
                return null;
            }
        }
    }


    private SyntaxNode* parseTerm()
    {
        import containers.dynamicarray;
        import std.traits : Unqual;
        alias QueueType = DynamicArray!(OperatorNode*, TAllocator);

        enum AssociativityFilterResult
        {
            error,
            stop,
            keep,
        }

        bool popOperators(alias associativityFilter)(ref QueueType queue)
        {
            while (true)
            {
                auto token = _lexer.peek();
                if (token is null)
                    return true;

                if (token.kind != TokenKind.operator)
                    return true;

                auto operator = token.operatorGroup.findOperatorByArity(OperatorArity.unary);

                AssociativityFilterResult associativityResult = associativityFilter(operator, token);
                if (associativityResult == AssociativityFilterResult.error)
                    return false;
                if (associativityResult == AssociativityFilterResult.stop)
                    return true;

                auto operatorNode = allocator.emptyOperatorNode;
                operatorNode.operator = operator; 
                operatorNode.operatorToken = token;
                queue ~= operatorNode;
                _lexer.pop();
            }
        }

        auto lhsUnaryOperators = QueueType(perhapsNotAllocator!allocator);
        popOperators!((op, token)
        {
            // arity is not unary
            if (op is null)
            {
                error("Expecting the operator to be unary at ", Location(token.startPosition));
                return AssociativityFilterResult.error;
            }
  
            if (op.associativity & OperatorAssociativity.right)
                return AssociativityFilterResult.keep;
  
            error("Expecting the unary operator to be right associative at ", Location(token.startPosition));
            return AssociativityFilterResult.error;
        })(lhsUnaryOperators);

        auto term = parseTermLhs();
        if (term is null)
            return null;

        auto rhsUnaryOperators = QueueType(perhapsNotAllocator!allocator);
        popOperators!((op, token)
        {
            if (op is null)
                return AssociativityFilterResult.stop;

            // Prevent the case where there's both a binary and a unary operator for the same token.
            if (token.operatorGroup.operators.length > 1)
                return AssociativityFilterResult.stop;

            if (op.associativity & OperatorAssociativity.left)
                return AssociativityFilterResult.keep;

            error("Expecting the unary operator to be left associative at ", Location(token.startPosition));
            return AssociativityFilterResult.error;
        })(rhsUnaryOperators);

        auto lhsOperatorsRange = lhsUnaryOperators[].retro;
        auto rhsOperatorsRange = rhsUnaryOperators[];

        // Wrap the term into unary operator nodes conform to their precedence
        SyntaxNode* node = term;
        while (!lhsOperatorsRange.empty && !rhsOperatorsRange.empty)
        {
            OperatorNode* operatorNode;
            auto lhsOperator = lhsOperatorsRange.front;
            auto rhsOperator = rhsOperatorsRange.front;
            if (lhsOperator.operator.precedence >= rhsOperator.operator.precedence)
            {
                operatorNode = lhsOperator;
                lhsOperatorsRange.popFront();
            }
            else
            {
                operatorNode = rhsOperator;
                rhsOperatorsRange.popFront();
            }
            operatorNode.operands = [node];
            node = operatorNode.asSyntaxNode;
        }
        while (!lhsOperatorsRange.empty)
        {
            auto operatorNode = lhsOperatorsRange.front;
            operatorNode.operands = [node];
            node = operatorNode.asSyntaxNode;
            lhsOperatorsRange.popFront();
        }
        while (!rhsOperatorsRange.empty)
        {
            auto operatorNode = rhsOperatorsRange.front;
            operatorNode.operands = [node];
            node = operatorNode.asSyntaxNode;
            rhsOperatorsRange.popFront();
        }

        return node;
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
        if (binaryOperator.associativity & OperatorAssociativity.left)
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

// A super simple method that just uses the default allocators and stuff everywhere.
ExpressionSyntaxTree parseExpression(string input, OperatorGroup[] operatorGroups)
{
    import std.experimental.allocator.gc_allocator;
    auto lexerRange = createArithmeticLexer(input, operatorGroups);
    auto lexer = createBufferedLexer(lexerRange, GCAllocator.instance, GCAllocator.instance);
    auto parser = createParser(&lexer, GCAllocator.instance);
    auto tree = parser.parse();
    return tree;
}
