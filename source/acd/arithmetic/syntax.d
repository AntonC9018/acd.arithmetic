module acd.arithmetic.syntax;

import acd.arithmetic.lexer;
import acd.arithmetic.operators;
import acd.arithmetic.internal;

///
enum SyntaxNodeKind
{
    @OperatorNode
    operator,
    
    @IdentifierNode
    identifier,
    
    @InvocationNode
    invocation,

    @ParenthesizedExpressionNode
    parenthesizedExpression,

    @IntegerLiteralNode
    integerLiteral,

    @FloatLiteralNode
    floatLiteral,
}

/// Base type for syntax nodes.
struct SyntaxNode
{
    SyntaxNodeKind kind;

    SyntaxNode* asSyntaxNode() return
    {
        return &this;
    }
}

///
struct OperatorNode
{
    SyntaxNode node;
    alias node this;

    SyntaxNode*[] operands;
    Token* operatorToken;
    Operator* operator;
}

///
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

/// Represents a function invocation.
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

///
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

/// Base type for literal nodes
struct LiteralNode
{
    SyntaxNode node;
    alias node this;

    Token* token;
}

/// Represents literals that have parsed as integers.
struct IntegerLiteralNode
{
    LiteralNode literal;
    alias literal this;

    long constantValue() const
    {
        return token.integer;
    }
}

/// Represents literals that have parsed as floating point numbers.
struct FloatLiteralNode
{
    LiteralNode literal;
    alias literal this;

    double constantValue() const
    {
        return token.floating;
    }
}

/// Allocates an operator node.
OperatorNode* emptyOperatorNode(TAllocator)(TAllocator allocator)
{
    auto node = cast(OperatorNode*) allocator.allocate(OperatorNode.sizeof).ptr;
    node.kind = SyntaxNodeKind.operator;
    return node;
}

/// Creates an operator node.
OperatorNode* operatorNode(TAllocator)(TAllocator allocator, SyntaxNode*[] operands, Token* operatorToken, Operator* operator)
{
    auto node = allocator.emptyOperatorNode;
    node.operands = operands;
    node.operatorToken = operatorToken;
    node.operator = operator;
    return node;
}

/// Creates an identifier node.
IdentifierNode* identifierNode(TAllocator)(TAllocator allocator, Token* token)
{
    auto node = cast(IdentifierNode*) allocator.allocate(IdentifierNode.sizeof).ptr;
    node.token = token;
    node.kind = SyntaxNodeKind.identifier;
    return node;
}

/// Creates an invocation node.
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

/// Creates a parenthsized expression node.
ParenthesizedExpressionNode* parenthesizedExpressionNode(TAllocator)(TAllocator allocator, SyntaxNode* innerExpression, Token*[2] parentheses)
{
    auto node = cast(ParenthesizedExpressionNode*) allocator.allocate(ParenthesizedExpressionNode.sizeof).ptr;
    node.innerExpression = innerExpression;
    node.parentheses = parentheses;
    node.kind = SyntaxNodeKind.parenthesizedExpression;
    return node;
}

/// Creates an integer literal node.
IntegerLiteralNode* integerLiteralNode(TAllocator)(TAllocator allocator, Token* token)
{
    auto node = cast(IntegerLiteralNode*) allocator.allocate(IntegerLiteralNode.sizeof).ptr;
    node.token = token;
    node.kind = SyntaxNodeKind.integerLiteral;
    return node;
}

/// Creates a float literal node.
FloatLiteralNode* floatLiteralNode(TAllocator)(TAllocator allocator, Token* token)
{
    auto node = cast(FloatLiteralNode*) allocator.allocate(FloatLiteralNode.sizeof).ptr;
    node.token = token;
    node.kind = SyntaxNodeKind.floatLiteral;
    return node;
}

///
StreamSpan getWholeSpan(OperatorNode* operatorNode)
{
    auto first = getWholeSpan(operatorNode.operands[0]);
    auto last = getWholeSpan(operatorNode.operands[$ - 1]);
    return StreamSpan(first.start, last.end);
}

///
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

void walk(SyntaxWalker walker, SyntaxNode* node)
{
    walker.visit(node);
}

/// This abstract class can be used to visit each node in a syntax tree,
/// without explicit switches and casts.
abstract class SyntaxWalker
{
    void visit(SyntaxNode* node)
    {
        final switch (node.kind)
        {
            case SyntaxNodeKind.operator:
            {
                auto op = cast(OperatorNode*) node;
                visit(op);
                break;
            }
            case SyntaxNodeKind.identifier:
            {
                auto id = cast(IdentifierNode*) node;
                visit(id);
                break;
            }
            case SyntaxNodeKind.invocation:
            {
                auto inv = cast(InvocationNode*) node;
                visit(inv);
                break;
            }
            case SyntaxNodeKind.parenthesizedExpression:
            {
                auto paren = cast(ParenthesizedExpressionNode*) node;
                visit(paren);
                break;
            }
            case SyntaxNodeKind.integerLiteral:
            {
                auto lit = cast(IntegerLiteralNode*) node;
                visit(lit);
                break;
            }
            case SyntaxNodeKind.floatLiteral:
            {
                auto lit = cast(FloatLiteralNode*) node;
                visit(lit);
                break;
            }
        }
    }
    void visit(OperatorNode* node)
    {
        foreach (operand; node.operands)
            visit(operand);
    }
    void visit(IdentifierNode* node)
    {
    }
    void visit(InvocationNode* node)
    {
        visit(node.identifier);
        foreach (argument; node.arguments)
            visit(argument);
    }
    void visit(ParenthesizedExpressionNode* node)
    {
        visit(node.innerExpression);
    }
    void visit(IntegerLiteralNode* node)
    {
    }
    void visit(FloatLiteralNode* node)
    {
    }
}
