module acd.arithmetic.symbols;

import acd.arithmetic.syntax;
import acd.arithmetic.lexer;
import acd.arithmetic.operators;
import acd.arithmetic.internal;

import std.stdio;
import std.typecons : Nullable, nullable;
import std.meta : Repeat;
import std.range;
import std.algorithm;

enum SymbolKind
{
    variable,
    function_,
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

enum maxFunctionArity = 8;

struct Function(TNumber)
{
    int arity;
    void*[2] delegate_;

    enum maxArity = maxFunctionArity;
    Nullable!TNumber exec(scope TNumber[] arguments)
    {
        enum null_ = Nullable!TNumber.init;
        if (arguments.length != arity)
            return null_;

        switch (arity)
        {
            default:
                return null_;
            static foreach (i; 0 .. maxFunctionArity)
            {
                case i:
                {
                    alias TDelegate = TNumber delegate(Repeat!(i, TNumber));
                    TNumber[i] args = arguments[0 .. i];
                    auto dg = reinterpret!TDelegate(delegate_);
                    return nullable(dg(args.tupleof));
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

auto createFunctionGroup(TNumber, TDelegates...)(TDelegates delegates)
{
    FunctionOverloadGroupBuilder!TNumber functionOverloadGroupBuilder;
    foreach (dg; delegates)
        functionOverloadGroupBuilder.add(dg);
    return functionOverloadGroupBuilder.build();
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

    auto opBinaryRight(string op : "in")(string symbolName)
    {
        return symbolName in table;
    }

    ref Symbol!TNumber opIndex(string name)
    {
        return table[name];
    }

    AddContext set(string name)
    {
        return AddContext(&this, name);
    }

    struct AddContext
    {
        SymbolTable!TNumber* self;
        string name;

        SymbolTable!TNumber* value(TNumber variableValue)
        {
            self.table[name] = createSymbol(variableValue);
            return self;
        }

        SymbolTable!TNumber* functionGroup(FunctionOverloadGroup!TNumber functionGroup)
        {
            self.table[name] = createSymbol(functionGroup);
            return self;
        }
    }
}

/// Is supposed to be used as an extension of AddContext.
template functions(TDelegates...)
{
    SymbolTable!TNumber* functions(TNumber)(SymbolTable!TNumber.AddContext context)
    {
        FunctionOverloadGroupBuilder!TNumber group;
        static foreach (dg; TDelegates)
        {{
            import std.traits;
            enum size_t arity = numParameters!(dg, TNumber, maxFunctionArity);
            alias TArgs = Repeat!(arity, TNumber);
            group.add(delegate(TArgs args) => dg(args));
        }}
        context.self.table[context.name] = createSymbol(group.build());
        return context.self;
    }
}

// move away from aa to a pointer based or index based symbol table??
TNumber eval(TNumber, alias error = writeln)(
    SymbolTable!TNumber symbolTable, SyntaxNode* node)
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
        auto arguments = parameters
            .map!(a => eval(symbolTable, a))
            .staticArray!(func.maxArity);
        auto result = func.exec(arguments[0 .. parameters.length]);
        if (result.isNull)
        {
            error("Invalid number of arguments for ", funcName,
                " at ", invocationLocation);
            return TNumber.init;
        }

        return result.get;
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

            auto symbol = ident.name in symbolTable;
            if (!symbol)
            {
                error("Unknown identifier ", ident.name, " at ", Location(ident.token.startPosition));
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
            assert(operator.arity == operatorNode.operands.length);
            return execFunc(
                operator.name,
                operatorNode.operands,
                Location(operatorNode.operatorToken.startPosition));
        }

        case SyntaxNodeKind.invocation:
        {
            auto invocation = cast(InvocationNode*) node;
            return execFunc(
                invocation.identifier.name,
                invocation.arguments,
                Location(invocation.openParenthesis.startPosition));
        }

        case SyntaxNodeKind.parenthesizedExpression:
        {
            auto parenthesizedExpression = cast(ParenthesizedExpressionNode*) node;
            return eval(symbolTable, parenthesizedExpression.innerExpression);
        }
    }
}

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

enum CommonSymbolCategories
{
    default_,
    constants = 1 << 0,
    classicTrigonometry = 1 << 1,
    inverseTrigonometry = 1 << 2,
    hyperbolicTrigonometry = 1 << 3,
    inverseHyperbolicTrigonometry = 1 << 4,
    arithmetic = 1 << 5,
    commonNumericFunctions = 1 << 6,
    all = constants | classicTrigonometry | inverseTrigonometry | hyperbolicTrigonometry | inverseHyperbolicTrigonometry | arithmetic | commonNumericFunctions,
}

SymbolTable!TNumber createDefaultSymbolTable(TNumber = double)(CommonSymbolCategories includeCategories = CommonSymbolCategories.default_)
{
    import std.math;

    if (includeCategories == CommonSymbolCategories.default_)
    {
        includeCategories = CommonSymbolCategories.all;
    }

    SymbolTable!TNumber symbolTable;
    
    // Constants
    if (includeCategories & CommonSymbolCategories.constants)
    {
        symbolTable
            .set("pi").value(PI)
            .set("e").value(E);
    }
    
    // Classic trigonometry
    if (includeCategories & CommonSymbolCategories.classicTrigonometry)
    {
        symbolTable
            .set("sin").functions!(x => sin(x))
            .set("cos").functions!(x => cos(x))
            .set("tan").functions!(x => tan(x));
    }

    // Inverses of classic trigonometric funcitons
    if (includeCategories & CommonSymbolCategories.inverseTrigonometry)
    {
        symbolTable
            .set("atan2").functions!((x, y) => atan2(y, x))
            .set("asin").functions!(x => asin(x))
            .set("acos").functions!(x => acos(x))
            .set("atan").functions!(x => atan(x));
    }
    
    // Hyberbolic trigonometry
    if (includeCategories & CommonSymbolCategories.hyperbolicTrigonometry)
    {
        symbolTable
            .set("sinh").functions!(x => sinh(x))
            .set("cosh").functions!(x => cosh(x))
            .set("tanh").functions!(x => tanh(x));
    }

    // Inverses of hyperbolic trigonometric functions
    if (includeCategories & CommonSymbolCategories.inverseHyperbolicTrigonometry)
    {
        symbolTable
            .set("asinh").functions!(x => asinh(x))
            .set("acosh").functions!(x => acosh(x))
            .set("atanh").functions!(x => atanh(x));
    }

    // Arithmetic
    if (includeCategories & CommonSymbolCategories.arithmetic)
    {
        symbolTable
            .set("+").functions!(x => x, (x, y) => x + y)
            .set("-").functions!(x => -x, (x, y) => x - y)
            .set("*").functions!((x, y) => x * y)
            .set("/").functions!((x, y) => x / y)
            .set("^").functions!((x, y) => pow(x, y));
    }

    // Common numeric functions
    if (includeCategories & CommonSymbolCategories.commonNumericFunctions)
    {
        static if (__traits(isFloating, TNumber))
        {
            symbolTable
                .set("sqrt").functions!(x => sqrt(x))
                .set("ln").functions!(x => cast(TNumber) log(cast(TNumber) x))
                .set("log").functions!(x => cast(TNumber) log10(cast(TNumber) x))
                .set("floor").functions!(x => floor(x))
                .set("ceil").functions!(x => ceil(x))
                .set("round").functions!(x => cast(TNumber) round(x))
                .set("trunc").functions!(x => cast(TNumber) trunc(x))
                .set("frac").functions!(x => x - cast(TNumber) trunc(x))
                .set("sign").functions!(x => cast(TNumber) (signbit(x) ? -1 : 1))
                .set("exp").functions!(x => exp(x));
        }
        
        import std.algorithm : min, max;
        symbolTable
            .set("abs").functions!(x => abs(x))
            .set("min").functions!((x, y) => min(x, y))
            .set("max").functions!((x, y) => max(x, y))
            .set("mod").functions!((x, y) => x % y)
            .set("pow").functions!((x, y) => pow(x, y));

        static if (__traits(isIntegral, TNumber))
        {
            symbolTable
                .add("sign").functions!(x => sign(x));
        }
    }

    return symbolTable;
}
