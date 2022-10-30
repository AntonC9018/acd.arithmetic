module acd.arithmetic.operators;

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

OperatorGroup[] createDefaultOperatorGroups()
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
    return operatorGroups;
}