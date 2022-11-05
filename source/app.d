import std.stdio;
import acd.arithmetic;

void main()
{
    OperatorGroup[] operatorGroups = 
    [
        operatorGroup("-")
            .add(OperatorArity.unary, OperatorAssociativity.right, 2)
            .build(),
        
        operatorGroup("~")
            .add(OperatorArity.unary, OperatorAssociativity.right, 3)
            .build(),
            
        operatorGroup("!")
            .add(OperatorArity.unary, OperatorAssociativity.left, 3)
            .build(),

        operatorGroup("$")
            .add(OperatorArity.unary, OperatorAssociativity.left, 4)
            .build(),
    ];

    auto expressionTree = parseExpression("-~-~Variable!$!$", operatorGroups);
    if (expressionTree.thereHaveBeenErrors || expressionTree.root is null)
    {
        writeln("Error parsing expression");
        return;
    }

    writeExpression(expressionTree.root);
    writeln();
    writeTree(expressionTree.root);
}


void main1()
{
    // The default operators include the basic arithmetic operators.
    OperatorGroup[] operatorGroups = createDefaultOperatorGroups();

    auto expressionTree = parseExpression("x + y ^ 2 * sin(x)", operatorGroups);
    if (expressionTree.thereHaveBeenErrors || expressionTree.root is null)
    {
        writeln("Error parsing expression");
        return;
    }

    // Prints the expression back in text form.
    writeExpression(expressionTree.root);

    // The default symbol table includes trig functions, the constants pi and e,
    // and some common math functions.
    SymbolTable!double symbolTable = createDefaultSymbolTable();

    // You can add your own values for the variables or constants.    
    symbolTable
        .set("x").value(1.0)
        .set("y").value(2.0);

    // You can also add your own functions.
    symbolTable
        .set("square").functions!(x => x ^^ 2);

    {
        double result = eval(symbolTable, expressionTree.root);
        writeln(result);
    }

    // Can change values
    symbolTable.set("x").value(2.0);

    {
        double result = eval(symbolTable, expressionTree.root);
        writeln(result);
    }
    
    // Can reuse the symbol table for other expressions
    expressionTree = parseExpression("x * cos(y)", operatorGroups);
    if (expressionTree.thereHaveBeenErrors || expressionTree.root is null)
    {
        writeln("Error parsing expression");
        return;
    }
    {
        double result = eval(symbolTable, expressionTree.root);
        writeln(result);
    }
}
