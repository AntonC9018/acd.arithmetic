# Arithmetic expression parser

This library is a powerful recursive descent arithmetic expression parser.
The precedence is handled via the concept of [precedence climbing](https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing).

The precedence and associativity are handled for both unary and binary operators.
For binary operators, associativity indicates how operations are group, e.g. `a + b + c` gets parsed as `((a + b) + c)` if `+` is defined as left associative, and as `(a + (b + c))` if `+` is left associative.
For unary operators, associativity indicates which side the operator is to be expected.

Features:
* Lexing and parsing of an expression to a syntax tree;
* Evalutation of a parsed expression;
* The symbol table is separated from the syntax tree, which basically makes parsed expressions safe to use in multithreaded contexts, given the threads use a different symbol table.

Unimplemented features:
* Ease of use and full coverage of allocators;
* Syntax tree optimizations;
* Expression compilation to native code;
* Allow multilayered symbol tables with a fallback lookup mechanism, so you don't have to copy the symbols to the new table;
* Better error handling (currently an error handler is used to display errors, but it could be better);
* A configuration mechanism for building the right lexers and parsers.


> I won't promise the library is the fastest or the best there is,
> but it will get the job done for basic applications for sure.


## Examples

```d
import std.stdio;
import acd.arithmetic;

void main()
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
```


```d
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

    /*
      (-(~(-(((((~Variable)!)$)!)$))))

      operator: -
        operator: ~
          operator: -
            operator: $
              operator: !
                operator: $
                  operator: !
                    operator: ~
                      identifier: Variable
    */
}
```