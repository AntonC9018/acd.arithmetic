module app;

import std.stdio;
import acd.arithmetic;

void main()
{
    OperatorGroup[] operatorGroups = createDefaultOperatorGroups();
    SymbolTable!double symbolTable = createDefaultSymbolTable();

    const input = "1 + 2 ^ 2 * 3";
    auto lexerRange = createArithmeticLexer(input, operatorGroups);
    auto lexer = createBufferedLexer(lexerRange);
    auto parser = createParser(&lexer);
    auto tree = parser.parse();

    writeTreeRecursively(tree.root, 0);

    writeExpressionRecursively(tree.root);
    writeln();

    writeln(eval!double(symbolTable, tree.root));
}
