/**
 * CPSC 326, Spring 2025
 * Pretty print visitor.
 *
 * Alexa Williams
 */
package cpsc326;

public class PrintVisitor implements Visitor {

    private int indent = 0;

    /**
     * Prints message without ending newline
     */
    private void write(String s) {
        System.out.print(s);
    }

    /**
     * Increase the indent level by one
     */
    private void incIndent() {
        indent++;
    }

    /**
     * Decrease the indent level by one
     */
    private void decIndent() {
        indent--;
    }

    /**
     * Print an initial indent string
     */
    private String indent() {
        return "  ".repeat(indent);
    }

    /**
     * Prints a newline
     */
    private void newline() {
        System.out.println();
    }

    /**
     * Prints the program
     */
    public void visit(Program node) {
        // always one blank line at the "top"
        newline();
        for (StructDef s : node.structs) {
            s.accept(this);
        }
        for (FunDef f : node.functions) {
            f.accept(this);
        }
    }

    // general AST classes
    @Override
    public void visit(FunDef node) {
        visit(node.returnType);
        write(" " + node.funName.lexeme + "(");
        for (int i = 0; i < node.params.size(); i++) {
            node.params.get(i).accept(this);
            if (i + 1 != node.params.size()) {
                write(", ");
            }
        }
        write(") {");
        newline();
        incIndent();
        for (int i = 0; i < node.stmts.size(); i++) {
            write(indent());
            node.stmts.get(i).accept(this);
            newline();
        }
        decIndent();
        write(indent() + "}");
        newline();
        newline();
    }

    @Override
    public void visit(StructDef node) {
        write("struct " + node.structName.lexeme + " {");
        newline();
        incIndent();
        for (int i = 0; i < node.fields.size(); i++) {
            write(indent());
            node.fields.get(i).accept(this);
            if (i + 1 != node.fields.size()) {
                write(", ");
            }
            newline();
        }
        decIndent();
        write(indent() + "}");
        newline();
        newline();
    }

    @Override
    public void visit(DataType node) {
        if (node.isArray) {
            write("[" + node.type.lexeme + "]");
        } else {
            write(node.type.lexeme);
        }
    }

    @Override
    public void visit(VarDef node) {
        write(node.varName.lexeme + ": ");
        node.dataType.accept(this);
    }

    // statements
    @Override
    public void visit(ReturnStmt node) {
        write("return ");
        node.expr.accept(this);
    }

    @Override
    public void visit(VarStmt node) {
        if (node.dataType.isEmpty()) {
            write("var" + node.varName.lexeme);
        } else {
            write("var " + node.varName.lexeme + ": " + node.dataType.get().type.lexeme + " = ");
            if (!(node.expr.isEmpty())) {
                node.expr.get().accept(this);
            }
        }
    }

    @Override
    public void visit(AssignStmt node) {
        for (int i = 0; i < node.lvalue.size(); i++) {
            if (!(node.lvalue.get(i).arrayExpr.isEmpty())) {
                write(node.lvalue.get(i).varName.lexeme);
                write("[");
                node.lvalue.get(i).arrayExpr.get().accept(this);
                write("]");
                if (i + 1 != node.lvalue.size()) {
                    write(".");
                }
            } else {
                if (i + 1 != node.lvalue.size()) {
                    write(node.lvalue.get(i).varName.lexeme + ".");
                } else {
                    write(node.lvalue.get(i).varName.lexeme);
                }
                if (!(node.lvalue.get(i).arrayExpr.isEmpty())) {
                    node.lvalue.get(i).arrayExpr.get().accept(this);
                }
            }
        }
        write(" = ");
        node.expr.accept(this);
    }

    @Override
    public void visit(WhileStmt node) {
        write("while ");
        node.condition.accept(this);
        write(" {");
        newline();
        incIndent();
        for (int i = 0; i < node.stmts.size(); i++) {
            write(indent());
            node.stmts.get(i).accept(this);
            newline();
        }
        decIndent();
        write(indent() + "}");
    }

    @Override
    public void visit(ForStmt node) {
        write("for " + node.varName.lexeme + " from ");
        node.fromExpr.accept(this);
        write(" to ");
        node.toExpr.accept(this);
        write(" {");
        newline();
        incIndent();
        for (int i = 0; i < node.stmts.size(); i++) {
            write(indent());
            node.stmts.get(i).accept(this);
            newline();
        }
        decIndent();
        write(indent() + "}");
    }

    @Override
    public void visit(IfStmt node) {
        write("if ");
        node.condition.accept(this);
        write(" {");
        newline();
        incIndent();
        for (int i = 0; i < node.ifStmts.size(); i++) {
            write(indent());
            node.ifStmts.get(i).accept(this);
            newline();
        }
        decIndent();
        write(indent() + "}");
        if (!node.elseIf.isEmpty()) {
            newline();
            write(indent());
            write("else ");
            node.elseIf.get().accept(this);
        }

        if (!node.elseStmts.isEmpty()) {
            newline();
            write(indent());
            write("else {");
            incIndent();
            for (int i = 0; i < node.elseStmts.get().size(); i++) {
                newline();
                write(indent());
                node.elseStmts.get().get(i).accept(this);
            }
            newline();
            decIndent();
            write(indent() + "}");
        }
    }

    // expressions
    @Override
    public void visit(BasicExpr node) {
        node.rvalue.accept(this);
    }

    @Override
    public void visit(UnaryExpr node) {
        write(node.unaryOp.lexeme + " (");
        node.expr.accept(this);
        write(")");
    }

    @Override
    public void visit(BinaryExpr node) {
        write("(");
        node.lhs.accept(this);
        write(" ");
        write(node.binaryOp.lexeme);
        write(" ");
        node.rhs.accept(this);
        write(")");
    }

    @Override
    public void visit(CallRValue node) {
        write(node.funName.lexeme + "(");
        for (int i = 0; i < node.args.size(); i++) {
            node.args.get(i).accept(this);
            if (i + 1 != node.args.size()) {
                write(", ");
            }
        }
        write(")");
    }

    @Override
    public void visit(SimpleRValue node) {
        if (node.literal.tokenType == TokenType.STRING_VAL) {
            write("\"" + node.literal.lexeme + "\"");
        } else {
            write(node.literal.lexeme);
        }
    }

    @Override
    public void visit(NewStructRValue node) {
        write("new " + node.structName.lexeme + "(");
        for (int i = 0; i < node.args.size(); i++) {
            node.args.get(i).accept(this);
            if (i + 1 != node.args.size()) {
                write(", ");
            }
        }
        write(")");
    }

    @Override
    public void visit(NewArrayRValue node) {
        write("new " + node.type.lexeme + "[");
        node.arrayExpr.accept(this);
        write("]");
    }

    @Override
    public void visit(VarRValue node) {
        for (int i = 0; i < node.path.size(); i++) {
            if (!(node.path.get(i).arrayExpr.isEmpty())) {
                write(node.path.get(i).varName.lexeme);
                write("[");
                node.path.get(i).arrayExpr.get().accept(this);
                write("]");
            } else {
                write(node.path.get(i).varName.lexeme);
            }

            if (i + 1 != node.path.size()) {
                write(".");
            }
        }
    }
}
