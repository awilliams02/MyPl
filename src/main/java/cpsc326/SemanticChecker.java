/**
 * CPSC 326, Spring 2025
 * The Semantic Checker implementation.
 *
 * Alexa Williams
 */
package cpsc326;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SemanticChecker implements Visitor {

    // for tracking function and struct definitions: 
    private Map<String, FunDef> functions = new HashMap<>();
    private Map<String, StructDef> structs = new HashMap<>();
    // for tracking variable types:
    private SymbolTable symbolTable = new SymbolTable();
    // for holding the last inferred type:
    private DataType currType;

    //----------------------------------------------------------------------
    // Helper functions
    //----------------------------------------------------------------------
    /**
     */
    private boolean isBaseType(String type) {
        return List.of("int", "double", "bool", "string").contains(type);
    }

    /**
     */
    private boolean isBuiltInFunction(String name) {
        return List.of("print", "println", "readln", "size", "get", "int_val",
                "dbl_val", "str_val").contains(name);
    }

    /**
     * Create an error message
     */
    private void error(String msg) {
        MyPLException.staticError(msg);
    }

    /**
     * Creates an error
     */
    private void error(String msg, Token token) {
        String s = "[%d,%d] %s";
        MyPLException.staticError(String.format(s, token.line, token.column, msg));
    }

    /**
     * Checks if the field name is a field in the struct definition. This is a
     * helper method for checking and inferring assignment statement lvalues and
     * var rvalue paths.
     *
     * @param fieldName the field name to check for
     * @param structDef the struct definition to check
     * @returns true if a match and false otherwise
     */
    private boolean isStructField(String fieldName, StructDef structDef) {
        for (var field : structDef.fields) {
            if (field.varName.lexeme.equals(fieldName)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Obtains the data type for the field name in the struct definition. This
     * is a helper method for checking and inferring assignment statement
     * lvalues and var rvalue paths.
     *
     * @param fieldName the field name
     * @param structDef the struct definition
     * @returns the corresponding data type or null if no such field exists
     */
    private DataType getStructFieldType(String fieldName, StructDef structDef) {
        for (var field : structDef.fields) {
            if (field.varName.lexeme.equals(fieldName)) {
                return field.dataType;
            }
        }
        return null;
    }

    //----------------------------------------------------------------------
    // Visit Functions
    //----------------------------------------------------------------------
    /**
     * Checks the program
     */
    public void visit(Program node) {
        for (StructDef s : node.structs) {
            if (structs.containsKey(s.structName.lexeme)) {
                error("duplicate struct name" + s.structName.lexeme);
            }
            structs.put(s.structName.lexeme, s);
        }
        for (StructDef s : node.structs) {
            s.accept(this);
        }
        for (FunDef f : node.functions) {
            if (isBuiltInFunction(f.funName.lexeme)) {
                error("cannot redefine built-in functions");
            }
            if (functions.containsKey(f.funName.lexeme)) {
                error("duplicate function name" + f.funName.lexeme);
            }
            functions.put(f.funName.lexeme, f);
        }
        for (FunDef f : node.functions) {
            f.accept(this);
        }
        if (!(functions.containsKey("main"))) {
            error("expected main function");
        }
    }

    /**
     * Checks a function definition signature and body ensuring valid data types
     * and no duplicate parameters
     */
    public void visit(FunDef node) {
        String funName = node.funName.lexeme;
        if (funName.equals("main")) {
            if (!(node.params.isEmpty())) {
                error("main function cannot have params");
            }
            if (!(node.returnType.type.lexeme.equals("void"))) {
                error("main function must have void return type");
            }
            symbolTable.pushEnvironment();
            node.returnType.accept(this);
            symbolTable.add("return", currType);
            for (Stmt s : node.stmts) {
                s.accept(this);
            }
            symbolTable.popEnvironment();
        } else {
            symbolTable.pushEnvironment();
            for (VarDef p : node.params) {
                if (symbolTable.existsInCurrEnv(p.varName.lexeme)) {
                    error("duplicate param name:" + p.varName.lexeme);
                }
                p.accept(this);
            }
            node.returnType.accept(this);
            symbolTable.add("return", currType);
            for (Stmt s : node.stmts) {
                s.accept(this);
            }
            symbolTable.popEnvironment();
        }
    }

    /**
     * Checks structs for duplicate fields and valid data types
     */
    public void visit(StructDef node) {
        String structName = node.structName.lexeme;
        symbolTable.pushEnvironment();
        for (VarDef f : node.fields) {
            if (symbolTable.existsInCurrEnv(f.varName.lexeme)) {
                error("duplicate struct field names: " + f.varName.lexeme);
            }
            f.accept(this);
        }
        symbolTable.popEnvironment();
    }

    /**
     * Checks DataTypes to make sure they exist
     */
    public void visit(DataType node) {
        String typeName = node.type.lexeme;
        if (!isBaseType(typeName) && !typeName.equals("void")) {
            if (!(structs.containsKey(typeName))) {
                error("no such type: " + typeName);
            }
        }
        currType = new DataType();
        currType.type = new Token(node.type.tokenType, node.type.lexeme, node.type.line, node.type.column);
        if (node.isArray) {
            currType.isArray = true;
        }
    }

    /**
     * Checks VarDefs and adds them to the symbolTable
     */
    public void visit(VarDef node) {
        String varName = node.varName.lexeme;
        node.dataType.accept(this);
        symbolTable.add(varName, node.dataType);
    }

    /**
     * Checks ReturnStmts as being valid and adds binding to symbolTable
     */
    public void visit(ReturnStmt node) {
        node.expr.accept(this);
        DataType returnType = symbolTable.get("return");
        if (!currType.type.lexeme.equals(returnType.type.lexeme)) {
            if (!currType.type.lexeme.equals("void")) {
                error("bad return type");
            }
        }
        currType = new DataType();
        currType = returnType;
    }

    /**
     * Checks VarStmts as being valid and adds binding to symbolTable
     */
    public void visit(VarStmt node) {
        String varName = node.varName.lexeme;
        DataType exprType;
        if (symbolTable.existsInCurrEnv(varName)) {
            error("illegal shadowing of:" + varName);
        }
        if (!node.expr.isEmpty()) {
            node.expr.get().accept(this);
            exprType = currType;
            if (currType.isArray) {
                exprType.isArray = true;
            } else {
                exprType.isArray = false;
            }
        } else {
            Token t = new Token(TokenType.VOID_TYPE, "void", node.varName.line, node.varName.column);
            currType = new DataType();
            currType.type = t;
            exprType = currType;
        }
        DataType dType = new DataType();
        if (!node.dataType.isEmpty()) {
            node.dataType.get().accept(this);
            dType = currType;
            if (currType.isArray) {
                dType.isArray = true;
            } else {
                dType.isArray = false;
            }
            if (!exprType.type.lexeme.equals(dType.type.lexeme)) {
                if (!exprType.type.lexeme.equals("void")) {
                    error("VarStmt has incompatible types: ");
                }
            }
        } else {
            if (exprType.type.lexeme.equals("void")) {
                error("unable to infer type");
            }
            dType = exprType;
        }
        if (dType.isArray) {
            if (!exprType.isArray) {
                if (!exprType.type.lexeme.equals("void")) {
                    error("array type mismatch");
                }
            }
        }
        if (exprType.isArray) {
            if (!dType.isArray) {
                error("type mismatch error");
            }
        }
        symbolTable.add(varName, dType);
    }

    /**
     * Checks AssignStmts as being valid
     */
    public void visit(AssignStmt node) {
        if (node.lvalue.size() == 1) {
            DataType lType = symbolTable.get(node.lvalue.get(0).varName.lexeme);
            node.expr.accept(this);
            if (!currType.type.lexeme.equals(lType.type.lexeme) && !currType.type.lexeme.equals("void")) {
                error("type mismatch for assign stmt");
            }
            if (!lType.isArray && currType.isArray) {
                if (!currType.type.lexeme.equals("void")) {
                    error("array type mismatch");
                }
            }
            currType = new DataType();
            currType.type = new Token(lType.type.tokenType, lType.type.lexeme, lType.type.line, lType.type.column);
        } else {
            String varName = node.lvalue.get(0).varName.lexeme;
            StructDef structDef = new StructDef();
            DataType lType = new DataType();
            DataType var = symbolTable.get(varName);
            if (structs.containsKey(var.type.lexeme)) {
                structDef = structs.get(var.type.lexeme);
            } else {
                error("variable " + varName + " does not exist");
            }
            for (int i = 1; i < node.lvalue.size(); i++) {
                varName = node.lvalue.get(i).varName.lexeme;
                if (!isStructField(varName, structDef)) {
                    error("struct field " + varName + " does not exist");
                } else {
                    lType = new DataType();
                    lType = getStructFieldType(varName, structDef);
                    if (lType.isArray) {
                        if (node.lvalue.get(i).arrayExpr.isEmpty() && (i + 1 < node.lvalue.size())) {
                            error("bad array expr ");
                        }
                    }
                }
                if (i + 1 < node.lvalue.size()) {
                    var = getStructFieldType(varName, structDef);
                    if (!structs.containsKey(var.type.lexeme)) {
                        error("struct " + varName + " does not exist");
                    }
                    structDef = structs.get(var.type.lexeme);

                }
            }
            node.expr.accept(this);
            if (!currType.type.lexeme.equals(lType.type.lexeme) && !currType.type.lexeme.equals("void")) {
                error("type mismatch for assign stmt");
            }
            currType = new DataType();
            currType.type = new Token(lType.type.tokenType, lType.type.lexeme, lType.type.line, lType.type.column);
        }
    }

    /**
     * Checks WhileStmts as being valid
     */
    public void visit(WhileStmt node) {
        node.condition.accept(this);
        if (currType.type.tokenType != TokenType.BOOL_TYPE) {
            error("condition must evaluate to boolean value");
        }
        if (currType.isArray) {
            error("illegal boolean array condition");
        }
        symbolTable.pushEnvironment();
        for (Stmt s : node.stmts) {
            s.accept(this);
        }
        symbolTable.popEnvironment();
    }

    /**
     * Checks ForStmts as being valid
     */
    public void visit(ForStmt node) {
        symbolTable.pushEnvironment();
        String varName = node.varName.lexeme;
        currType = new DataType();
        currType.type = new Token(TokenType.INT_TYPE, "int", node.varName.line, node.varName.column);
        symbolTable.add(varName, currType);
        node.fromExpr.accept(this);
        if (!currType.type.lexeme.equals("int")) {
            error("from expression must be an INT type");
        }
        node.toExpr.accept(this);
        if (!currType.type.lexeme.equals("int")) {
            error("to expression must be an INT type");
        }
        for (Stmt s : node.stmts) {
            s.accept(this);
        }
        symbolTable.popEnvironment();
    }

    /**
     * Checks IfStmts as being valid
     */
    public void visit(IfStmt node) {
        node.condition.accept(this);
        if (currType.type.tokenType != TokenType.BOOL_TYPE) {
            error("if condition must evaluate to BOOL value");
        }
        if (currType.isArray) {
            error("illegal boolean array condition");
        }
        symbolTable.pushEnvironment();
        for (Stmt s : node.ifStmts) {
            s.accept(this);
        }
        symbolTable.popEnvironment();
        if (!node.elseIf.isEmpty()) {
            symbolTable.pushEnvironment();
            node.elseIf.get().accept(this);
            symbolTable.popEnvironment();
        }
        if (!node.elseStmts.isEmpty()) {
            symbolTable.pushEnvironment();
            for (Stmt e : node.elseStmts.get()) {
                e.accept(this);
            }
            symbolTable.popEnvironment();
        }
    }

    /**
     * Checks BasicExpr as being valid
     */
    public void visit(BasicExpr node) {
        node.rvalue.accept(this);
    }

    /**
     * Checks UnaryExpr as being valid
     */
    public void visit(UnaryExpr node) {
        node.expr.accept(this);
        if (node.unaryOp.lexeme.equals("not")) {
            if (!currType.type.lexeme.equals("bool") || currType.isArray) {
                error("expecting boolean expression");
            }
        }
        currType = new DataType();
        currType.type = new Token(TokenType.BOOL_TYPE, "bool", node.unaryOp.line, node.unaryOp.column);
    }

    /**
     * Checks BinaryExpr as being valid and sets the currType
     */
    public void visit(BinaryExpr node) {
        DataType lhs;
        DataType rhs;
        node.lhs.accept(this);
        lhs = currType;
        node.rhs.accept(this);
        rhs = currType;
        Token op = node.binaryOp;
        int line = node.binaryOp.line;
        int column = node.binaryOp.column;

        if (lhs.type.lexeme.equals("string") && rhs.type.lexeme.equals("string") && op.lexeme.equals("+") && !lhs.isArray && !rhs.isArray) {
            currType = new DataType();
            currType.type = new Token(TokenType.STRING_TYPE, "string", line, column);
        } else if (((lhs.type.lexeme.equals("int") && rhs.type.lexeme.equals("int")) || (lhs.type.lexeme.equals("double") && rhs.type.lexeme.equals("double"))) && (op.lexeme.equals("+") || op.lexeme.equals("-") || op.lexeme.equals("*") || op.lexeme.equals("/")) && !lhs.isArray && !rhs.isArray) {
            currType = new DataType();
            currType.type = new Token(lhs.type.tokenType, lhs.type.lexeme, line, column);
        } else if ((lhs.type.lexeme.equals(rhs.type.lexeme) || lhs.type.lexeme.equals("void") || rhs.type.lexeme.equals("void")) && (op.lexeme.equals("==") || op.lexeme.equals("!="))) {
            currType = new DataType();
            currType.type = new Token(TokenType.BOOL_TYPE, "bool", line, column);
        } else if (((lhs.type.lexeme.equals("int") && (rhs.type.lexeme.equals("int"))) || (lhs.type.lexeme.equals("double") && (rhs.type.lexeme.equals("double"))) || (lhs.type.lexeme.equals("string") && (rhs.type.lexeme.equals("string"))) && (op.lexeme.equals("<") || op.lexeme.equals(">") || op.lexeme.equals("<=") || op.lexeme.equals(">="))) && !lhs.isArray && !rhs.isArray) {
            currType = new DataType();
            currType.type = new Token(TokenType.BOOL_TYPE, "bool", line, column);
        } else if (lhs.type.lexeme.equals("bool") && rhs.type.lexeme.equals("bool") && (op.lexeme.equals("and") || op.lexeme.equals("or")) && !lhs.isArray && !rhs.isArray) {
            currType = new DataType();
            currType.type = new Token(TokenType.BOOL_TYPE, "bool", line, column);
        } else {
            error("expression types are not compatable");
        }
    }

    /**
     * Checks CallRValue as being valid and sets currType
     */
    public void visit(CallRValue node) {
        String funName = node.funName.lexeme;
        if (!functions.containsKey(funName)) {
            if (!isBuiltInFunction(funName)) {
                error("function " + funName + " does not exist");
            }
        }
        if (funName.equals("print") || funName.equals("println")) {
            if (node.args.size() != 1) {
                error("can only take one arg");
            }
            node.args.get(0).accept(this);
            if (!currType.type.lexeme.equals("string") && !isBaseType(currType.type.lexeme) && !currType.type.lexeme.equals("void")) {
                error("type mismatch error");
            }
            if (currType.isArray) {
                error("cannot print array type");
            }
            currType = new DataType();
            currType.type = new Token(TokenType.VOID_TYPE, "void", 0, 0);
        } else if (funName.equals("readln")) {
            if (node.args.size() != 0) {
                error("too many args");
            }
            currType = new DataType();
            currType.type = new Token(TokenType.STRING_TYPE, "string", 0, 0);
        } else if (funName.equals("size")) {
            if (node.args.size() != 1) {
                error("too many args");
            }
            node.args.get(0).accept(this);
            if (!currType.type.lexeme.equals("string") && !currType.isArray) {
                error("type mismatch for size function");
            }
            currType = new DataType();
            currType.type = new Token(TokenType.INT_TYPE, "int", 0, 0);
        } else if (funName.equals("get")) {
            if (node.args.size() != 2) {
                error("wrong number of args for get function");
            }
            node.args.get(0).accept(this);
            if (!currType.type.lexeme.equals("int")) {
                error("wrong arg type for get function");
            }
            node.args.get(1).accept(this);
            if (!currType.type.lexeme.equals("string") && !currType.isArray) {
                error("wrong arg type for get function");
            }
            DataType t = currType;
            if (t.type.lexeme.equals("string")) {
                currType = new DataType();
                currType.type = new Token(TokenType.STRING_TYPE, "string", 0, 0);
            } else {
                currType = new DataType();
                currType.type = new Token(t.type.tokenType, t.type.lexeme, 0, 0);
            }
        } else if (funName.equals("int_val")) {
            if (node.args.size() != 1) {
                error("wrong number of args for int_val function");
            }
            node.args.get(0).accept(this);
            if (!currType.type.lexeme.equals("double") && !currType.type.lexeme.equals("string")) {
                error("wrong arg type for int_val function");
            }
            currType = new DataType();
            currType.type = new Token(TokenType.INT_TYPE, "int", 0, 0);
        } else if (funName.equals("dbl_val")) {
            if (node.args.size() != 1) {
                error("wrong number of args for dbl_val function");
            }
            node.args.get(0).accept(this);
            if (!currType.type.lexeme.equals("int") && !currType.type.lexeme.equals("string")) {
                error("wrong arg type for dbl_val function");
            }
            currType = new DataType();
            currType.type = new Token(TokenType.DOUBLE_VAL, "double", 0, 0);
        } else if (funName.equals("str_val")) {
            if (node.args.size() != 1) {
                error("wrong number of args for str_val function");
            }
            node.args.get(0).accept(this);
            if (!(currType.type.lexeme.equals("int") || currType.type.lexeme.equals("double"))) {
                error("wrong arg type for str_val function");
            }
            currType = new DataType();
            currType.type = new Token(TokenType.STRING_TYPE, "string", 0, 0);
        } else {

            FunDef function = functions.get(funName);
            if (node.args.size() != function.params.size()) {
                error("number of args do not match");
            }
            int i = 0;
            for (Expr a : node.args) {
                a.accept(this);
                VarDef thisParam = function.params.get(i);
                if (!thisParam.dataType.type.lexeme.equals(currType.type.lexeme)) {
                    if (!currType.type.lexeme.equals("void")) {
                        error("incompatible param type" + thisParam.dataType.type.lexeme + " " + currType.type.lexeme + currType.type.line);
                    }
                }
                if (thisParam.dataType.isArray) {
                    if (!currType.isArray) {
                        error("incompatible param type, missing array");
                    }
                }
                if (!thisParam.dataType.isArray) {
                    if (currType.isArray) {
                        error("incompatible param type, non array param");
                    }
                }
                i++;
            }
            Token returnType = function.returnType.type;
            currType = new DataType();
            currType.type = new Token(returnType.tokenType, returnType.lexeme, node.funName.line, node.funName.column);
            if (function.returnType.isArray) {
                currType.isArray = true;
            }
        }
    }

    /**
     * Checks SimpleRValue as being valid and sets the currType
     */
    public void visit(SimpleRValue node) {
        TokenType literalType = node.literal.tokenType;
        int line = node.literal.line;
        int column = node.literal.column;
        Token typeToken = null;
        if (literalType == TokenType.INT_VAL) {
            typeToken = new Token(TokenType.INT_TYPE, "int", line, column);
        } else if (literalType == TokenType.STRING_VAL) {
            typeToken = new Token(TokenType.STRING_TYPE, "string", line, column);
        } else if (literalType == TokenType.DOUBLE_VAL) {
            typeToken = new Token(TokenType.DOUBLE_TYPE, "double", line, column);
        } else if (literalType == TokenType.BOOL_VAL) {
            typeToken = new Token(TokenType.BOOL_TYPE, "bool", line, column);
        } else if (literalType == TokenType.NULL_VAL) {
            typeToken = new Token(TokenType.VOID_TYPE, "void", line, column);
        }
        currType = new DataType();
        currType.type = typeToken;
    }

    /**
     * Checks NewStructRValue as being valid
     */
    public void visit(NewStructRValue node) {
        String structName = node.structName.lexeme;
        int line = node.structName.line;
        int column = node.structName.column;
        if (!structs.containsKey(structName)) {
            error("type " + structName + " does not exist");
        }

        StructDef structDef = structs.get(structName);
        if (structDef.fields.size() != node.args.size()) {
            error("number of args does not match");
        }
        int i = 0;
        for (Expr a : node.args) {
            a.accept(this);
            if (!currType.type.lexeme.equals(structDef.fields.get(i).dataType.type.lexeme) && !currType.type.lexeme.equals("void")) {
                error("wrong field type");
            }
            i++;
        }

        currType = new DataType();
        currType.type = new Token(TokenType.STRUCT, structName, line, column);
    }

    /**
     * Checks NewArrayRValue as being valid
     */
    public void visit(NewArrayRValue node) {
        if (!isBaseType(node.type.lexeme)) {
            if (!structs.containsKey(node.type.lexeme)) {
                error("cannot create array because this type does not exist");
            }
        }
        currType = new DataType();
        currType.type = new Token(node.type.tokenType, node.type.lexeme, node.type.line, node.type.column);
        currType.isArray = true;
    }

    /**
     * Checks VarRValue as being valid and sets the currType
     */
    public void visit(VarRValue node) {
        Boolean flag = false;
        if (node.path.size() == 1) {
            if (!symbolTable.exists(node.path.get(0).varName.lexeme)) {
                error("variable does not exist");
            }
            DataType lType = new DataType();
            lType.type = symbolTable.get(node.path.get(0).varName.lexeme).type;
            if (symbolTable.get(node.path.get(0).varName.lexeme).isArray) {
                if (!node.path.get(0).arrayExpr.isEmpty()) {
                    lType.isArray = false;
                } else {
                    lType.isArray = true;
                }
            }
            currType = new DataType();
            currType.type = new Token(lType.type.tokenType, lType.type.lexeme, lType.type.line, lType.type.column);
            if (lType.isArray) {
                currType.isArray = true;
            } else {
                currType.isArray = false;
            }
        } else {
            String varName = node.path.get(0).varName.lexeme;
            StructDef structDef = new StructDef();
            DataType lType = new DataType();
            if (!symbolTable.exists(varName)) {
                error("variable does not exist");
            }
            DataType var = symbolTable.get(varName);
            if (structs.containsKey(var.type.lexeme)) {
                structDef = structs.get(var.type.lexeme);
            } else {
                error("variable " + varName + " does not exist");
            }
            for (int i = 1; i < node.path.size(); i++) {
                varName = node.path.get(i).varName.lexeme;
                if (!isStructField(varName, structDef)) {
                    error("struct field " + varName + " does not exist");
                } else {
                    lType = new DataType();
                    lType = getStructFieldType(varName, structDef);
                    if (lType.isArray) {
                        if (node.path.get(i).arrayExpr.isEmpty()) {
                            error("bad array expr");
                        }
                    }
                }
                if (i + 1 < node.path.size()) {
                    var = getStructFieldType(varName, structDef);
                    if (!structs.containsKey(var.type.lexeme)) {
                        error("struct " + varName + " does not exist");
                    }
                    structDef = structs.get(var.type.lexeme);
                }
            }
            currType = new DataType();
            currType.type = new Token(lType.type.tokenType, lType.type.lexeme, lType.type.line, lType.type.column);
            if (lType.isArray) {
                currType.isArray = true;
            }
        }
    }
}
