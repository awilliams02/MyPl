/**
 * CPSC 326, Spring 2025
 * The AST Parser implementation.
 *
 * Alexa Williams
 */
package cpsc326;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Simple recursive descent parser for checking program syntax.
 */
public class ASTParser {

    private Lexer lexer;          // the lexer
    private Token currToken;      // the current token

    /**
     * Create a SimpleParser from the give lexer.
     *
     * @param lexer The lexer for the program to parse.
     */
    public ASTParser(Lexer lexer) {
        this.lexer = lexer;
    }

    /**
     * Run the parser.
     */
    public Program parse() {
        advance();
        Program p = program();
        eat(TokenType.EOS, "expecting end of file");
        return p;
    }

    /**
     * Generate and throw a mypl parser exception.
     *
     * @param msg The error message.
     */
    private void error(String msg) {
        String lexeme = currToken.lexeme;
        int line = currToken.line;
        int column = currToken.column;
        String s = "[%d,%d] %s found '%s'";
        MyPLException.parseError(String.format(s, line, column, msg, lexeme));
    }

    /**
     * Move to the next lexer token, skipping comments.
     */
    private void advance() {
        currToken = lexer.nextToken();
        while (match(TokenType.COMMENT)) {
            currToken = lexer.nextToken();
        }
    }

    /**
     * Checks that the current token has the given token type.
     *
     * @param targetTokenType The token type to check against.
     * @return True if the types match, false otherwise.
     */
    private boolean match(TokenType targetTokenType) {
        return currToken.tokenType == targetTokenType;
    }

    /**
     * Checks that the current token is contained in the given list of token
     * types.
     *
     * @param targetTokenTypes The token types ot check against.
     * @return True if the current type is in the given list, false otherwise.
     */
    private boolean matchAny(List<TokenType> targetTokenTypes) {
        return targetTokenTypes.contains(currToken.tokenType);
    }

    /**
     * Advance to next token if current token matches the given token type.
     *
     * @param targetType The token type to check against.
     */
    private void eat(TokenType targetTokenType, String msg) {
        if (!match(targetTokenType)) {
            error(msg);
        }
        advance();
    }

    /**
     * Check if the current token is an allowed binary operator
     */
    private boolean isBinOp() {
        return matchAny(List.of(TokenType.PLUS, TokenType.MINUS, TokenType.TIMES,
                TokenType.DIVIDE, TokenType.AND, TokenType.OR,
                TokenType.EQUAL, TokenType.LESS, TokenType.GREATER,
                TokenType.LESS_EQ, TokenType.GREATER_EQ,
                TokenType.NOT_EQUAL));
    }

    /**
     * Check if the current token is a literal value
     */
    private boolean isLiteral() {
        return matchAny(List.of(TokenType.INT_VAL, TokenType.DOUBLE_VAL,
                TokenType.STRING_VAL, TokenType.BOOL_VAL,
                TokenType.NULL_VAL));
    }

    /**
     * Checks for a valid program.
     */
    private Program program() {
        Program p = new Program();
        while (matchAny(List.of(TokenType.STRUCT, TokenType.INT_TYPE, TokenType.DOUBLE_TYPE, TokenType.STRING_TYPE, TokenType.BOOL_TYPE, TokenType.ID, TokenType.LBRACKET, TokenType.VOID_TYPE))) {
            if (match(TokenType.STRUCT)) {
                p.structs.add(structDef());
            } else {
                p.functions.add(funDef());
            }
        }
        return p;
    }

    /**
     * Checks for a valid struct definition.
     */
    private StructDef structDef() {
        StructDef s = new StructDef();
        eat(TokenType.STRUCT, "expected STRUCT");
        s.structName = currToken;
        eat(TokenType.ID, "expected ID");
        eat(TokenType.LBRACE, "expected {");
        s.fields = fields();
        eat(TokenType.RBRACE, "expected }");
        return s;
    }

    /**
     * checks for valid fields.
     */
    private List<VarDef> fields() {
        List<VarDef> l = new ArrayList();
        if (match(TokenType.ID)) {
            VarDef f = new VarDef();
            f = field();
            l.add(f);
            while (match(TokenType.COMMA)) {
                advance();
                f = field();
                l.add(f);
            }
        }
        return l;
    }

    /**
     * checks for valid field.
     */
    private VarDef field() {
        VarDef f = new VarDef();
        f.varName = currToken;
        eat(TokenType.ID, "expected ID");
        eat(TokenType.COLON, "expected COLON");
        f.dataType = dataType();
        return f;
    }

    /**
     * checks for a valid function definition.
     */
    private FunDef funDef() {
        FunDef f = new FunDef();
        f.returnType = returnType();
        f.funName = currToken;
        eat(TokenType.ID, "expected ID");
        eat(TokenType.LPAREN, "expected (");
        f.params = params();
        eat(TokenType.RPAREN, "expected )");
        f.stmts = block();
        return f;
    }

    /**
     * checks for a valid block.
     */
    private List<Stmt> block() {
        List<Stmt> b = new ArrayList();
        Stmt s;
        eat(TokenType.LBRACE, "expected {");
        while (matchAny(List.of(TokenType.VAR, TokenType.WHILE, TokenType.IF, TokenType.FOR, TokenType.RETURN, TokenType.ID))) {
            s = stmt();
            b.add(s);
        }
        eat(TokenType.RBRACE, "expected }");
        return b;
    }

    /**
     * checks for a valid return type.
     */
    private DataType returnType() {
        DataType d = new DataType();
        if (match(TokenType.VOID_TYPE)) {
            d.type = currToken;
            d.isArray = false;
            advance();
        } else {
            d = dataType();
        }
        return d;
    }

    /**
     * checks for valid params.
     */
    private List<VarDef> params() {
        List<VarDef> p = new ArrayList();
        if (match(TokenType.ID)) {
            VarDef v = new VarDef();
            v = param();
            p.add(v);
            while (match(TokenType.COMMA)) {
                advance();
                v = param();
                p.add(v);
            }
        }
        return p;
    }

    /**
     * checks for a valid param.
     */
    private VarDef param() {
        VarDef p = new VarDef();
        p.varName = currToken;
        eat(TokenType.ID, "expected ID");
        eat(TokenType.COLON, "expected COLON");
        p.dataType = dataType();
        return p;
    }

    /**
     * checks for a valid date type.
     */
    private DataType dataType() {
        DataType d = new DataType();
        if (match(TokenType.ID)) {
            d.type = currToken;
            d.isArray = false;
            advance();
        } else if (match(TokenType.LBRACKET)) {
            d.isArray = true;
            advance();
            if (match(TokenType.ID)) {
                d.type = currToken;
                advance();
            } else {
                SimpleRValue bt = baseType();
                d.type = bt.literal;
            }
            eat(TokenType.RBRACKET, "expected ]");
        } else {
            SimpleRValue srv = baseType();
            d.type = srv.literal;
            d.isArray = false;
        }
        return d;
    }

    /**
     * checks for a valid base type.
     */
    private SimpleRValue baseType() {
        SimpleRValue b = new SimpleRValue();
        if (matchAny(List.of(TokenType.INT_TYPE, TokenType.DOUBLE_TYPE, TokenType.STRING_TYPE, TokenType.BOOL_TYPE))) {
            b.literal = currToken;
            advance();
        } else {
            error("expected INT, DOUBLE, STRING, BOOL");
        }
        return b;
    }

    /**
     * checks for a valid stmt.
     */
    private Stmt stmt() {
        Stmt s;
        if (match(TokenType.VAR)) {
            s = varStmt();
        } else if (match(TokenType.WHILE)) {
            s = whileStmt();
        } else if (match(TokenType.IF)) {
            s = ifStmt();
        } else if (match(TokenType.FOR)) {
            s = forStmt();
        } else if (match(TokenType.RETURN)) {
            s = returnStmt();
        } else {
            AssignStmt a = new AssignStmt();
            CallRValue c = new CallRValue();
            Token name = currToken;
            advance();
            if (match(TokenType.LPAREN)) {
                advance();
                c.args = args();
                c.funName = name;
                eat(TokenType.RPAREN, "expected)");
                s = c;
            } else {
                VarRef v = new VarRef();
                v.varName = name;
                if (match(TokenType.LBRACKET)) {
                    advance();
                    v.arrayExpr = Optional.of(expr());
                    eat(TokenType.RBRACKET, "expected ]");
                }
                a.lvalue.add(v);
                while (match(TokenType.DOT)) {
                    VarRef v2 = new VarRef();
                    advance();
                    v2.varName = currToken;
                    eat(TokenType.ID, "expected ID");
                    if (match(TokenType.LBRACKET)) {
                        advance();
                        v2.arrayExpr = Optional.of(expr());
                        eat(TokenType.RBRACKET, "expected ]");
                    }
                    a.lvalue.add(v2);
                }
                eat(TokenType.ASSIGN, "expected =");
                a.expr = expr();
                s = a;
            }
        }
        return s;
    }

    /**
     * checks for a valid var stmt.
     */
    private VarStmt varStmt() {
        VarStmt v = new VarStmt();
        eat(TokenType.VAR, "expected VAR");
        v.varName = currToken;
        eat(TokenType.ID, "expected ID");
        if (match(TokenType.ASSIGN)) {
            v.expr = Optional.of(varInit());
        } else {
            v.dataType = Optional.of(varType());
            if (match(TokenType.ASSIGN)) {
                v.expr = Optional.of(varInit());
            }
        }
        return v;
    }

    /**
     * checks for a valid var type.
     */
    private DataType varType() {
        DataType v = new DataType();
        eat(TokenType.COLON, "expected COLON");
        v = dataType();
        return v;
    }

    /**
     * checks for a valid var init.
     */
    private Expr varInit() {
        Expr v;
        eat(TokenType.ASSIGN, "expected ASSIGN");
        v = expr();
        return v;
    }

    /**
     * checks for a valid while stmt.
     */
    private WhileStmt whileStmt() {
        WhileStmt w = new WhileStmt();
        eat(TokenType.WHILE, "expected WHILE");
        w.condition = expr();
        w.stmts = block();
        return w;
    }

    /**
     * checks for a valid if stmt.
     */
    private IfStmt ifStmt() {
        IfStmt i = new IfStmt();
        eat(TokenType.IF, "expected IF");
        i.condition = expr();
        i.ifStmts = block();
        if (match(TokenType.ELSE)) {
            advance();
            if (match(TokenType.IF)) {
                i.elseIf = Optional.of(ifStmt());
            } else {
                i.elseStmts = Optional.of(block());
            }
        }
        return i;
    }

    /**
     * checks for a valid for stmt.
     */
    private ForStmt forStmt() {
        ForStmt f = new ForStmt();
        eat(TokenType.FOR, "expected FOR");
        f.varName = currToken;
        eat(TokenType.ID, "expected ID");
        eat(TokenType.FROM, "expected FROM");
        f.fromExpr = expr();
        eat(TokenType.TO, "expected TO");
        f.toExpr = expr();
        f.stmts = block();
        return f;
    }

    /**
     * checks for a valid return stmt.
     */
    private ReturnStmt returnStmt() {
        ReturnStmt r = new ReturnStmt();
        eat(TokenType.RETURN, "expected RETURN");
        r.expr = expr();
        return r;
    }

    /**
     * checks for a valid assign stmt.
     */
    private AssignStmt assignStmt() {
        AssignStmt a = new AssignStmt();
        a.lvalue = lValue();
        eat(TokenType.ASSIGN, "expected ASSIGN");
        a.expr = expr();
        return a;
    }

    /**
     * checks for a valid lvalue.
     */
    private List<VarRef> lValue() {
        List<VarRef> l = new ArrayList();
        VarRef v = new VarRef();
        v.varName = currToken;
        eat(TokenType.ID, "expected ID");
        if (match(TokenType.LBRACKET)) {
            advance();
            v.arrayExpr = Optional.of(expr());
            eat(TokenType.RBRACKET, "expected ]");
        }
        l.add(v);
        while (match(TokenType.DOT)) {
            VarRef v2 = new VarRef();
            advance();
            v2.varName = currToken;
            eat(TokenType.ID, "expected ID");
            if (match(TokenType.LBRACKET)) {
                advance();
                v2.arrayExpr = Optional.of(expr());
                eat(TokenType.RBRACKET, "expected ]");
            }
            l.add(v2);
        }
        return l;
    }

    /**
     * checks for a valid fun call.
     */
    private Object funCall() {
        CallRValue f = new CallRValue();
        f.funName = currToken;
        eat(TokenType.ID, "expected ID");
        eat(TokenType.LPAREN, "expected (");
        f.args = args();
        eat(TokenType.RPAREN, "expected )");
        return f;
    }

    /**
     * checks for valid args.
     */
    private List<Expr> args() {
        List<Expr> a = new ArrayList();
        Expr e;
        if (matchAny(List.of(TokenType.INT_VAL, TokenType.DOUBLE_VAL, TokenType.STRING_VAL, TokenType.BOOL_VAL, TokenType.NULL_VAL, TokenType.NEW, TokenType.ID, TokenType.NOT, TokenType.LPAREN))) {
            e = expr();
            a.add(e);
            while (match(TokenType.COMMA)) {
                advance();
                e = expr();
                a.add(e);
            }
        }
        return a;
    }

    /**
     * checks for a valid expr.
     */
    private Expr expr() {
        Expr e;
        Expr lhs;
        if (match(TokenType.LPAREN)) {
            advance();
            lhs = expr();
            e = lhs;
            eat(TokenType.RPAREN, "expected )");
        } else if (match(TokenType.NOT)) {
            UnaryExpr u = new UnaryExpr();
            u.unaryOp = currToken;
            advance();
            lhs = expr();
            u.expr = lhs;
            e = u;
        } else {
            Expr l;
            BasicExpr b = new BasicExpr();
            b.rvalue = rValue();
            l = b;
            lhs = l;
            e = l;
        }

        if (isBinOp()) {
            BinaryExpr b = new BinaryExpr();
            b.binaryOp = binOp();
            b.rhs = expr();
            b.lhs = lhs;
            e = b;
        }
        return e;
    }

    /**
     * checks for a valid bin op.
     */
    private Token binOp() {
        Token b;
        if (matchAny(List.of(TokenType.PLUS, TokenType.MINUS, TokenType.TIMES, TokenType.DIVIDE, TokenType.AND, TokenType.OR, TokenType.EQUAL, TokenType.LESS, TokenType.GREATER, TokenType.LESS_EQ, TokenType.GREATER_EQ, TokenType.NOT_EQUAL))) {
            b = currToken;
            advance();
        } else {
            error("expected PLUS | MINUS | TIMES | DIVIDE | AND | OR | EQUAL | LESS | GREATER | LESS_EQ | GREATER_EQ | NOT_EQUAL");
            b = new Token(TokenType.EOS, "error here", 0, 0);
        }
        return b;
    }

    /**
     * checks for a valid rvalue.
     */
    private RValue rValue() {
        RValue r;
        if (match(TokenType.NEW)) {
            NewRValue n;
            n = newRValue();
            r = n;
        } else if (matchAny(List.of(TokenType.INT_VAL, TokenType.DOUBLE_VAL, TokenType.STRING_VAL, TokenType.BOOL_VAL, TokenType.NULL_VAL))) {
            SimpleRValue s = new SimpleRValue();
            s = literal();
            r = s;
        } else {
            CallRValue c = new CallRValue();
            VarRValue v = new VarRValue();
            Token name = currToken;
            eat(TokenType.ID, "expecting ID");
            if (match(TokenType.LPAREN)) {
                advance();
                c.args = args();
                c.funName = name;
                eat(TokenType.RPAREN, "expecting )");
                r = c;
            } else {
                v = varRValue();
                v.path.get(0).varName = name;
                r = v;
            }
        }
        return r;
    }

    /**
     * checks for a valid new r value.
     */
    private NewRValue newRValue() {
        NewRValue n;
        eat(TokenType.NEW, "expecting NEW");
        if (match(TokenType.ID)) {
            Token name = currToken;
            advance();
            if (match(TokenType.LPAREN)) {
                NewStructRValue s = new NewStructRValue();
                s.structName = name;
                advance();
                s.args = args();
                eat(TokenType.RPAREN, "expecting )");
                n = s;
            } else {
                NewArrayRValue a = new NewArrayRValue();
                a.type = name;
                eat(TokenType.LBRACKET, "expecting [");
                a.arrayExpr = expr();
                eat(TokenType.RBRACKET, "expecting]");
                n = a;
            }
        } else {
            NewArrayRValue a = new NewArrayRValue();
            SimpleRValue bt = baseType();
            a.type = bt.literal;
            eat(TokenType.LBRACKET, "expecting [");
            a.arrayExpr = expr();
            eat(TokenType.RBRACKET, "expecting]");
            n = a;
        }
        return n;
    }

    /**
     * checks for a valid literal.
     */
    private SimpleRValue literal() {
        SimpleRValue l = new SimpleRValue();
        if (isLiteral()) {
            l.literal = currToken;
            advance();
        } else {
            error("expected INT_VAL | DOUBLE_VAL | STRING_VAL | BOOL_VAL | NULL_VAL");
        }
        return l;
    }

    /**
     * checks for a valid var r value.
     */
    private VarRValue varRValue() {
        VarRValue v = new VarRValue();
        VarRef r = new VarRef();
        if (match(TokenType.LBRACKET)) {
            advance();
            r.arrayExpr = Optional.of(expr());
            eat(TokenType.RBRACKET, "expecting ]");
        }
        v.path.add(r);

        while (match(TokenType.DOT)) {
            VarRef v2 = new VarRef();
            advance();
            v2.varName = currToken;
            eat(TokenType.ID, "expecting ID");
            if (match(TokenType.LBRACKET)) {
                advance();
                v2.arrayExpr = Optional.of(expr());
                eat(TokenType.RBRACKET, "expecting ]");
            }
            v.path.add(v2);
        }
        return v;
    }
}
