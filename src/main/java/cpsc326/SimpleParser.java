/**
 * CPSC 326, Spring 2025
 * The Simple Parser implementation.
 *
 * Alexa Williams
 */
package cpsc326;

import java.util.List;

/**
 * Simple recursive descent parser for checking program syntax.
 */
public class SimpleParser {

    private Lexer lexer;          // the lexer
    private Token currToken;      // the current token

    /**
     * Create a SimpleParser from the give lexer.
     *
     * @param lexer The lexer for the program to parse.
     */
    public SimpleParser(Lexer lexer) {
        this.lexer = lexer;
    }

    /**
     * Run the parser.
     */
    public void parse() {
        advance();
        program();
        eat(TokenType.EOS, "expecting end of file");
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
     * @param targetTokenType The token type to check against.
     */
    private void eat(TokenType targetTokenType, String msg) {
        if (!match(targetTokenType)) {
            error(msg);
        }
        advance();
    }

    /**
     * Helper to check that the current token is a binary operator.
     */
    private boolean isBinOp() {
        return matchAny(List.of(TokenType.PLUS, TokenType.MINUS, TokenType.TIMES,
                TokenType.DIVIDE, TokenType.AND, TokenType.OR,
                TokenType.EQUAL, TokenType.LESS, TokenType.GREATER,
                TokenType.LESS_EQ, TokenType.GREATER_EQ,
                TokenType.NOT_EQUAL));
    }

    /**
     * Helper to check that the current token is a literal value.
     */
    private boolean isLiteral() {
        return matchAny(List.of(TokenType.INT_VAL, TokenType.DOUBLE_VAL,
                TokenType.STRING_VAL, TokenType.BOOL_VAL,
                TokenType.NULL_VAL));
    }

    /**
     * Checks for a valid program.
     */
    private void program() {
        while (matchAny(List.of(TokenType.STRUCT, TokenType.INT_TYPE, TokenType.DOUBLE_TYPE, TokenType.STRING_TYPE, TokenType.BOOL_TYPE, TokenType.ID, TokenType.LBRACKET, TokenType.VOID_TYPE))) {
            if (match(TokenType.STRUCT)) {
                structDef();
            } else {
                funDef();
            }
        }
    }

    /**
     * Checks for a valid struct definition.
     */
    private void structDef() {
        eat(TokenType.STRUCT, "expected STRUCT");
        eat(TokenType.ID, "expected ID");
        eat(TokenType.LBRACE, "expected {");
        fields();
        eat(TokenType.RBRACE, "expected }");
    }

    /**
     * checks for valid fields.
     */
    private void fields() {
        if (match(TokenType.ID)) {
            field();
            while (match(TokenType.COMMA)) {
                advance();
                field();
            }
        }
        //else nothing because or empty set!
    }

    /**
     * checks for valid field.
     */
    private void field() {
        eat(TokenType.ID, "expected ID");
        eat(TokenType.COLON, "expected COLON");
        dataType();
    }

    /**
     * checks for a valid function definition.
     */
    private void funDef() {
        returnType();
        eat(TokenType.ID, "expected ID");
        eat(TokenType.LPAREN, "expected (");
        params();
        eat(TokenType.RPAREN, "expected )");
        block();
    }

    /**
     * checks for a valid block.
     */
    private void block() {
        eat(TokenType.LBRACE, "expected {");
        while (matchAny(List.of(TokenType.VAR, TokenType.WHILE, TokenType.IF, TokenType.FOR, TokenType.RETURN, TokenType.ID))) {
            stmt();
        }
        eat(TokenType.RBRACE, "expected }");
    }

    /**
     * checks for a valid return type.
     */
    private void returnType() {
        if (match(TokenType.VOID_TYPE)) {
            advance();
        } else {
            dataType();
        }
    }

    /**
     * checks for valid params.
     */
    private void params() {
        if (match(TokenType.ID)) {
            param();
            while (match(TokenType.COMMA)) {
                advance();
                param();
            }
        }
    }

    /**
     * checks for a valid param.
     */
    private void param() {
        eat(TokenType.ID, "expected ID");
        eat(TokenType.COLON, "expected COLON");
        dataType();
    }

    /**
     * checks for a valid date type.
     */
    private void dataType() {
        if (match(TokenType.ID)) {
            advance();
        } else if (match(TokenType.LBRACKET)) {
            advance();
            if (match(TokenType.ID)) {
                advance();
            } else {
                baseType();
            }
            eat(TokenType.RBRACKET, "expected ]");
        } else {
            baseType();
        }
    }

    /**
     * checks for a valid base type.
     */
    private void baseType() {
        if (matchAny(List.of(TokenType.INT_TYPE, TokenType.DOUBLE_TYPE, TokenType.STRING_TYPE, TokenType.BOOL_TYPE))) {
            advance();
        } else {
            error("expected INT, DOUBLE, STRING, BOOL");
        }
    }

    /**
     * checks for a valid stmt.
     */
    private void stmt() {
        if (match(TokenType.VAR)) {
            varStmt();
        } else if (match(TokenType.WHILE)) {
            whileStmt();
        } else if (match(TokenType.IF)) {
            ifStmt();
        } else if (match(TokenType.FOR)) {
            forStmt();
        } else if (match(TokenType.RETURN)) {
            returnStmt();
        } else if (match(TokenType.ID)) {
            advance();
            if (match(TokenType.LPAREN)) {
                advance();
                args();
                eat(TokenType.RPAREN, "expected)");
            } else {
                if (match(TokenType.LBRACKET)) {
                    advance();
                    expr();
                    eat(TokenType.RBRACKET, "expected ]");
                }
                while (match(TokenType.DOT)) {
                    advance();
                    eat(TokenType.ID, "expected ID");
                    if (match(TokenType.LBRACKET)) {
                        advance();
                        expr();
                        eat(TokenType.RBRACKET, "expected ]");
                    }
                }
                eat(TokenType.ASSIGN, "expected =");
                expr();
            }
        }
    }

    /**
     * checks for a valid var stmt.
     */
    private void varStmt() {
        eat(TokenType.VAR, "expected VAR");
        eat(TokenType.ID, "expected ID");
        if (match(TokenType.ASSIGN)) {
            varInit();
        } else if (match(TokenType.COLON)) {
            varType();
            if (match(TokenType.ASSIGN)) {
                varInit();
            }
        }
    }

    /**
     * checks for a valid var type.
     */
    private void varType() {
        eat(TokenType.COLON, "expected COLON");
        dataType();
    }

    /**
     * checks for a valid var init.
     */
    private void varInit() {
        eat(TokenType.ASSIGN, "expected ASSIGN");
        expr();
    }

    /**
     * checks for a valid while stmt.
     */
    private void whileStmt() {
        eat(TokenType.WHILE, "expected WHILE");
        expr();
        block();
    }

    /**
     * checks for a valid if stmt.
     */
    private void ifStmt() {
        eat(TokenType.IF, "expected IF");
        expr();
        block();
        if (match(TokenType.ELSE)) {
            advance();
            if (match(TokenType.IF)) {
                ifStmt();
            } else {
                block();
            }
        }
    }

    /**
     * checks for a valid for stmt.
     */
    private void forStmt() {
        eat(TokenType.FOR, "expected FOR");
        eat(TokenType.ID, "expected ID");
        eat(TokenType.FROM, "expected FROM");
        expr();
        eat(TokenType.TO, "expected TO");
        expr();
        block();
    }

    /**
     * checks for a valid return stmt.
     */
    private void returnStmt() {
        eat(TokenType.RETURN, "expected RETURN");
        expr();
    }

    /**
     * checks for a valid assign stmt.
     */
    private void assignStmt() {
        lValue();
        eat(TokenType.ASSIGN, "expected ASSIGN");
        expr();
    }

    /**
     * checks for a valid lvalue.
     */
    private void lValue() {
        eat(TokenType.ID, "expected ID");
        if (match(TokenType.LBRACKET)) {
            advance();
            expr();
            eat(TokenType.RBRACKET, "expected ]");
        }
        while (match(TokenType.DOT)) {
            advance();
            eat(TokenType.ID, "expected ID");
            if (match(TokenType.LBRACKET)) {
                advance();
                expr();
                eat(TokenType.RBRACKET, "expected ]");
            }
        }
    }

    /**
     * checks for a valid fun call.
     */
    private void funCall() {
        eat(TokenType.ID, "expected ID");
        eat(TokenType.LPAREN, "expected (");
        args();
        eat(TokenType.RPAREN, "expected )");
    }

    /**
     * checks for valid args.
     */
    private void args() {
        if (matchAny(List.of(TokenType.INT_VAL, TokenType.DOUBLE_VAL, TokenType.STRING_VAL, TokenType.BOOL_VAL, TokenType.NULL_VAL, TokenType.NEW, TokenType.ID, TokenType.NOT, TokenType.LPAREN))) {
            expr();
            while (match(TokenType.COMMA)) {
                advance();
                expr();
            }
        }
    }

    /**
     * checks for a valid expr.
     */
    private void expr() {
        if (match(TokenType.LPAREN)) {
            advance();
            expr();
            eat(TokenType.RPAREN, "expected )");
        } else if (match(TokenType.NOT)) {
            advance();
            expr();
        } else {
            rValue();
        }

        if (isBinOp()) {
            binOp();
            expr();
        }
    }

    /**
     * checks for a valid bin op.
     */
    private void binOp() {
        if (matchAny(List.of(TokenType.PLUS, TokenType.MINUS, TokenType.TIMES, TokenType.DIVIDE, TokenType.AND, TokenType.OR, TokenType.EQUAL, TokenType.LESS, TokenType.GREATER, TokenType.LESS_EQ, TokenType.GREATER_EQ, TokenType.NOT_EQUAL))) {
            advance();
        } else {
            error("expected PLUS | MINUS | TIMES | DIVIDE | AND | OR | EQUAL | LESS | GREATER | LESS_EQ | GREATER_EQ | NOT_EQUAL");
        }
    }

    /**
     * checks for a valid rvalue.
     */
    private void rValue() {
        if (match(TokenType.NEW)) {
            newRValue();
        } else if (matchAny(List.of(TokenType.INT_VAL, TokenType.DOUBLE_VAL, TokenType.STRING_VAL, TokenType.BOOL_VAL, TokenType.NULL_VAL))) {
            literal();
        } else if (match(TokenType.ID)) {
            advance();
            if (match(TokenType.LPAREN)) {
                advance();
                args();
                eat(TokenType.RPAREN, "expecting )");
            } else {
                varRValue();
            }
        } else {
            error("expecting ID");
        }
    }

    /**
     * checks for a valid new r value.
     */
    private void newRValue() {
        eat(TokenType.NEW, "expecting NEW");
        if (match(TokenType.ID)) {
            advance();
            if (match(TokenType.LPAREN)) {
                advance();
                args();
                eat(TokenType.RPAREN, "expecting )");
            } else {
                eat(TokenType.LBRACKET, "expecting [");
                expr();
                eat(TokenType.RBRACKET, "expecting]");
            }
        } else {
            baseType();
            eat(TokenType.LBRACKET, "expecting [");
            expr();
            eat(TokenType.RBRACKET, "expecting]");
        }
    }

    /**
     * checks for a valid literal.
     */
    private void literal() {
        if (isLiteral()) {
            advance();
        } else {
            error("expected INT_VAL | DOUBLE_VAL | STRING_VAL | BOOL_VAL | NULL_VAL");
        }
    }

    /**
     * checks for a valid var r value.
     */
    private void varRValue() {
        //epecting that ID was already eaten...
        if (match(TokenType.LBRACKET)) {
            advance();
            expr();
            eat(TokenType.RBRACKET, "expecting ]");
        }

        while (match(TokenType.DOT)) {
            advance();
            eat(TokenType.ID, "expecting ID");
            if (match(TokenType.LBRACKET)) {
                advance();
                expr();
                eat(TokenType.RBRACKET, "expecting ]");
            }
        }
    }

}
