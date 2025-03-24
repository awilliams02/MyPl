/**
 * CPSC 326, Spring 2025
 * MyPL Lexer Implementation.
 *
 * Alexa Williams
 */
package cpsc326;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * The Lexer class takes an input stream containing mypl source code and
 * transforms (tokenizes) it into a stream of tokens.
 */
public class Lexer {

    private BufferedReader buffer; // handle to the input stream
    private int line = 1;          // current line number

    private int column = 0;        // current column number

    /**
     * Creates a new Lexer object out of an input stream.
     */
    public Lexer(InputStream input) {
        buffer = new BufferedReader(new InputStreamReader(input));
    }

    /**
     * Helper function to read a single character from the input stream.
     *
     * @return A single character
     */
    private char read() {
        try {
            ++column;
            return (char) buffer.read();
        } catch (IOException e) {
            error("read error", line, column + 1);
        }
        return (char) -1;
    }

    /**
     * Helper function to look ahead one character in the input stream.
     *
     * @return A single character
     */
    private char peek() {
        int ch = -1;
        try {
            buffer.mark(1);
            ch = (char) buffer.read();
            buffer.reset();
            return (char) ch;
        } catch (IOException e) {
            error("read error", line, column + 1);
        }
        return (char) -1;
    }

    /**
     * Helper function to check if the given character is an end of line symbol.
     *
     * @return True if the character is an end of line character and false
     * otherwise.
     */
    private boolean isEOL(char ch) {
        if (ch == '\n') {
            return true;
        }
        if (ch == '\r' && peek() == '\n') {
            read();
            return true;
        } else if (ch == '\r') {
            return true;
        }
        return false;
    }

    /**
     * Helper function to check if the given character is an end of file symbol.
     *
     * @return True if the character is an end of file character and false
     * otherwise.
     */
    private boolean isEOF(char ch) {
        return ch == (char) -1;
    }

    /**
     * Print an error message and exit the program.
     */
    private void error(String msg, int line, int column) {
        String s = "[%d,%d] %s";
        MyPLException.lexerError(String.format(s, line, column, msg));
    }

    /**
     * Obtains and returns the next token in the stream.
     *
     * @return The next token in the stream.
     */
    public Token nextToken() {
        // read the initial character
        char ch = read();
        char p;

        if (isEOF(ch)) {
            return new Token(TokenType.EOS, "end-of-stream", line, column);
        }
        // read past whitespace to start
        while (Character.isWhitespace(ch)) {
            int l = line;
            if (isEOL(ch)) {
                line++;
                column = 0;
            }

            if (isEOF(ch)) {
                return new Token(TokenType.EOS, "end-of-stream", l, column);
            }

            ch = read();
        }

        if (isEOF(ch)) {
            return new Token(TokenType.EOS, "end-of-stream", line, column);
        }

        if (ch == '.') {
            return new Token(TokenType.DOT, ".", line, column);
        } else if (ch == ':') {
            return new Token(TokenType.COLON, ":", line, column);
        } else if (ch == ',') {
            return new Token(TokenType.COMMA, ",", line, column);
        } else if (ch == '(') {
            return new Token(TokenType.LPAREN, "(", line, column);
        } else if (ch == ')') {
            return new Token(TokenType.RPAREN, ")", line, column);
        } else if (ch == '[') {
            return new Token(TokenType.LBRACKET, "[", line, column);
        } else if (ch == ']') {
            return new Token(TokenType.RBRACKET, "]", line, column);
        } else if (ch == '{') {
            return new Token(TokenType.LBRACE, "{", line, column);
        } else if (ch == '}') {
            return new Token(TokenType.RBRACE, "}", line, column);
        } else if (ch == '+') {
            return new Token(TokenType.PLUS, "+", line, column);
        } else if (ch == '-') {
            return new Token(TokenType.MINUS, "-", line, column);
        } else if (ch == '*') {
            return new Token(TokenType.TIMES, "*", line, column);
        } else if (ch == '/') {
            return new Token(TokenType.DIVIDE, "/", line, column);
        } else if (ch == '=') {
            int col = column;
            p = peek();
            if (p == '=') {
                ch = read();
                return new Token(TokenType.EQUAL, "==", line, col);
            } else {
                return new Token(TokenType.ASSIGN, "=", line, column);
            }
        } else if (ch == '<') {
            int col = column;
            p = peek();
            if (p == '=') {
                ch = read();
                return new Token(TokenType.LESS_EQ, "<=", line, col);
            } else {
                return new Token(TokenType.LESS, "<", line, col);
            }
        } else if (ch == '>') {
            int col = column;
            p = peek();
            if (p == '=') {
                ch = read();
                return new Token(TokenType.GREATER_EQ, ">=", line, col);
            } else {
                return new Token(TokenType.GREATER, ">", line, column);
            }
        } else if (ch == '!') {
            int col = column;
            p = peek();
            if (p == '=') {
                ch = read();
                return new Token(TokenType.NOT_EQUAL, "!=", line, col);
            } else {
                error("expecting !=", line, column);
            }
        } else if (ch == '#') {
            int col = column;
            int l = line;
            char c = read();
            char peek;
            String lexeme = "";
            lexeme += c;
            while (!isEOL(c)) {
                c = read();
                peek = peek();
                if (isEOF(peek)) {
                    if (!isEOL(c)) {
                        lexeme += c;
                    }
                    break;
                }
                if (!isEOL(c)) {
                    lexeme += c;
                }
            }
            peek = peek();
            if (!isEOF(peek)) {
                column = 0;
                line++;
            }
            return new Token(TokenType.COMMENT, lexeme, l, col);
        } else if (ch == '"') {
            int col = column;
            char c = read();
            String lexeme = "";
            while (c != '"') {
                lexeme += c;
                c = read();
                if (isEOF(c)) {
                    error("non-terminated string", line, column);
                } else if (isEOL(c)) {
                    error("non-terminated string", line, column);
                }
            }
            return new Token(TokenType.STRING_VAL, lexeme, line, col);
        } else if (Character.isDigit(ch)) {
            char pk = peek();
            if (ch == '0' && Character.isDigit(pk)) {
                error("leading zero in number", line, column);
            }
            int col = column;
            boolean d = false;
            char c = ch;
            String lexeme = "";
            lexeme += ch;
            while (Character.isDigit(c) || c == '.') {
                char peek = peek();
                if (Character.isDigit(peek) || peek == '.') {
                    if (!d) {
                        c = read();
                        if (c == '.') {
                            d = true;
                            lexeme += c;
                            char pe = peek();
                            if (!Character.isDigit(pe)) {
                                error("missing digit after decimal", line, column + 1);
                            }
                        } else {
                            lexeme += c;
                        }
                    } else {
                        if (peek == '.') {
                            break;
                        } else {
                            c = read();
                            if (!Character.isDigit(c)) {
                                error("missing digit after decimal", line, col);
                            } else {
                                lexeme += c;
                            }
                        }
                    }
                } else {
                    break;
                }
            }
            if (d) {
                return new Token(TokenType.DOUBLE_VAL, lexeme, line, col);
            } else {
                return new Token(TokenType.INT_VAL, lexeme, line, col);
            }
        } else if (Character.isLetterOrDigit(ch) || ch == '_') {
            if (ch == '_') {
                error("unrecognized symbol '_'", line, column);
            }
            int col = column;
            char c = ch;
            String lexeme = "";
            lexeme += ch;
            while (Character.isLetterOrDigit(c) || c == '_') {
                char peek = peek();
                if (Character.isLetterOrDigit(peek) || peek == '_') {
                    c = read();
                    lexeme += c;
                } else {
                    break;
                }
            }
            if (lexeme.equals("and")) {
                return new Token(TokenType.AND, lexeme, line, col);
            } else if (lexeme.equals("or")) {
                return new Token(TokenType.OR, lexeme, line, col);
            } else if (lexeme.equals("not")) {
                return new Token(TokenType.NOT, lexeme, line, col);
            } else if (lexeme.equals("struct")) {
                return new Token(TokenType.STRUCT, lexeme, line, col);
            } else if (lexeme.equals("var")) {
                return new Token(TokenType.VAR, lexeme, line, col);
            } else if (lexeme.equals("while")) {
                return new Token(TokenType.WHILE, lexeme, line, col);
            } else if (lexeme.equals("for")) {
                return new Token(TokenType.FOR, lexeme, line, col);
            } else if (lexeme.equals("from")) {
                return new Token(TokenType.FROM, lexeme, line, col);
            } else if (lexeme.equals("to")) {
                return new Token(TokenType.TO, lexeme, line, col);
            } else if (lexeme.equals("if")) {
                return new Token(TokenType.IF, lexeme, line, col);
            } else if (lexeme.equals("else")) {
                return new Token(TokenType.ELSE, lexeme, line, col);
            } else if (lexeme.equals("new")) {
                return new Token(TokenType.NEW, lexeme, line, col);
            } else if (lexeme.equals("return")) {
                return new Token(TokenType.RETURN, lexeme, line, col);
            } else if (lexeme.equals("int")) {
                return new Token(TokenType.INT_TYPE, lexeme, line, col);
            } else if (lexeme.equals("double")) {
                return new Token(TokenType.DOUBLE_TYPE, lexeme, line, col);
            } else if (lexeme.equals("string")) {
                return new Token(TokenType.STRING_TYPE, lexeme, line, col);
            } else if (lexeme.equals("bool")) {
                return new Token(TokenType.BOOL_TYPE, lexeme, line, col);
            } else if (lexeme.equals("void")) {
                return new Token(TokenType.VOID_TYPE, lexeme, line, col);
            } else if (lexeme.equals("true")) {
                return new Token(TokenType.BOOL_VAL, lexeme, line, col);
            } else if (lexeme.equals("false")) {
                return new Token(TokenType.BOOL_VAL, lexeme, line, col);
            } else if (lexeme.equals("null")) {
                return new Token(TokenType.NULL_VAL, lexeme, line, col);
            } else {
                return new Token(TokenType.ID, lexeme, line, col);
            }
        } else {
            int col = column;
            error("unrecognized symbol '" + ch + "'", line, col);
        }
        return null;
    }

}
