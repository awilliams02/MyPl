/**
 * CPSC 326, Spring 2025
 * The available mypl token types. 
 */

package cpsc326;


public enum TokenType {
  // punctuation symbols -- CHECKED !
  DOT, COLON, COMMA, LPAREN, RPAREN, LBRACKET, RBRACKET, LBRACE, RBRACE, 
  // arithmetic operators -- CHECKED !
  PLUS, MINUS, TIMES, DIVIDE, 
  // assignment and comparator operators -- CHECKED !
  ASSIGN, EQUAL, NOT_EQUAL, LESS, LESS_EQ, GREATER, GREATER_EQ, 
  // primitive values and identifiers -- CHECKED !
  STRING_VAL, INT_VAL, DOUBLE_VAL, BOOL_VAL, NULL_VAL, ID, 
  // boolean operators -- CHECKED !
  AND, OR, NOT,
  // data types -- CHECKED !
  INT_TYPE, DOUBLE_TYPE, STRING_TYPE, BOOL_TYPE, VOID_TYPE,
  // reserved words -- CHECKED !
  STRUCT, VAR, WHILE, FOR, FROM, TO, IF, ELSE, NEW, RETURN, 
  // comment token and end-of-stream -- CHECKED !
  COMMENT, EOS
}
