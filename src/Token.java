import java.util.HashMap;

public class Token {

    private int type;
    private String text;
    private int value;
    private int line;

    public Token(int type, String text, int value, int line) {
        this.type = type;
        this.text = text;
        this.value = value;
        this.line = line;
    }

    public int getType() {
        return type;
    }

    public String getText() {
        return text;
    }

    public int getValue() {
        return value;
    }

    public int getLine() {
        return line;
    }

    //TODO Assez ?
    public static enum TokenTypes {
        PLUS, MINUS, MULTIPLY, DIVIDE, MODULO, POWER,
        LESS_THAN, LESS_THAN_OR_EQUAL, GREATER_THAN, GREATER_THAN_OR_EQUAL, EQUAL, NOT_EQUAL,
        AND, OR, NOT,
        LEFT_PAREN, RIGHT_PAREN, SEMICOLON, COMMA,
        KEYWORD,
        CONSTANT, EOF,
        MINUS_UNARY, NOT_UNARY, MULTIPLY_UNARY, DEBUG, BLOCK, DROP,
        LEFT_BRACE, RIGHT_BRACE, LEFT_BRACKET, RIGHT_BRACKET, AFFECTATION,
        IDENTIFIER, INT, SEND, RECEIVE, RETURN
    }

    //TODO : Assez ?
    public static HashMap<Integer, String> TokenSymbols = new HashMap<>() {
        {
            put(TokenTypes.PLUS.ordinal(), "+");
            put(TokenTypes.MINUS.ordinal(), "-");
            put(TokenTypes.MULTIPLY.ordinal(), "*");
            put(TokenTypes.DIVIDE.ordinal(), "/");
            put(TokenTypes.MODULO.ordinal(), "%");
            put(TokenTypes.POWER.ordinal(), "^");
            put(TokenTypes.OR.ordinal(), "||");
            put(TokenTypes.AND.ordinal(), "&&");
            put(TokenTypes.LESS_THAN.ordinal(), "<");
            put(TokenTypes.LESS_THAN_OR_EQUAL.ordinal(), "<=");
            put(TokenTypes.GREATER_THAN.ordinal(), ">");
            put(TokenTypes.GREATER_THAN_OR_EQUAL.ordinal(), ">=");
            put(TokenTypes.EQUAL.ordinal(), "==");
            put(TokenTypes.NOT_EQUAL.ordinal(), "!=");
            put(TokenTypes.LEFT_PAREN.ordinal(), "(");
            put(TokenTypes.RIGHT_PAREN.ordinal(), ")");
            put(TokenTypes.SEMICOLON.ordinal(), ";");
            put(TokenTypes.COMMA.ordinal(), ",");
            put(TokenTypes.KEYWORD.ordinal(), "keyword");
            put(TokenTypes.CONSTANT.ordinal(), "constant");
            put(TokenTypes.EOF.ordinal(), "EOF");
            put(TokenTypes.MINUS_UNARY.ordinal(), "-");
            put(TokenTypes.NOT_UNARY.ordinal(), "!");
            put(TokenTypes.NOT.ordinal(), "!");
            put(TokenTypes.DEBUG.ordinal(), "debug");
            put(TokenTypes.BLOCK.ordinal(), "block");
            put(TokenTypes.DROP.ordinal(), "drop");
            put(TokenTypes.LEFT_BRACE.ordinal(), "{");
            put(TokenTypes.RIGHT_BRACE.ordinal(), "}");
            put(TokenTypes.AFFECTATION.ordinal(), "=");
            put(TokenTypes.INT.ordinal(), "int");
            put(TokenTypes.SEND.ordinal(), "send");
            put(TokenTypes.RECEIVE.ordinal(), "receive");
            put(TokenTypes.RETURN.ordinal(), "return");
        }
    };

}
