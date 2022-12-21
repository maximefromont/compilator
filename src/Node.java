import java.util.ArrayList;
import java.util.HashMap;

public class Node {

    private int type;
    private ArrayList<Node> children;
    private int numLine;
    private int value;
    private String text;
    private int slot;
    private int slotNumber;

    public Node(int type, ArrayList<Node> children, int numLine, int value) {
        this.type = type;
        this.children = children;
        this.numLine = numLine;
        this.value = value;
    }

    public int getType() {
        return type;
    }

    public ArrayList<Node> getChildren() {
        return children;
    }

    public int getNumLine() {
        return numLine;
    }

    public int getValue() {
        return value;
    }

    public void setValue(int value) {
        this.value = value;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public int getSlot() {
        return slot;
    }

    public void setSlot(int slot) {
        this.slot = slot;
    }

    public int getSlotNumber() {
        return slotNumber;
    }

    public void setSlotNumber(int slotNumber) {
        this.slotNumber = slotNumber;
    }

    public static enum NodeTypes {
        PLUS, MINUS, MULTIPLY, DIVIDE, MODULO, POWER,
        LESS_THAN, LESS_THAN_OR_EQUAL, GREATER_THAN, GREATER_THAN_OR_EQUAL, EQUAL, NOT_EQUAL,
        AND, OR, NOT,
        LEFT_PAREN, RIGHT_PAREN, SEMICOLON,
        KEYWORD,
        CONSTANT, EOF,
        MINUS_UNARY, NOT_UNARY, INDIRECT,
        DEBUG, BLOCK, DROP, TEST,
        LEFT_BRACE, RIGHT_BRACE, AFFECTATION,
        REFERENCE, DECLARATION,
        WHILE, FOR, BREAK, CONTINUE,
        CALL, FUNCTION, SEND, RECEIVE, RETURN
    }

    //TODO : Assez ?
    public static HashMap<Integer, String> NodeSymbols = new HashMap<>() {
        {
            put(NodeTypes.PLUS.ordinal(), "+");
            put(NodeTypes.MINUS.ordinal(), "-");
            put(NodeTypes.MULTIPLY.ordinal(), "*");
            put(NodeTypes.DIVIDE.ordinal(), "/");
            put(NodeTypes.MODULO.ordinal(), "%");
            put(NodeTypes.POWER.ordinal(), "^");
            put(NodeTypes.OR.ordinal(), "||");
            put(NodeTypes.AND.ordinal(), "&&");
            put(NodeTypes.LESS_THAN.ordinal(), "<");
            put(NodeTypes.LESS_THAN_OR_EQUAL.ordinal(), "<=");
            put(NodeTypes.GREATER_THAN.ordinal(), ">");
            put(NodeTypes.GREATER_THAN_OR_EQUAL.ordinal(), ">=");
            put(NodeTypes.EQUAL.ordinal(), "==");
            put(NodeTypes.NOT_EQUAL.ordinal(), "!=");
            put(NodeTypes.KEYWORD.ordinal(), "keyword");
            put(NodeTypes.CONSTANT.ordinal(), "constant");
            put(NodeTypes.EOF.ordinal(), "EOF");
            put(NodeTypes.MINUS_UNARY.ordinal(), "-");
            put(NodeTypes.NOT_UNARY.ordinal(), "!");
            put(NodeTypes.DEBUG.ordinal(), "debug");
            put(NodeTypes.BLOCK.ordinal(), "block");
            put(NodeTypes.DROP.ordinal(), "drop");
            put(NodeTypes.TEST.ordinal(), "test");
            put(NodeTypes.REFERENCE.ordinal(), "reference");
            put(NodeTypes.DECLARATION.ordinal(), "declaration");
            put(NodeTypes.AFFECTATION.ordinal(), "affectation");
            put(NodeTypes.WHILE.ordinal(), "while");
            put(NodeTypes.BREAK.ordinal(), "break");
            put(NodeTypes.CONTINUE.ordinal(), "continue");
            put(NodeTypes.FOR.ordinal(), "for");
            put(NodeTypes.CALL.ordinal(), "call");
            put(NodeTypes.FUNCTION.ordinal(), "function");
            put(NodeTypes.INDIRECT.ordinal(), "indirect");
        }
    };

}
