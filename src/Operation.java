import java.util.HashMap;

public class Operation {

    private int leftPriority;
    private int rightPriority;
    private int type;

    public Operation(int leftPriority, int rightPriority, int type) {
        this.leftPriority = leftPriority;
        this.rightPriority = rightPriority;
        this.type = type;
    }

    public int getLeftPriority() {
        return leftPriority;
    }

    public int getRightPriority() {
        return rightPriority;
    }

    public int getType() {
        return type;
    }

    public static HashMap<Integer, Operation> operations = new HashMap<Integer, Operation>() {
        {
            put(Token.TokenTypes.AFFECTATION.ordinal(), new Operation(5, 5, Node.NodeTypes.AFFECTATION.ordinal()));
            put(Token.TokenTypes.OR.ordinal(), new Operation(10, 11, Node.NodeTypes.OR.ordinal()));
            put(Token.TokenTypes.AND.ordinal(), new Operation(20, 21, Node.NodeTypes.AND.ordinal()));
            put(Token.TokenTypes.LESS_THAN.ordinal(), new Operation(30, 31, Node.NodeTypes.LESS_THAN.ordinal()));
            put(Token.TokenTypes.LESS_THAN_OR_EQUAL.ordinal(), new Operation(30, 31, Node.NodeTypes.LESS_THAN_OR_EQUAL.ordinal()));
            put(Token.TokenTypes.GREATER_THAN.ordinal(), new Operation(30, 31, Node.NodeTypes.GREATER_THAN.ordinal()));
            put(Token.TokenTypes.GREATER_THAN_OR_EQUAL.ordinal(), new Operation(30, 31, Node.NodeTypes.GREATER_THAN_OR_EQUAL.ordinal()));
            put(Token.TokenTypes.EQUAL.ordinal(), new Operation(30, 31, Node.NodeTypes.EQUAL.ordinal()));
            put(Token.TokenTypes.NOT_EQUAL.ordinal(), new Operation(30, 31, Node.NodeTypes.NOT_EQUAL.ordinal()));
            put(Token.TokenTypes.PLUS.ordinal(), new Operation(40, 41, Node.NodeTypes.PLUS.ordinal()));
            put(Token.TokenTypes.MINUS.ordinal(), new Operation(40, 41, Node.NodeTypes.MINUS.ordinal()));
            put(Token.TokenTypes.MULTIPLY.ordinal(), new Operation(50, 51, Node.NodeTypes.MULTIPLY.ordinal()));
            put(Token.TokenTypes.DIVIDE.ordinal(), new Operation(50, 51, Node.NodeTypes.DIVIDE.ordinal()));
            put(Token.TokenTypes.MODULO.ordinal(), new Operation(50, 51, Node.NodeTypes.MODULO.ordinal()));
            put(Token.TokenTypes.MINUS_UNARY.ordinal(), new Operation(55, 55, Node.NodeTypes.MINUS_UNARY.ordinal()));
            put(Token.TokenTypes.NOT_UNARY.ordinal(), new Operation(55, 55, Node.NodeTypes.NOT_UNARY.ordinal()));
            put(Token.TokenTypes.MULTIPLY_UNARY.ordinal(), new Operation(70, 70, Node.NodeTypes.INDIRECT.ordinal()));
            put(Token.TokenTypes.POWER.ordinal(), new Operation(60, 60, Node.NodeTypes.POWER.ordinal()));
    }};

}
