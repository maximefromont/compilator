import java.util.HashMap;

public class Assembleur {

    public static HashMap<Integer, String> AssemblySymbols = new HashMap<>() {
        {
            put(Node.NodeTypes.PLUS.ordinal(), "add");
            put(Node.NodeTypes.MINUS.ordinal(), "sub");
            put(Node.NodeTypes.MINUS_UNARY.ordinal(), "neg");
            put(Node.NodeTypes.MULTIPLY.ordinal(), "mul");
            put(Node.NodeTypes.DIVIDE.ordinal(), "div");
            put(Node.NodeTypes.MODULO.ordinal(), "mod");
            put(Node.NodeTypes.EQUAL.ordinal(), "cmpeq");
            put(Node.NodeTypes.GREATER_THAN_OR_EQUAL.ordinal(), "cmpge");
            put(Node.NodeTypes.GREATER_THAN.ordinal(), "cmpgt");
            put(Node.NodeTypes.LESS_THAN_OR_EQUAL.ordinal(), "cmple");
            put(Node.NodeTypes.LESS_THAN.ordinal(), "cmplt");
            put(Node.NodeTypes.NOT_EQUAL.ordinal(), "cmpne");
            put(Node.NodeTypes.AND.ordinal(), "and");
            put(Node.NodeTypes.OR.ordinal(), "or");
        }
    };

}
