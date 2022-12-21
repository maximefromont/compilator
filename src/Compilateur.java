import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Scanner;

public class Compilateur {

    static ArrayList<Token> tokensList;
    static Symbol symobolIdentity = new Symbol("", "", 0, 0);
    static ArrayList<HashMap<String, Symbol>> symbolsList = new ArrayList<HashMap<String, Symbol>>();

    static int currentTokenIndex = 0;
    static int ifIndex = 0;
    static int loopIndex = 0;
    static int slotNumber = 0;

    static String code = "";

    static ArrayList<Integer> loopStack = new ArrayList<Integer>();

    static void error(String message, int line) {
        System.out.println("Error: " + message + " at line " + line);
        System.exit(0);
    }

    //PARTIE LEXICALE
    static void lexicalAnalysis(String text, int line) {
        int indexCharacter = 0;
        while (text.length() > indexCharacter) {
            switch (text.charAt(indexCharacter)) {
                case 0:
                case 1:
                case 2:
                case 3:
                case 4:
                case 5:
                case 6:
                case 7:
                case 8:
                case 9:
                    String number = "";
                    while (text.length() > indexCharacter && Character.isDigit(text.charAt(indexCharacter))) {
                        number += text.charAt(indexCharacter);
                        indexCharacter++;
                    }
                    indexCharacter--;
                    tokensList.add(new Token(Token.TokenTypes.CONSTANT.ordinal(), "", Integer.valueOf(number), line));
                    break;
                case '+':
                    tokensList.add(new Token(Token.TokenTypes.PLUS.ordinal(), "", 0, line));
                    break;
                case '-':
                    tokensList.add(new Token(Token.TokenTypes.MINUS.ordinal(), "", 0, line));
                    break;
                case '/':
                    if (text.length() > indexCharacter + 1 && text.charAt(indexCharacter + 1) == '/') {
                        indexCharacter = text.length() + 1;
                        break;
                    } else {
                        tokensList.add(new Token(Token.TokenTypes.DIVIDE.ordinal(), "", 0, line));
                    }
                    break;
                case '*':
                    tokensList.add(new Token(Token.TokenTypes.MULTIPLY.ordinal(), "", 0, line));
                    break;
                case '%':
                    tokensList.add(new Token(Token.TokenTypes.MODULO.ordinal(), "", 0, line));
                    break;
                case '^':
                    tokensList.add(new Token(Token.TokenTypes.POWER.ordinal(), "", 0, line));
                    break;
                case ';':
                    tokensList.add(new Token(Token.TokenTypes.SEMICOLON.ordinal(), "", 0, line));
                    break;
                case '(':
                    tokensList.add(new Token(Token.TokenTypes.LEFT_PAREN.ordinal(), "", 0, line));
                    break;
                case ')':
                    tokensList.add(new Token(Token.TokenTypes.RIGHT_PAREN.ordinal(), "", 0, line));
                    break;
                case '{':
                    tokensList.add(new Token(Token.TokenTypes.LEFT_BRACE.ordinal(), "", 0, line));
                    break;
                case '}':
                    tokensList.add(new Token(Token.TokenTypes.RIGHT_BRACE.ordinal(), "", 0, line));
                    break;
                case '[':
                    tokensList.add(new Token(Token.TokenTypes.LEFT_BRACKET.ordinal(), "", 0, line));
                    break;
                case ']':
                    tokensList.add(new Token(Token.TokenTypes.RIGHT_BRACKET.ordinal(), "", 0, line));
                    break;
                case '!':
                    if (text.length() > indexCharacter + 1 && text.charAt(indexCharacter + 1) == '=') {
                        indexCharacter++;
                        tokensList.add(new Token(Token.TokenTypes.NOT_EQUAL.ordinal(), "", 0, line));
                    } else {
                        tokensList.add(new Token(Token.TokenTypes.NOT.ordinal(), "", 0, line));
                    }
                    break;
                case '=':
                    if (text.length() > indexCharacter + 1 && text.charAt(indexCharacter + 1) == '=') {
                        indexCharacter++;
                        tokensList.add(new Token(Token.TokenTypes.EQUAL.ordinal(), "", 0, line));
                    } else {
                        tokensList.add(new Token(Token.TokenTypes.AFFECTATION.ordinal(), "", 0, line));
                    }
                    break;
                case '<':
                    if (text.length() > indexCharacter + 1 && text.charAt(indexCharacter + 1) == '=') {
                        indexCharacter++;
                        tokensList.add(new Token(Token.TokenTypes.LESS_THAN_OR_EQUAL.ordinal(), "", 0, line));
                    } else {
                        tokensList.add(new Token(Token.TokenTypes.LESS_THAN.ordinal(), "", 0, line));
                    }
                    break;
                case '>':
                    if (text.length() > indexCharacter + 1 && text.charAt(indexCharacter + 1) == '=') {
                        indexCharacter++;
                        tokensList.add(new Token(Token.TokenTypes.GREATER_THAN_OR_EQUAL.ordinal(), "", 0, line));
                    } else {
                        tokensList.add(new Token(Token.TokenTypes.GREATER_THAN.ordinal(), "", 0, line));
                    }
                    break;
                case ',':
                    tokensList.add(new Token(Token.TokenTypes.COMMA.ordinal(), "", 0, line));
                    break;
                case '&':
                    if (text.length() > indexCharacter + 1 && text.charAt(indexCharacter + 1) == '&') {
                        indexCharacter++;
                        tokensList.add(new Token(Token.TokenTypes.AND.ordinal(), "", 0, line));
                    } else {
                        String word = "";
                        word += text.charAt(indexCharacter);
                        tokensList.add(new Token(Token.TokenTypes.IDENTIFIER.ordinal(), word, 0, line));
                    }
                    break;
                case '|':
                    if (text.length() > indexCharacter + 1 && text.charAt(indexCharacter + 1) == '|') {
                        indexCharacter++;
                        tokensList.add(new Token(Token.TokenTypes.OR.ordinal(), "", 0, line));
                    } else {
                        String word = "";
                        word += text.charAt(indexCharacter);
                        tokensList.add(new Token(Token.TokenTypes.IDENTIFIER.ordinal(), word, 0, line));
                    }
                    break;
                default:
                    if (Character.isLetter(text.charAt(indexCharacter))) {
                        String word = "";
                        word += text.charAt(indexCharacter);
                        while (text.length() > indexCharacter && (Character.isLetter(text.charAt(indexCharacter + 1)) || text.charAt(indexCharacter + 1) == '_')) {
                            word += text.charAt(indexCharacter + 1);
                            indexCharacter++;
                        } //TODO : Utiliser les constantes
                        if (word.equals("if") || word.equals("while") || word.equals("for") || word.equals("else") || word.equals("break") || word.equals("continue")) {
                            tokensList.add(new Token(Token.TokenTypes.KEYWORD.ordinal(), word, 0, line));
                        } else if(word.equals("debug")){
                            tokensList.add(new Token(Token.TokenTypes.DEBUG.ordinal(), "", 0, line));
                        } else if(word.equals("int")) {
                            tokensList.add(new Token(Token.TokenTypes.INT.ordinal(), "", 0, line));
                        } else if(word.equals("return")) {
                            tokensList.add(new Token(Token.TokenTypes.RETURN.ordinal(), "", 0, line));
                        } else if(word.equals("and")) {
                            tokensList.add(new Token(Token.TokenTypes.AND.ordinal(), "", 0, line));
                        } else if(word.equals("or")) {
                            tokensList.add(new Token(Token.TokenTypes.OR.ordinal(), "", 0, line));
                        } else if(word.equals("send")) {
                            tokensList.add(new Token(Token.TokenTypes.SEND.ordinal(), "", 0, line));
                        } else if(word.equals("receive")) {
                            tokensList.add(new Token(Token.TokenTypes.RECEIVE.ordinal(), "", 0, line));
                        } else {
                            tokensList.add(new Token(Token.TokenTypes.IDENTIFIER.ordinal(), word, 0, line));
                        }
                    }
                    break;
            }
            indexCharacter++;
        }
    }

    static Token currentToken() {
        return tokensList.get(currentTokenIndex);
    }

    static Token nextToken() {
        return tokensList.get(currentTokenIndex + 1);
    }

    static void incrementToken() {
        currentTokenIndex++;
    }

    static void accept(int type) {
        if (currentToken().getType() == type) {
            incrementToken();
        } else { //TODO : Performance = ENFER
            String symbolUnexpected = "";
            for (int i = 0; i < Token.TokenTypes.values().length; i++) {
                if (Token.TokenTypes.values()[i].ordinal() == currentToken().getType()) {
                    symbolUnexpected = Token.TokenTypes.values()[i].name();
                }
            }
            String symbolExpected = "";
            for (int i = 0; i < Token.TokenTypes.values().length; i++) {
                if (Token.TokenTypes.values()[i].ordinal() == type) {
                    symbolExpected = Token.TokenTypes.values()[i].name();
                }
            }
            error("Error : " + symbolUnexpected + " is not expected. Expected : " + symbolExpected, currentToken().getLine());
        }
    }

    static boolean check(int type) {
        if (currentToken().getType() == type) {
            incrementToken();
            return true;
        } else {
            return false;
        }
    }

    //PARTIE SYNTHAXIQUE
    static Node newNode(int type, int line) {
        Node node = new Node(type, new ArrayList<Node>(), line, 0);
        return node;
    }

    static void addChild(Node parent, Node child) {
        parent.getChildren().add(child);
    }

    static Node expression(int minimumPriority) {
        Node N = S();
        while (currentToken().getType() != Token.TokenTypes.EOF.ordinal() && Operation.operations.containsKey(currentToken().getType()) && Operation.operations.get(currentToken().getType()).getLeftPriority() >= minimumPriority) {
            Operation operation = Operation.operations.get(currentToken().getType());
            int line = currentToken().getLine();
            incrementToken();
            Node A = expression(operation.getRightPriority());
            Node T = newNode(operation.getType(), line);
            addChild(T, N);
            addChild(T, A);
            N = T;
        }
        return N;
    }

    static Node atom() {
        if(check(Token.TokenTypes.LEFT_PAREN.ordinal())) {
            Node N = expression(0);
            accept(Token.TokenTypes.RIGHT_PAREN.ordinal());
            return N;
        } else if(check(Token.TokenTypes.MINUS.ordinal())) {
            Node N = newNode(Node.NodeTypes.MINUS_UNARY.ordinal(), currentToken().getLine());
            Node arg = expression(Operation.operations.get(Token.TokenTypes.MINUS_UNARY.ordinal()).getLeftPriority());
            addChild(N, arg);
            return N;
        } else if(check(Token.TokenTypes.NOT.ordinal())) {
            Node N = newNode(Node.NodeTypes.NOT_UNARY.ordinal(), currentToken().getLine());
            Node arg = expression(Operation.operations.get(Token.TokenTypes.NOT_UNARY.ordinal()).getLeftPriority());
            addChild(N, arg);
            return N;
        } else if(check(Token.TokenTypes.MULTIPLY.ordinal())) {
            Node N = newNode(Node.NodeTypes.INDIRECT.ordinal(), currentToken().getLine());
            Node arg = expression(Operation.operations.get(Token.TokenTypes.MINUS_UNARY.ordinal()).getLeftPriority());
            N.setText(String.valueOf(currentToken().getValue()));
            addChild(N, arg);
            return N;
        } else if(currentToken().getType() == Token.TokenTypes.CONSTANT.ordinal()) {
            Node N = newNode(Node.NodeTypes.CONSTANT.ordinal(), currentToken().getLine());
            N.setText(String.valueOf(currentToken().getValue()));
            incrementToken();
            return N;
        } else if(currentToken().getType() == Token.TokenTypes.RECEIVE.ordinal()) {
            Node N = newNode(Node.NodeTypes.RECEIVE.ordinal(), currentToken().getLine());
            incrementToken();
            return N;
        } else if(currentToken().getType() == Token.TokenTypes.IDENTIFIER.ordinal()) {
            if(nextToken().getType() == Token.TokenTypes.LEFT_PAREN.ordinal()) {
                Token T = currentToken();
                Node N = newNode(Node.NodeTypes.CALL.ordinal(), currentToken().getLine());
                incrementToken();
                incrementToken();
                N.setText(currentToken().getText());
                while(currentToken().getType() != Token.TokenTypes.RIGHT_PAREN.ordinal()) {
                    addChild(N, expression(0));
                    if(!check(Token.TokenTypes.COMMA.ordinal())) {
                        break;
                    }
                }
                accept(Token.TokenTypes.RIGHT_PAREN.ordinal());
                return N;
            } else {
                Node N = newNode(Node.NodeTypes.REFERENCE.ordinal(), currentToken().getLine());
                N.setText(currentToken().getText());
                incrementToken();
                return N;
            }
        } else {
            //Find current token symbol
            String symbol = ""; //TODO : PERFORMANCE = ENFER
            for (int i = 0; i < Token.TokenTypes.values().length; i++) {
                if (Token.TokenTypes.values()[i].ordinal() == currentToken().getType()) {
                    symbol = Token.TokenTypes.values()[i].name();
                }
            }
            error("Error : Token " + symbol + " is not an atom.", currentToken().getLine());
            return null;
        }
    }

    static Node instruction() {
        if (check(Token.TokenTypes.DEBUG.ordinal())) {
            Node E1 = expression(0);
            accept(Token.TokenTypes.SEMICOLON.ordinal());
            Node N = newNode(Node.NodeTypes.DEBUG.ordinal(), currentToken().getLine());
            addChild(N, E1);
            return N;
        } else if(check(Token.TokenTypes.SEND.ordinal())) {
            Node E1 = expression(0);
            accept(Token.TokenTypes.SEMICOLON.ordinal());
            Node N = newNode(Node.NodeTypes.SEND.ordinal(), currentToken().getLine());
            addChild(N, E1);
            return N;
        } else if(check(Token.TokenTypes.RECEIVE.ordinal())) {
            Node N = newNode(Node.NodeTypes.RECEIVE.ordinal(), currentToken().getLine());
            return N;
        } else if(check(Token.TokenTypes.LEFT_BRACE.ordinal())) {
            Node N = newNode(Node.NodeTypes.BLOCK.ordinal(), currentToken().getLine());
            while(!check(Token.TokenTypes.RIGHT_BRACE.ordinal())) {
                addChild(N, instruction());
            }
            return N;
        } else if(check(Token.TokenTypes.RETURN.ordinal())) {
            Node N = newNode(Node.NodeTypes.RETURN.ordinal(), currentToken().getLine());
            addChild(N, expression(0));
            accept(Token.TokenTypes.SEMICOLON.ordinal());
            return N;
        } else if(currentToken().getText().equals("if") && check(Token.TokenTypes.KEYWORD.ordinal())) {
            accept(Token.TokenTypes.LEFT_PAREN.ordinal());
            Node E1 = expression(0);
            accept(Token.TokenTypes.RIGHT_PAREN.ordinal());
            Node I1 = instruction();
            Node N = newNode(Node.NodeTypes.TEST.ordinal(), currentToken().getLine());
            addChild(N, E1);
            addChild(N, I1);
            if(currentToken().getText().equals("else") && check(Token.TokenTypes.KEYWORD.ordinal())) {
                Node I2 = instruction();
                addChild(N, I2);
            }
            return N;
        } else if(check(Token.TokenTypes.INT.ordinal())) {
            Node N = newNode(Node.NodeTypes.DECLARATION.ordinal(), currentToken().getLine());
            N.setText(currentToken().getText());
            incrementToken();
            if(check(Token.TokenTypes.AFFECTATION.ordinal())) {
                addChild(N, expression(0));
            }
            accept(Token.TokenTypes.SEMICOLON.ordinal());
            return N;
        } else if(currentToken().getText().equals("while") && check(Token.TokenTypes.KEYWORD.ordinal())) {
            accept(Token.TokenTypes.LEFT_PAREN.ordinal());
            Node N = newNode(Node.NodeTypes.WHILE.ordinal(), currentToken().getLine());
            Node condition = newNode(Node.NodeTypes.TEST.ordinal(), currentToken().getLine());
            Node E = expression(0);
            accept(Token.TokenTypes.RIGHT_PAREN.ordinal());
            addChild(condition, E);

            Node I = instruction();
            addChild(condition, I);

            Node breakNode = newNode(Node.NodeTypes.BREAK.ordinal(), currentToken().getLine());
            addChild(condition, breakNode);

            addChild(N, condition);
            return N;
        } else if(currentToken().getText().equals("for") && check(Token.TokenTypes.KEYWORD.ordinal())) {
            accept(Token.TokenTypes.LEFT_PAREN.ordinal());
            Node B1 = newNode(Node.NodeTypes.BLOCK.ordinal(), currentToken().getLine());
            Node D1 = newNode(Node.NodeTypes.DROP.ordinal(), currentToken().getLine());

            Node loop = newNode(Node.NodeTypes.FOR.ordinal(), currentToken().getLine());
            Node E1 = instruction();

            Node E2 = expression(0);
            accept(Token.TokenTypes.SEMICOLON.ordinal());
            Node E3 = expression(0);
            accept(Token.TokenTypes.RIGHT_PAREN.ordinal());
            Node I = instruction();

            addChild(D1, E1);
            addChild(B1, D1);

            Node condition = newNode(Node.NodeTypes.TEST.ordinal(), E2.getNumLine());
            addChild(condition, E2);

            Node B2 = newNode(Node.NodeTypes.BLOCK.ordinal(), I.getNumLine());
            addChild(B2, I);
            Node D2 = newNode(Node.NodeTypes.DROP.ordinal(), E3.getNumLine());
            addChild(D2, E3);
            addChild(B2, D2);

            addChild(condition, B2);
            Node breakNode = newNode(Node.NodeTypes.BREAK.ordinal(), E2.getNumLine());
            addChild(condition, breakNode);
            addChild(loop, condition);

            addChild(B1, loop);
            return B1;
        } else if(currentToken().getText().equals("break") && check(Token.TokenTypes.KEYWORD.ordinal())) {
            Node N = newNode(Node.NodeTypes.BREAK.ordinal(), currentToken().getLine());
            accept(Token.TokenTypes.SEMICOLON.ordinal());
            return N;
        } else if(currentToken().getText().equals("continue") && check(Token.TokenTypes.KEYWORD.ordinal())) {
            Node N = newNode(Node.NodeTypes.CONTINUE.ordinal(), currentToken().getLine());
            accept(Token.TokenTypes.SEMICOLON.ordinal());
            return N;
        } else {
            Node E1 = expression(0);
            accept(Token.TokenTypes.SEMICOLON.ordinal());
            Node N = newNode(Node.NodeTypes.DROP.ordinal(), currentToken().getLine());
            addChild(N, E1);
            return N;
        }
    }

    static Node function() {
        accept(Token.TokenTypes.INT.ordinal());
        Token token = currentToken();
        accept(Token.TokenTypes.IDENTIFIER.ordinal());

        Node N = newNode(Node.NodeTypes.FUNCTION.ordinal(), token.getLine());
        N.setText(token.getText());
        accept(Token.TokenTypes.LEFT_PAREN.ordinal());
        while(currentToken().getType() != Token.TokenTypes.RIGHT_PAREN.ordinal()) {
            accept(Token.TokenTypes.INT.ordinal());
            String value = currentToken().getText();
            Node E = newNode(Node.NodeTypes.DECLARATION.ordinal(), currentToken().getLine());
            E.setText(value);
            accept(Token.TokenTypes.IDENTIFIER.ordinal());
            addChild(N, E);
            if(!check(Token.TokenTypes.COMMA.ordinal())) {
                break;
            }
        }
        accept(Token.TokenTypes.RIGHT_PAREN.ordinal());

        addChild(N, instruction());

        return N;
    }

    static Node S() {
        Node N = atom();
        if(check(Token.TokenTypes.LEFT_BRACKET.ordinal())) {
            Node E = expression(0);
            accept(Token.TokenTypes.RIGHT_BRACKET.ordinal());
            Node ind = newNode(Node.NodeTypes.INDIRECT.ordinal(), currentToken().getLine());
            Node add = newNode(Node.NodeTypes.PLUS.ordinal(), currentToken().getLine());
            addChild(add, N);
            addChild(add, E);
            addChild(ind, add);
            return ind;
        }
        return N;
    }

    static void generateCode(Node N) {
        final Node.NodeTypes[] vals = Node.NodeTypes.values();
        switch(vals[N.getType()]) {
            case CONSTANT:
                code += "push " + String.valueOf(N.getValue()) + "\n";
                break;
            case PLUS:
            case MINUS:
            case MULTIPLY:
            case MODULO:
            case DIVIDE:
            case EQUAL:
            case LESS_THAN:
            case LESS_THAN_OR_EQUAL:
            case GREATER_THAN:
            case GREATER_THAN_OR_EQUAL:
            case AND:
            case OR:
            case NOT_EQUAL:
                generateCode(N.getChildren().get(0));
                generateCode(N.getChildren().get(1));
                code += Assembleur.AssemblySymbols.get(vals[N.getType()]) + "\n";
                break;
            case MINUS_UNARY:
                code += "push -" + String.valueOf(N.getChildren().get(0).getValue()) + "\n";
                break;
            case DEBUG:
                generateCode(N.getChildren().get(0));
                code += "dbg \n"; //TODO : debug ou dbg ?
                break;
            case BLOCK:
                for(Node child : N.getChildren()) {
                    generateCode(child);
                }
                break;
            case DROP:
                generateCode(N.getChildren().get(0));
                code += "drop \n";
                break;
            case NOT_UNARY:
                generateCode(N.getChildren().get(0));
                code += "not \n";
                break;
            case TEST:
                int localIfIndex = ifIndex;
                ifIndex++;
                generateCode(N.getChildren().get(0));
                code += "jump if" + String.valueOf(localIfIndex) + "\n";
                generateCode(N.getChildren().get(1));
                if(N.getChildren().size() <= 2) {
                    code += ".if" + String.valueOf(localIfIndex) + ":\n";
                } else {
                    int localIfIndex2 = ifIndex;
                    ifIndex++;
                    code += "jump if" + String.valueOf(localIfIndex2) + "\n";
                    code += ".if" + String.valueOf(localIfIndex) + ":\n";
                    generateCode(N.getChildren().get(2));
                    code += ".if" + String.valueOf(localIfIndex2) + ":\n";
                }
                ifIndex++;
                break;
            case SEND:
                generateCode(N.getChildren().get(0));
                code += "send \n";
                break;
            case RECEIVE:
                code += "recv \n";
                break;
            case REFERENCE:
                code += "get " + N.getSlot() + "\n";
                break;
            case DECLARATION:
                if(N.getChildren().size() > 0) {
                    Node node = newNode(Node.NodeTypes.AFFECTATION.ordinal(), N.getNumLine());
                    Node E1 = newNode(Node.NodeTypes.REFERENCE.ordinal(), N.getNumLine());
                    E1.setSlot(N.getSlot());
                    addChild(node, E1);
                    addChild(node, N.getChildren().get(0));
                    generateCode(node);
                }
                break;
            case AFFECTATION:
                if(N.getChildren().get(0).getType() == Node.NodeTypes.REFERENCE.ordinal()) {
                    generateCode(N.getChildren().get(1));
                    code += "dup \n";
                    code += "set " + N.getChildren().get(0).getSlot() + "\n";
                } else {
                    generateCode(N.getChildren().get(0).getChildren().get(0));
                    generateCode(N.getChildren().get(1));
                    code += "write \n";
                    code += "push 0 \n";
                }
                break;
            case WHILE:
            case FOR:
                loopStack.add(loopIndex);
                loopIndex++;
                code += ".loop" + String.valueOf(loopIndex) + ":\n"; //Last element inserted is loopIndex
                generateCode(N.getChildren().get(0));
                code += "jump loop" + String.valueOf(loopIndex) + "\n"; //Last element inserted is loopIndex
                code += ".endloop" + String.valueOf(loopIndex) + ":\n"; //Last element inserted is loopIndex
                loopStack.remove(loopStack.size() - 1);
                break;
            case BREAK:
                if(loopStack.size() > 0) {
                    code += "jump endloop" + String.valueOf(loopStack.get(loopStack.size() - 1)) + "\n";
                } else {
                    error("Break outside of loop", N.getNumLine());
                }
                break;
            case CONTINUE:
                if(loopStack.size() > 0) {
                    code += "jump loop" + String.valueOf(loopStack.get(loopStack.size() - 1)) + "\n";
                } else {
                    error("Continue outside of loop", N.getNumLine());
                }
                break;
            case CALL:
                code += "prep " + N.getText() + "\n";
                for(Node child : N.getChildren()) {
                    generateCode(child);
                }
                code += "call " + N.getChildren().size() + "\n";
                break;
            case RETURN:
                generateCode(N.getChildren().get(0));
                code += "ret \n";
                break;
            case FUNCTION:
                code += "." + N.getText() + ":\n";
                code += "resn " + String.valueOf(N.getSlotNumber()-N.getChildren().size()+1) + "\n";
                generateCode(N.getChildren().get(N.getChildren().size()-1));
                code += "push 0 \n";
                code += "ret \n";
                break;
            case POWER:
                code += "prep puissance \n";
                generateCode(N.getChildren().get(0));
                generateCode(N.getChildren().get(1));
                code += "call 2 \n";
                break;
            case INDIRECT:
                generateCode(N.getChildren().get(0));
                code += "read \n";
                break;
            default:
                break;
        }
    }

    static void blockStart() {
        HashMap<String, Symbol> map = new HashMap<String, Symbol>();
        symbolsList.add(map);
    }

    static void blockEnd() {
        symbolsList.remove(symbolsList.size()-1);
    }

    //TODO : A tester
    static Symbol declare(String nom) {
        if(symbolsList.size() > 0) {
            HashMap<String, Symbol> map = symbolsList.get(symbolsList.size() - 1);
            if(map.containsKey(nom)) {
                System.out.println("Erreur : la variable " + nom + " est déjà déclarée");
                System.exit(0);
                return symobolIdentity;
            }
            else {
                Symbol symbol = new Symbol(nom, "", 0, 0);
                map.put(nom, symbol);
                return symbol;
            }
        }
        else {
            System.out.println("Erreur : la variable " + nom + " est déjà déclarée");
            System.exit(0);
            return symobolIdentity;
        }
    }

    //TODO : A tester
    static Symbol acess(String nom, int line) {
        if(symbolsList.size() > 0) {
            HashMap<String, Symbol> map = symbolsList.get(symbolsList.size() - 1);
            if(map.containsKey(nom)) {
                return map.get(nom);
            }
            else {
                error("Erreur : la variable " + nom + " n'est pas déclarée", line);
                return symobolIdentity;
            }
        }
        else {
            error("Erreur : la variable " + nom + " n'est pas déclarée", line);
            return symobolIdentity;
        }
    }

    static void sem(Node N, boolean affected) {
        final Node.NodeTypes[] vals = Node.NodeTypes.values();
        switch(vals[N.getType()]) {
            case FUNCTION:
                slotNumber = 0;
                Symbol S = declare(N.getText());
                S.setType("function");
                blockStart();
                for(Node child : N.getChildren()) {
                    sem(child, true);
                    S.incrementNumberArgs();
                }
                S.decrementNumberArgs();
                blockEnd();
                N.setSlotNumber(slotNumber);
                break;
            case CALL:
                S = acess(N.getText(), N.getNumLine());
                if(S.getType().equals("function")) {
                    error("Erreur : la variable " + N.getText() + " n'est pas une fonction", N.getNumLine());
                }
                for (Node child : N.getChildren()) {
                    sem(child, false);
                }
                if(S.getNumberArgs() < N.getChildren().size()) {
                    error("Erreur : la fonction " + N.getText() + " a besoin de " + S.getNumberArgs() + " arguments", N.getNumLine());
                } else if(S.getNumberArgs() > N.getChildren().size()) {
                    error("Erreur : la fonction " + N.getText() + " a besoin de " + S.getNumberArgs() + " arguments", N.getNumLine());
                }
                break;
            case BLOCK:
                blockStart();
                for(Node child : N.getChildren()) {
                    sem(child, false);
                }
                blockEnd();
                break;
            case DECLARATION:
                S = declare(N.getText());
                S.setType("variable");
                S.setSlot(slotNumber);
                N.setSlot(S.getSlot());
                if(N.getChildren().size() > 0 || affected) {
                    S.setAffected(1);
                }
                slotNumber++;
                break;
            case REFERENCE:
                S = acess(N.getText(), N.getNumLine());
                if(!S.getType().equals("variable") || S.getAffected() == 0) {
                    error("Erreur : la variable " + N.getText() + " n'est pas déclarée", N.getNumLine());
                }
                N.setSlot(S.getSlot());
                break;
            case AFFECTATION:
                if(N.getChildren().get(0).getType() != Node.NodeTypes.INDIRECT.ordinal()) {
                    Node node = N.getChildren().get(0);
                    S = acess(node.getText(), node.getNumLine());
                    S.setAffected(1);
                }
                //break; //TODO : Garder le break ou non ?
            default:
                for(Node child : N.getChildren()) {
                    sem(child, false);
                }
                break;
        }
    }

    static void Optimisation(Node N) {
        final Node.NodeTypes[] vals = Node.NodeTypes.values();
        switch(vals[N.getType()]) {
            case PLUS:
            case MINUS:
            case MULTIPLY:
            case MODULO:
            case DIVIDE:
            case MINUS_UNARY:
                for(Node child : N.getChildren()) {
                    Optimisation(child);
                }
                if(N.getChildren().get(0).getType() == Node.NodeTypes.CONSTANT.ordinal() && N.getType() == Node.NodeTypes.MINUS_UNARY.ordinal()) {
                    Node optimizedNode = newNode(Node.NodeTypes.CONSTANT.ordinal(), N.getNumLine());
                    optimizedNode.setValue(-1 * N.getChildren().get(0).getValue());
                    N = optimizedNode;
                } else if(N.getChildren().get(0).getType() == Node.NodeTypes.CONSTANT.ordinal() && N.getChildren().get(1).getType() == Node.NodeTypes.CONSTANT.ordinal()) {
                    Node optimizedNode = newNode(Node.NodeTypes.CONSTANT.ordinal(), N.getNumLine());
                    switch(vals[N.getType()]) {
                        case PLUS:
                            optimizedNode.setValue(N.getChildren().get(0).getValue() + N.getChildren().get(1).getValue());
                            break;
                        case MINUS:
                            optimizedNode.setValue(N.getChildren().get(0).getValue() - N.getChildren().get(1).getValue());
                            break;
                        case MULTIPLY:
                            optimizedNode.setValue(N.getChildren().get(0).getValue() * N.getChildren().get(1).getValue());
                            break;
                        case DIVIDE:
                            optimizedNode.setValue(N.getChildren().get(0).getValue() / N.getChildren().get(1).getValue());
                            break;
                        case MODULO:
                            optimizedNode.setValue(N.getChildren().get(0).getValue() % N.getChildren().get(1).getValue());
                            break;
                        default:
                            break;
                    }
                    N = optimizedNode;
                }
                break;
            default:
                for(Node child : N.getChildren()) {
                    Optimisation(child);
                }
                break;
        }
    }

    public static void main(String[] args){

        if(args.length < 2) {
            System.out.println("Usage : java -jar compiler.jar <input file> <output file>");
            System.exit(1);
        }

        boolean debugMode = false;
        boolean showCode = false;
        for(int i = 2; i < args.length; i++) {
            if(args[i].equals("-d") || args[i].equals("--debug")) {
                debugMode = true;
            } else if(args[i].equals("-s") || args[i].equals("--show")) {
                showCode = true;
            } else if(args[i].equals("-u") || args.equals("--usage")) {
                System.out.println("Usage : java -jar compiler.jar <input file> <output file> [options]");
                System.out.println("Options :");
                System.out.println("\t-d, --debug\t\tEnable debug mode");
                System.out.println("\t-s, --show\t\tShow the code");
                System.out.println("\t-u, --usage\t\tShow this message");
                System.exit(0);
            } else {
                System.out.println("Usage : java -jar compiler.jar <input file> <output file> [options]");
                System.out.println("Options :");
                System.out.println("\t-d, --debug\t\tEnable debug mode");
                System.out.println("\t-s, --show\t\tShow the code");
                System.out.println("\t-u, --usage\t\tShow this message");
                System.exit(1);
            }
        }

        try {
            File file = new File(args[0]);

            int lineIndex = 1;

            try {
                File runtimeFile = new File("Runtime");
                Scanner runtimeSc = new Scanner(runtimeFile);
                String runtimeLine = "";
                while(runtimeSc.hasNextLine()) {
                    runtimeLine = runtimeSc.nextLine();
                    lexicalAnalysis(runtimeLine, lineIndex);
                    lineIndex++;
                }
                runtimeSc.close();
            } catch (FileNotFoundException e) {
                System.out.println("Erreur : le fichier Runtime n'a pas été trouvé");
                System.exit(1);
            }

            lineIndex = 1;
            Scanner sc = new Scanner(file);
            String line = "";

            while(sc.hasNextLine()) {
                line = sc.nextLine();
                lexicalAnalysis(line, lineIndex);
                lineIndex++;
            }
            sc.close();

            tokensList.add(new Token(Token.TokenTypes.EOF.ordinal(), "", 0, 0));

            if(debugMode) {
                for(Token token : tokensList) {
                    System.out.println("Token : " + token);
                }
            }


        } catch (FileNotFoundException e) {
            System.out.println("Erreur : le fichier " + args[0] + " n'existe pas");
            System.exit(1);
        }

        try {
            //Imprime la sortie standard dans un fichier out.txt
            PrintStream out = new PrintStream(new FileOutputStream(args[1]));
            System.setOut(out);
        } catch (FileNotFoundException e) {
            System.out.println("Erreur : le fichier " + args[1] + " n'existe pas");
            System.exit(1);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        blockStart();
        while(currentToken().getType() != Token.TokenTypes.EOF.ordinal()) {
            Node N = function();
            sem(N, false);
            Optimisation(N);
            if (debugMode) {
                System.out.println("AST : " + N);
            }
            generateCode(N);
        }
        blockEnd();

        code += ".start:\n";
        code += "prep main \n";
        code += "call 0 \n";
        code += "halt \n";

        if(showCode) {
            System.out.println(code);
        }

        //remettre la sortie standard dans la console
        System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out)));
    }

}