import java.util.ArrayList;
import java.util.HashMap;

public class Semantic {

    static Symbol identitySymbol = new Symbol("", "", 0, 0);
    static ArrayList<HashMap<String, Symbol>> symbolsList = new ArrayList<>();

    static void blockStart() {
        HashMap<String, Symbol> map = new HashMap<>();
        symbolsList.add(map);
    }

    static void blockEnd() {
        symbolsList.remove(symbolsList.size() - 1);
    }

    //TODO : A tester
    static Symbol declare(String nom) {
        if(symbolsList.size() > 0) {
            HashMap<String, Symbol> map = symbolsList.get(symbolsList.size() - 1);
            if(map.containsKey(nom)) {
                System.out.println("Erreur : la variable " + nom + " est déjà déclarée");
                return identitySymbol;
            }
            else {
                Symbol symbol = new Symbol(nom, "", 0, 0);
                map.put(nom, symbol);
                return symbol;
            }
        }
        else {
            System.out.println("Erreur : la variable " + nom + " est déjà déclarée");
            return identitySymbol;
        }
    }

    //TODO : A tester
    static Symbol acess(String nom) {
        if(symbolsList.size() > 0) {
            HashMap<String, Symbol> map = symbolsList.get(symbolsList.size() - 1);
            if(map.containsKey(nom)) {
                return map.get(nom);
            }
            else {
                System.out.println("Erreur : la variable " + nom + " n'est pas déclarée");
                return identitySymbol;
            }
        }
        else {
            System.out.println("Erreur : la variable " + nom + " n'est pas déclarée");
            return identitySymbol;
        }
    }

    public static void main(String[] args){

        //TODO : Tester dans ce main
        blockStart();
        Symbol s1 = declare("a");
        blockEnd();
        acess("a");
        blockStart();
        acess("a");
        blockStart();
        Symbol s2 = declare("a");
        acess("a");
        blockEnd();
        Symbol s3 = declare("a");
        Symbol s4 = acess("b");
        blockEnd();
    }

}
