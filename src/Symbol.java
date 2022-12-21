public class Symbol {

    private String id;
    private String type;
    private int slot;
    private int affected;
    private int numberArgs;


    public Symbol(String id, String type, int slot, int affected) {
        this.id = id;
        this.type = type;
        this.slot = slot;
        this.affected = affected;
        numberArgs = 0;
    }

    public String getId() {
        return id;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public int getSlot() {
        return slot;
    }

    public void setSlot(int slot) {
        this.slot = slot;
    }

    public int getAffected() {
        return affected;
    }

    public void setAffected(int affected) {
        this.affected = affected;
    }

    public int getNumberArgs() {
        return numberArgs;
    }

    public void incrementNumberArgs() {
        numberArgs++;
    }

    public void decrementNumberArgs() {
        numberArgs--;
    }
}
