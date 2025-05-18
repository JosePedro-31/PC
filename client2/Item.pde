public class Item{

    private PVector position = new PVector(0, 0); // posição do item
    private float radius = 15; // raio do item entre os valores do tiro e do jogador
    private float r, g, b;
    private boolean isActive = false; // para verificar se o tiro está ativo e tem de ser desenhado

    public Item(float r, float g, float b) {
        this.r = r;
        this.g = g;
        this.b = b;
    }

    public void setPosition(float x, float y) {
        this.position.x = x;
        this.position.y = y;
    }

    public void activateItem() {
        this.isActive = true;
    }

    public void deactivateItem() {
        this.isActive = false;
    }

    public void spawn(){
        if (this.isActive){
            pushMatrix();
            stroke(255); 
            fill(this.r, this.g, this.b); 
            circle(this.position.x, this.position.y, this.radius);
            popMatrix();
        }
    }
}