public class Shot{

    private PVector position = new PVector(0, 0); // posição do tiro
    private float radius = 10; // Raio do tiro
    private float r = 255, g = 255, b = 0; // tiro tem cor amarela
    private boolean isActive = false; // para verificar se o tiro está ativo e tem de ser desenhado

    public void updateShot(float x, float y) {
        this.position.x = x;
        this.position.y = y;
    }

    public void activateShot() {
        this.isActive = true;
    }

    public void deactivateShot() {
        this.isActive = false;
    }

    public void renderShot(){
        if (isActive){
            pushMatrix();
            stroke(255); // contorno branco
            fill(this.r, this.g, this.b); // cor amarela
            circle(this.position.x, this.position.y, this.radius);
            popMatrix();
        }
    }
}