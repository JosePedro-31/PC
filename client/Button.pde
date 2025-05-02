public class Button{
    private PImage current_image;
    private PImage default_image;
    private PImage hover_image;
    float x;
    float y;
    float width;
    float height;

    public Button(String default_path, String hover_path, float x, float y){
        this.default_image = loadImage(default_path);
        this.hover_image = loadImage(hover_path);
        this.current_image = this.default_image;
        this.x = x;
        this.y = y;
        this.width = this.current_image.width;
        this.height = this.current_image.height;
    }

    public void draw(){
        image(this.current_image, this.x, this.y);
    }

    public void updatePosition(float x, float y){
        this.x = x;
        this.y = y;
    }

    public void changeToHover(){
        this.current_image = this.hover_image;
    }
    public void changeToDefault(){
        this.current_image = this.default_image;
    }

    // Check if the mouse is over the button
    public boolean isMouseOver(){
        return (mouseX > this.x && mouseX < this.x + this.width && mouseY > this.y && mouseY < this.y + this.height);
    }
}