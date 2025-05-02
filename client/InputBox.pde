public class InputBox {
    private PImage image;
    private String text = "";
    private boolean selected = false; 
    float x;
    float y; 
    float width;
    float height;

    public InputBox(String image_path, float x, float y) {
        this.image = loadImage(image_path);
        this.x = x;
        this.y = y;
        this.width = this.image.width;
        this.height = this.image.height;
    }

    public void updatePosition(float x, float y) {
        this.x = x;
        this.y = y;
    }

    public void select(){
        this.selected = true;
    }

    public void deselect(){
        this.selected = false;
    }

    public boolean isSelected() {
        return this.selected;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getText() {
        return this.text;
    }

    public void clearText() {
        this.text = "";
    }

    public void receiveKey(char key){
        int ascii_value = (int) key;
        if (ascii_value == 8) { // Backspace key
            deleteCharacter();
        } else if ((this.text.length() < 16) && // Limit username to 16 characters
                  (ascii_value >= 48 && ascii_value <= 57) || // Numeric characters
                  (ascii_value >= 65 && ascii_value <= 90) || // Uppercase letters
                  (ascii_value >= 97 && ascii_value <= 122)) { // Lowercase letters
            this.text += key;
        }
    }

    private void deleteCharacter() {
        if(this.text.length() > 0) {
            this.text = this.text.substring(0, this.text.length() - 1); // Remove the last character
        }
    }

    public boolean isMouseOver() {
        return (mouseX > this.x && mouseX < this.x + this.width && mouseY > this.y && mouseY < this.y + this.height);
    }

    public void draw() {
        image(this.image, this.x, this.y);
        fill(0);
        textSize(14);
        textAlign(LEFT, CENTER);
        text(this.text, this.x + 10, this.y + this.height/2 + 7); // Adjusted to center the text vertically
    }
}