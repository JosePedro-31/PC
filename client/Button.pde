Public Class Button{
    private PImage current_image;
    private PImage default_image;
    private PImage hover_image;
    private float x;
    private float y;
    private float width;
    private float height;

    Public Button(String default_path, String hover_path, float x, float y){
        this.default_image = loadImage(default_path);
        this.hover_image = loadImage(hover_path);
        this.current_image = this.default_image;
        this.x = x;
        this.y = y;
        this.width = this.current_image.width;
        this.height = this.current_image.height;
    }

    Public void draw(){
        image(this.current_image, this.x, this.y);
    }

    Public void updatePosition(float x, float y){
        this.x = x;
        this.y = y;
    }

    Public void changeToHover(){
        this.current_image = this.hover_image;
    }
    Public void changeToDefault(){
        this.current_image = this.default_image;
    }

}