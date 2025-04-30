Player p = new Player(255, 0, 0); // Create a player with red color

void setup() {
    size(1280, 720);
    background(255);
    p.updatePlayer("Player1", 100, 100, 0); // Update player position and points
    p.renderPlayer(); // Render the player
}
