import processing.sound.*;

private enum State{
    MENU,
    REGISTER,
    LOGIN,
    LOGGED,
    LOBBY,
    PLAYING,
    MATCH_OVER
}

private State currentState;

private ConnectionManager cm;

private Player player = new Player(0, 0, 255); // Jogador é azul
private Player opponent = new Player(255, 0, 0); // adversário é vermelho

private Shot[] playerShots = new Shot[7]; // Array de tiros do jogador
private Shot[] opponentShots = new Shot[7]; // Array de tiros do adversário

private int previousShotTime = 0; // Tempo do último tiro disparado em milissegundos
private int shotCooldown = 250; // tempo de recarga do tiro em milissegundos

private boolean matchThreadStarted = false;

private Button loginButton;
private Button registerButton;
private Button continueButton;
private Button backButton;
private Button playButton;
private Button playAgainButton;

private InputBox usernameBox;
private InputBox passwordBox;

private SoundFile battleTheme;

private char previousKey = ' ';

void setup() {
    size(1000, 600);
    background(1);
    currentState = State.MENU;

    battleTheme = new SoundFile(this, "Sound/ducktales_moon_theme.wav");
    
    try {
        cm = new ConnectionManager("192.168.1.162", 1111); // Connect to the server
    } catch (Exception e) {
        println("Connection failed: " + e.getMessage());
    }
    
    // Initialize buttons
    registerButton = new Button("Images/register_default.png", "Images/register_hover.png", 0, 0);
    registerButton.updatePosition(width/2 - registerButton.width/2, height/2 - registerButton.height/2);
    
    loginButton = new Button("Images/login_default.png", "Images/login_hover.png", 0, 0);
    loginButton.updatePosition(width/2 - loginButton.width/2, height/2 + loginButton.height/2);
    
    backButton = new Button("Images/back_default.png", "Images/back_hover.png", 0, 0);
    backButton.updatePosition(0, backButton.height/2);

    continueButton = new Button("Images/continue_default.png", "Images/continue_hover.png", 0, 0);
    continueButton.updatePosition(width/2 - continueButton.width/2, height/2 + continueButton.height*1.5);
    
    playButton = new Button("Images/play_default.png", "Images/play_hover.png", 0, 0);
    playButton.updatePosition(width/2 - playButton.width/2, height/2 + playButton.height/2);    

    playAgainButton = new Button("Images/play_again_default.png", "Images/play_again_hover.png", 0, 0);
    playAgainButton.updatePosition(width/2 - playAgainButton.width/2, height/2 + playAgainButton.height*2);

    // Initialize input boxes
    usernameBox = new InputBox("Images/username_box.png", 0, 0);
    usernameBox.updatePosition(width/2 - usernameBox.width/2, height/2 - usernameBox.height);

    passwordBox = new InputBox("Images/password_box.png", 0, 0);
    passwordBox.updatePosition(width/2 - passwordBox.width/2, height/2);

    for (int i = 0; i < 7; i++) {
    playerShots[i] = new Shot(); // Tiros do jogador são azuis
    opponentShots[i] = new Shot(); // Tiros do adversário são vermelhos
    }
}

void draw() {
    background(1);
    switch (currentState) {
        case MENU:
            loginButton.draw();
            registerButton.draw();
            backButton.draw();
            break;
        case REGISTER:
            backButton.draw();
            usernameBox.draw();
            passwordBox.draw();
            continueButton.draw();
            break;
        case LOGIN:
            backButton.draw();
            usernameBox.draw();
            passwordBox.draw();
            continueButton.draw();            
            break;
        case LOGGED:
            playButton.draw();
            backButton.draw();
            break;
        case LOBBY:
            // TO DO: Lobby screen
            fill(255);
            text("Lobby screen", 300, 300);
            String response = this.cm.receiveMessage();
            if(response.equals("Match started")){
                currentState = State.PLAYING;
                battleTheme.play();
            }
            break;
        case PLAYING:        
            player.renderPlayer();
            opponent.renderPlayer();

            fill(255);
            textSize(20);
            text(player.getName() + ": " + player.getPoints() + " Points", 20, 30);
            
            text(opponent.getName() + ": " + opponent.getPoints() + " Points", 800, 30);

            for (int i = 0; i < playerShots.length; i++) {
                playerShots[i].renderShot();
                opponentShots[i].renderShot();
            }
    
            if (matchThreadStarted == false){
                matchThreadStarted = true;
                println("Starting parser thread...");
                thread("parser");
            }
            break;
        case MATCH_OVER:
            //escrever o resultado do jogo
            playAgainButton.draw();
            backButton.draw();
            break;
    }
}

void mouseMoved() {
    if (currentState == State.MENU) {
        if (loginButton.isMouseOver()) {
            loginButton.changeToHover();
        } else {
            loginButton.changeToDefault();
        }
        if (registerButton.isMouseOver()) {
            registerButton.changeToHover();
        } else {
            registerButton.changeToDefault();
        }
        if (backButton.isMouseOver()) {
            backButton.changeToHover();
        } else {
            backButton.changeToDefault();
        }
    }
    if(currentState == State.REGISTER || currentState == State.LOGIN) {
        if (continueButton.isMouseOver()) {
            continueButton.changeToHover();
        } else {
            continueButton.changeToDefault();
        }
        if (backButton.isMouseOver()) {
            backButton.changeToHover();
        } else {
            backButton.changeToDefault();
        }
    }
    if(currentState == State.LOGGED) {
        if (playButton.isMouseOver()) {
            playButton.changeToHover();
        } else {
            playButton.changeToDefault();
        }
        if (backButton.isMouseOver()) {
            backButton.changeToHover();
        } else {
            backButton.changeToDefault();
        }
    }
    if(currentState == State.MATCH_OVER) {
        if (playAgainButton.isMouseOver()) {
            playAgainButton.changeToHover();
        } else {
            playAgainButton.changeToDefault();
        }
        if (backButton.isMouseOver()) {
            backButton.changeToHover();
        } else {
            backButton.changeToDefault();
        }
    }
}

void mousePressed() {
    if (currentState == State.MENU) {
        if (loginButton.isMouseOver() && mouseButton == LEFT) {
            currentState = State.LOGIN;
            loginButton.changeToDefault();

        } else if (registerButton.isMouseOver() && mouseButton == LEFT) {
            currentState = State.REGISTER;
            registerButton.changeToDefault();

        } else if (backButton.isMouseOver() && mouseButton == LEFT) {
            exit(); // Close the application
        }
    }
    else if(currentState == State.REGISTER || currentState == State.LOGIN) {
        if (backButton.isMouseOver() && mouseButton == LEFT) {
            currentState = State.MENU;
            usernameBox.clearText();
            usernameBox.deselect();
            passwordBox.clearText();
            passwordBox.deselect();

        } else if (usernameBox.isMouseOver() && mouseButton == LEFT) {
            usernameBox.select();
            passwordBox.deselect();

        } else if (passwordBox.isMouseOver() && mouseButton == LEFT) {
            passwordBox.select();
            usernameBox.deselect();

        } else if(currentState == State.LOGIN && continueButton.isMouseOver() && mouseButton == LEFT) {
            //TO DO: Verificar se o user e a pass estão corretos
            println("Username: " + usernameBox.getText() + ", Password: " + passwordBox.getText());
            
            cm.loginUser(usernameBox.getText(), passwordBox.getText());
            String response = this.cm.receiveMessage();
            println("Response: " + response);

            if (response.equals("Login successful")){
                player.setName(usernameBox.getText());
                currentState = State.LOGGED;
                continueButton.changeToDefault();
            }
            
            usernameBox.clearText();
            passwordBox.clearText();
            usernameBox.deselect();
            passwordBox.deselect();

        } else if(currentState == State.REGISTER && continueButton.isMouseOver() && mouseButton == LEFT) {
            //TO DO: Verificar se o username ainda não existe, se não guardar o nome e a pass
            println("Username: " + usernameBox.getText() + ", Password: " + passwordBox.getText());
            
            cm.registerUser(usernameBox.getText(), passwordBox.getText());
            String response = this.cm.receiveMessage();
            println("Response: " + response);

            if (response.equals("Registration successful")){
                player.setName(usernameBox.getText());
                currentState = State.LOGGED;
                continueButton.changeToDefault();
            }
            
            usernameBox.clearText();
            passwordBox.clearText();
            usernameBox.deselect();
            passwordBox.deselect();
        }
    }
    else if(currentState == State.LOGGED) {
        if (playButton.isMouseOver() && mouseButton == LEFT) {
            
            this.cm.joinLobby(player.getName());
            String response = this.cm.receiveMessage();
            if(response.equals("Join lobby successful")){
                currentState = State.LOBBY;
                playButton.changeToDefault();
            }

        } else if (backButton.isMouseOver() && mouseButton == LEFT) {
            currentState = State.MENU;
            
        }
    }
    else if(currentState == State.PLAYING) {
        if (mouseButton == LEFT && shotCooldown < millis() - previousShotTime) {
            this.cm.sendShot(mouseX, mouseY);
            previousShotTime = millis();
        }
    }

    else if(currentState == State.MATCH_OVER) {
        if (playAgainButton.isMouseOver() && mouseButton == LEFT) {
            
            this.cm.joinLobby(player.getName());
            String response = this.cm.receiveMessage();
            if(response.equals("Join lobby successful")){
                playAgainButton.changeToDefault();
                currentState = State.LOBBY;
            }

        } else if (backButton.isMouseOver() && mouseButton == LEFT) {
            currentState = State.LOGGED;
            
        }
    }
    
}

void parser(){
    // This function is called in a separate thread to parse messages from the server
    println("Parser thread started...");
    String message = this.cm.receiveMessage();
    println("Message: " + message);
    while (true) {
        String[] parts = message.split(",");
        if (parts[0].equals("Players")){ // "Players,username1,x1,y1,points1,username2,x2,y2,points2"
            String name1 = parts[1];
            float x1 = Float.parseFloat(parts[2]);
            float y1 = Float.parseFloat(parts[3]);
            int points1 = Integer.parseInt(parts[4]);
            String name2 = parts[5];
            float x2 = Float.parseFloat(parts[6]);
            float y2 = Float.parseFloat(parts[7]);
            int points2 = Integer.parseInt(parts[8]);

            if (name1.equals(player.getName())){
                player.updatePlayer(name1, x1, y1, points1);
                opponent.updatePlayer(name2, x2, y2, points2);
            } else {
                opponent.updatePlayer(name1, x1, y1, points1);
                player.updatePlayer(name2, x2, y2, points2);
            }
        } else if(parts[0].equals("ShotsPlayer")){ // "ShotsPlayer,num de shots ex:2,x1,y1,x2,y2"
            int numShots = Integer.parseInt(parts[1]);
            int c = 0;
            for (int i = 2; i < 2 + numShots * 2; i += 2, c += 1) {
                float x = Float.parseFloat(parts[i]);
                float y = Float.parseFloat(parts[i + 1]);
                playerShots[c].updateShot(x, y);
                playerShots[c].activateShot();
            }
            while (c < playerShots.length) { // Deactivate remaining shots
                playerShots[c].deactivateShot();
                c++;
            }
        } else if(parts[0].equals("ShotsOpponent")){ // "ShotsOpponent,num de shots ex:2,x1,y1,x2,y2"
            int numShots = Integer.parseInt(parts[1]);
            int c = 0;
            for (int i = 2; i < 2 + numShots * 2; i += 2, c += 1) {
                float x = Float.parseFloat(parts[i]);
                float y = Float.parseFloat(parts[i + 1]);
                opponentShots[c].updateShot(x, y);
                opponentShots[c].activateShot();
            }
            while (c < opponentShots.length) { // Deactivate remaining shots
                opponentShots[c].deactivateShot();
                c++;
            }
        }

        message = this.cm.receiveMessage();
        println("Message: " + message);
    }
}


void keyPressed() {
    if (currentState == State.REGISTER || currentState == State.LOGIN) {
        if (usernameBox.isSelected()) {
            usernameBox.receiveKey(key);
            usernameBox.draw(); // Redraw the input box to show the username
        } else if (passwordBox.isSelected()) {
            passwordBox.receiveKey(key);
            passwordBox.draw(); // Redraw the input box to show the password
        }
    } else if (currentState == State.PLAYING) {
        if (key != previousKey) { // Ignore repeated key presses
            this.cm.sendKeyPress(key); 
            previousKey = key;
        }
    }
}

void keyReleased() {
    if (currentState == State.PLAYING) {
        this.cm.sendKeyRelease(key);
        previousKey = ' '; // Reset previous key when released
    }
}
