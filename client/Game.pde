
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

private Player player = new Player(0, 0, 255); // Player is blue
private Player opponent = new Player(255, 0, 0); // Opponent is red

private Button loginButton;
private Button registerButton;
private Button continueButton;
private Button backButton;
private Button playButton;
private Button playAgainButton;

private InputBox usernameBox;
private InputBox passwordBox;

void setup() {
    size(1000, 600);
    background(1);
    currentState = State.MENU;
    /*
    try {
        cm = new ConnectionManager("localhost", 1234); // Connect to the server
    } catch (Exception e) {
        println("Connection failed: " + e.getMessage());
    }
    */
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
            text("Lobby screen", 300, 300);
            String response = this.cm.receiveMessage();
            if(response.equals("match found")){
                currentState = State.PLAYING;
            }
            break;
        case PLAYING:
            // TO DO: Playing screen
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
            // if correto:
                player.setName(usernameBox.getText());
                usernameBox.clearText();
                passwordBox.clearText();
                currentState = State.LOGGED;
                continueButton.changeToDefault();

        } else if(currentState == State.REGISTER && continueButton.isMouseOver() && mouseButton == LEFT) {
            //TO DO: Verificar se o username ainda não existe, se não guardar o nome e a pass
            println("Username: " + usernameBox.getText() + ", Password: " + passwordBox.getText());
            // if valido:
                player.setName(usernameBox.getText());
                usernameBox.clearText();
                passwordBox.clearText();
                currentState = State.LOGGED;
                continueButton.changeToDefault();

        }
    }
    else if(currentState == State.LOGGED) {
        if (playButton.isMouseOver() && mouseButton == LEFT) {
            this.cm.joinMatch(player.getName());
            String response = this.cm.receiveMessage();
            if(response.equals("joined a lobby")){
                playButton.changeToDefault();
                currentState = State.LOBBY;
            }

        } else if (backButton.isMouseOver() && mouseButton == LEFT) {
            currentState = State.MENU;
            
        }
    }
    else if(currentState == State.MATCH_OVER) {
        if (playAgainButton.isMouseOver() && mouseButton == LEFT) {
            this.cm.joinMatch(player.getName());
            String response = this.cm.receiveMessage();
            if(response.equals("joined a lobby")){
                playAgainButton.changeToDefault();
                currentState = State.LOBBY;
            }

        } else if (backButton.isMouseOver() && mouseButton == LEFT) {
            currentState = State.LOGGED;
            
        }
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
    }
}

void keyReleased() {
    if (key == 'w' || key == 'a' || key == 's' || key == 'd') {
        this.cm.sendKeyRelease(key);
    }
}