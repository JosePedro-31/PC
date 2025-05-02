
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

private Button loginButton;
private Button registerButton;
private Button exitButton;
private Button playButton;
private Button backButton;
private Button continueButton;

private InputBox usernameBox;
private InputBox passwordBox;

void setup() {
    size(1000, 600);
    background(1);
    currentState = State.MENU;

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
    //exitButton = new Button("assets/exit.png", "assets/exit_hover.png", 100, 300);

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
            break;
        case PLAYING:
            // TO DO: Playing screen
            break;
        case MATCH_OVER:
            // TO DO: Match over screen
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
            usernameBox.clearText();
            passwordBox.clearText();
            currentState = State.LOGGED;
            continueButton.changeToDefault();

        } else if(currentState == State.REGISTER && continueButton.isMouseOver() && mouseButton == LEFT) {
            //TO DO: Verificar se o username ainda não existe, se não guardar o nome e a pass
            println("Username: " + usernameBox.getText() + ", Password: " + passwordBox.getText());
            usernameBox.clearText();
            passwordBox.clearText();
            currentState = State.LOGGED;
            continueButton.changeToDefault();

        }
    }
    else if(currentState == State.LOGGED) {
        if (playButton.isMouseOver() && mouseButton == LEFT) {
            //TO DO: Entrar no jogo
            println("Entrar no jogo");
            playButton.changeToDefault();
            currentState = State.LOBBY;

        } else if (backButton.isMouseOver() && mouseButton == LEFT) {
            currentState = State.MENU;
            
        }
    }
    
}

void keyPressed() {
    if (currentState == State.REGISTER || currentState == State.LOGIN) {
        if (usernameBox.isSelected()) {
            usernameBox.receiveKey(key);
            usernameBox.draw(); // Redraw the input box to show the updated text
        } else if (passwordBox.isSelected()) {
            passwordBox.receiveKey(key);
            passwordBox.draw(); // Redraw the input box to show the updated text
        }
    }
}