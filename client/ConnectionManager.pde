import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.*;

public class ConnectionManager {

  private Socket socket;
  private BufferedReader in;
  private PrintWriter out;
  public boolean isActive;
  StringBuilder sb = new StringBuilder();


  public ConnectionManager(String host, int port) {
    try {
      this.socket = new Socket(host, port);
      this.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
      this.out = new PrintWriter(socket.getOutputStream());
      this.isActive = true;
      print("Sucess\n");
    }
    catch (IOException e) {
      this.isActive = false;
      e.printStackTrace();
    }
  }
  
  public String receiveMessage() {

    String message = "";

    try {
      message = this.in.readLine();
    }
    catch (IOException e) {
      message = "An error has occured";
    }
    return message;
  }

  public boolean registerUser(String username, String password) {
    this.out.println("register_user," + username + "," + password);
    this.out.flush();
    return true;
  }

  public boolean loginUser(String username, String password) {
    this.out.println("login_user," + username + "," + password);
    this.out.flush();
    return true;
  }

  public void joinLobby(String username) {
    this.out.println("join_lobby," + username);
    this.out.flush();
  }

  public void sendKeyPress(char key) {
    if(key == 'w' || key == 'a' || key == 's' || key == 'd') {
      this.out.println("key_press," + key);
      this.out.flush();
    }
  }

  public void sendKeyRelease(char key) {
    if(key == 'w' || key == 'a' || key == 's' || key == 'd') {
      this.out.println("key_release," + key);
      this.out.flush();
    }
  }
  
}
