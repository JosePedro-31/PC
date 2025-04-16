import processing.net.*;
import java.net.*;


public class ConnectionManager {

  private socket socket;
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
      print("Sucesso\n");
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
  
   
  
}
