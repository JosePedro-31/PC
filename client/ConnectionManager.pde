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

  public void sendMessage(String message) throws IOException { // Adiciona a declaração throws
    if (isActive && out != null) {
        out.println(message);
        out.flush();
        // Verifica erros após o flush
        if (out.checkError()) {
            throw new IOException("Error sending message: PrintWriter encountered an error.");
        }
    } else {
        // Lança uma exceção em vez de imprimir para System.err
        throw new IOException("Connection is not active or output stream is null.");
    }
  }
  
}
