import processing.net.*;

Client tcpClient;
String serverIp = "127.0.0.1"; // Ou o IP do servidor
int serverPort = 12345;       // Mesma porta do servidor

void setup() {
  size(400, 200);
  textSize(20);
  connectToServer();
}

void draw() {
  background(0);
  if (tcpClient != null && tcpClient.available() > 0) {
    String message = tcpClient.readString();
    if (message != null) {
      fill(255);
      text("Server says: " + message, 50, 100);
    }
  } else if (tcpClient == null || !tcpClient.active()) {
     fill(255,0,0);
     text("Disconnected", 50, 100);
  }
}

void connectToServer() {
  try {
    tcpClient = new Client(this, serverIp, serverPort);
    if (tcpClient.active()) {
       println("Connected to server!");
       tcpClient.write("Hello from Processing!\n"); // Enviar algo
    }
  } catch (Exception e) {
     println("Error connecting: " + e.getMessage());
     tcpClient = null;
  }
}

// Tenta reconectar se clicar
void mousePressed() {
    if (tcpClient == null || !tcpClient.active()) {
        connectToServer();
    }
}
