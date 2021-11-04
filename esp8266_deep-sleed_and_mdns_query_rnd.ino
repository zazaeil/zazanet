#ifdef ESP32
#include <ESPmDNS.h>
#include <WiFi.h>

#else
#include <ESP8266mDNS.h>
#include <ESP8266WiFi.h>
#endif

const char* ssid = "Becoss_lan";
const char* password =  "82181444";

void setup() {
  Serial.begin(74880);  // Тут внимание на скорость порта. Не забудь перевести на такую же скорость свой Serial monitor!
  Serial.setTimeout(1000);

  // Ждем инициализации последовательной коммуникации:
  while (!Serial) { }

  Serial.println("\nI'm awake, but I'm going into deep sleep mode for 3 seconds");
  //  "Я проснулся, но перехожу
  //   в режим глубокого сна на 33 секунды"
  WiFi.begin(ssid, password);

  Serial.print("Connecting to WiFi");
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.print("\n");
}

void loop() {

  if (!MDNS.begin("ESP")) {
    Serial.println("mDNS failed to start");
    return;
  }

  int nrOfServices = MDNS.queryService("http", "tcp");

  if (nrOfServices == 0) {
    Serial.println("No services were found.");
  }

  else {

    Serial.print("Number of services found: ");
    Serial.println(nrOfServices);

    for (int i = 0; i < nrOfServices; i = i + 1) {

      Serial.println("---------------");

      Serial.print("Hostname: ");
      Serial.println(MDNS.hostname(i));

      Serial.print("IP address: ");
      Serial.println(MDNS.IP(i));

      Serial.print("Port: ");
      Serial.println(MDNS.port(i));

      //Serial.print("Service: ");
      //Serial.println(MDNS.txt(i, "zazanet-service")); // это работает на ESP32 в либе <ESPmDNS.h><ESPmDNS.h>
      // но не работает на ESP8266 в либе <ESP8266mDNS.h><ESP8266mDNS.h>

      Serial.println("---------------");
    }

    // Переводим ESP8266 в режим глубокого сна на 30 секунд;
    // ESP8266 будет пробуждать себя сама,
    // пока контакт GPIO16 (D0 на плате NodeMCU)
    // будет подключен к контакту RST:
    ESP.deepSleep(3e6); // в микросекундах!
  }
}
