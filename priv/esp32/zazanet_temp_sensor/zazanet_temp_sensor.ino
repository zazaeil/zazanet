#include <ESP.h>
#include <ESPmDNS.h>
#include <WiFi.h>
#include <HTTPClient.h>
#include <DHT.h>

#define DHTTYPE DHT11

#define INTERVAL_MICROSECONDS 3e7

const char* ssid = "";
const char* password =  "";

uint8_t DHTPin = 5; 
DHT dht(DHTPin, DHTTYPE);
  
void setup() {
  Serial.begin(115200);
  while (!Serial) { }

  pinMode(DHTPin, INPUT);
  dht.begin();
  
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
  }
}

void loop() {
   if (!MDNS.begin("zazanet_esp32_dht11_sensor")) {
    Serial.println("[ERROR] mDNS failed to start");
    
    return;
  }
    
  int servicesSize = MDNS.queryService("http", "tcp");
  Serial.printf("Discovered %i \"_http._tcp\" mDSN services.\n", servicesSize);
  int zazanetBackendIndex = -1;
  for (int i = 0; i < servicesSize; i++) {
    if (MDNS.txt(i, "zazanet-service") == "backend") {
      zazanetBackendIndex = i;

      break;
    }
  }

  if (zazanetBackendIndex == -1) {
    return;
  }

  String zazanetBackendIP = MDNS.IP(zazanetBackendIndex).toString();

  String chipID = String((uint32_t)ESP.getEfuseMac());
  
  HTTPClient http;
  http.begin("http://" + zazanetBackendIP + ":" + MDNS.port(zazanetBackendIndex) + "/api/v1/devices/" + chipID);   
  // all headers are important, otherwise the backend will reject them
  http.addHeader("Content-Type", "application/json");
  http.addHeader("Accept", "application/json");
  http.setTimeout(10000);
  http.setReuse(false);
  
  char JSON[65];
  for (int i = 0; i < 65; i++) {
    JSON[i] = '0';
  }
  snprintf(JSON, 
    sizeof(JSON), 
    "{\"ttl\":%u,\"state\":{\"temperature\":%.2f,\"humidity\":%.2f}}", 
    (int32_t)((2 * INTERVAL_MICROSECONDS) / 1000), // ttl
    dht.readTemperature(),
    dht.readHumidity());

  int httpStatusCode = http.PUT(JSON);
  Serial.printf("[%i] PUT %s:%i/api/v1/devices/%s\n%s\n", httpStatusCode, zazanetBackendIP.c_str(), MDNS.port(zazanetBackendIndex), chipID.c_str(), JSON);
  http.end();
    
  MDNS.end();

  ESP.deepSleep(INTERVAL_MICROSECONDS);
}
