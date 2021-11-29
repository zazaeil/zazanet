#include <ESPmDNS.h>
#include <WiFi.h>
#include <HTTPClient.h>

#define DEVICE String("zazanet-temp-sensor")
#define DEVICE_VSN String("0.0.0")
#define INTERVAL_MICROSECONDS 3e6

const char* ssid = "";
const char* password =  "";

void setup() {
  Serial.begin(115200);
  Serial.setTimeout(1000);

  while (!Serial) { }

  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
  }
  Serial.println("WiFi OK.");
}

void loop() {
   if (!MDNS.begin(DEVICE.c_str())) {
    Serial.println("[ERROR] mDNS failed to start");

    return;
  }

  int servicesSize = MDNS.queryService("http", "tcp");
  Serial.printf("Discovered %i \"_http._tcp\" mDSN services.\n", servicesSize);
  if (servicesSize == 0) {
    return;
  } else {
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

    HTTPClient http;
    http.begin("http://" + zazanetBackendIP + ":" + MDNS.port(zazanetBackendIndex) + "/api/v1/data");
    // all headers are important, otherwise the backend will reject them
    http.addHeader("Content-Type", "application/json");
    http.setUserAgent(DEVICE + "/" + DEVICE_VSN);
    http.setTimeout(5000);
    http.setReuse(false);
    int httpStatusCode = http.POST(
        "{"
          " \"hello\": \"world\" "
        "}");
    Serial.printf("[%i] POST %s:%i/api/v1/data\n", httpStatusCode, zazanetBackendIP.c_str(), MDNS.port(zazanetBackendIndex));
    http.end();

    MDNS.end();

    ESP.deepSleep(INTERVAL_MICROSECONDS);
  }
}
