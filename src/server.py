import random
import requests
import time

if __name__ == '__main__':
    while True:
        # value = random.randint(14, 25) + random.random()
        value = float(input('temperature = '))
        resp = requests.put('http://localhost:8080/api/v1/timeline', json={
            'when': int(time.time() * 1_000),
            'what': 'sensor.1.temperature',
            'data': {
                'hardware': 'dht11',
                'unit': 'celsius',
                'value': value
            }
        })
        print('{} :: {}'.format(value, resp.status_code))
        time.sleep(1)
