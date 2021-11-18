import random
import time

import requests as R

if __name__ == '__main__':
    an_id = random.randint(0, 10)
    while True:
        R.put('http://localhost/api/v1/devices/' + str(an_id),
              headers={'Accept': 'application/json'},
              json={
                  'ttl': 10000,
                  'state': {
                      'temperature': {
                          'value': random.randint(18, 32) + random.random()
                      }
                  }
              })
        time.sleep(10)
