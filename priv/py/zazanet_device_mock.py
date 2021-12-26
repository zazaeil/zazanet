import random
import time
import argparse
import json

import zeroconf

def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--id', type=int, default=random.randint(0, 4_294_967_295), help='Zazanet Device ID.')
    return parser.parse_args()

def get_param(key_val):
    [k, v] = key_val.split('=')
    if v == 'null':
        v = None
    if k == 'temperature':
        return [k, float(v)]
    elif k == 'humidity':
        return [k, float(v)]
    elif k == 'battery':
        return [k, float(v)]
    return [k, v]

from dataclasses import dataclass
@dataclass
class State:
    backend_addr: str # IPv4 + TCP port

if __name__ == '__main__':
    args = get_args()
    state = State(backend_addr=None)
    zc = zeroconf.Zeroconf(ip_version=zeroconf.IPVersion.V4Only)
    try:
        cmd = input(f'[zazanet-device-{args.id}] >> ')
        while cmd and cmd != ':exit':
            cmd = cmd.split()
            if cmd[0] == ':scan':
                 services = ["_http._tcp.local."]
                 services = list(zeroconf.ZeroconfServiceTypes.find(zc=zc))
                 print(services)
            if cmd[0] == ':send':
                params = {}
                for param in cmd[1:]:
                    try:
                        [k, v] = get_param(param)
                        params[k] = v
                    except ValueError:
                        print(f'ERROR: bad param: {param}.')
                        continue
                a_json = json.dumps({
                    'id': args.id,
                    'event_ts': time.time() * 1_000, # milliseconds
                    'state': params
                })
                if state.backend_addr:
                    print(a_json)
                else:
                    print('Need to wait.')
            else:
                print('Unknown command.')
            cmd = input(f'[zazanet-device-{args.id}] >> ')
    except KeyboardInterrupt:
        pass
    finally:
        zc.close()
