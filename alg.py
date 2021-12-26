sensors = dict([(1, .4), (2, .5), (3, .1)])
data = [
    [(1, {'temperature': 20.3}),
     (2, {'temperature': 21.1,
          'humidity': 44})],

    [(1, {'temperature': 20.7})],

    [(2, {'temperature': 20.8,
          'humidity': 44.5})]
]

from collections import defaultdict
def avg(data, param):
    if not data:
        return None
    table = defaultdict(list)
    for an_id, params in [pair for record in data for pair in record]:
        if param in params:
            table[an_id].append(params[param])
    res = {}
    for an_id, params in table.items():
        res[an_id] = sum(params) / len(params)
    return res

data_avg = avg(data, 'temperature')

def weight(weights, data):
    total = sum(map(lambda an_id: weights[an_id] if an_id in weights else 0, data.keys()))
    if total > 1:
        raise ValueError
    delta = (1 - total) / len(data)
    print('delta={}'.format(delta))
    res = {}
    for an_id, val in data.items():
        res[an_id] = val * weights[an_id] + val * delta
    return res

print(weight(sensors, data_avg))
