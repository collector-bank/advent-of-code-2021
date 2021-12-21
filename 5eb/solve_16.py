import itertools
import operator
import functools
from pprint import pprint


def read_input(filename):
    with open(filename) as filehandle:
        indata = filehandle.read()
    return indata


def hexchar2bits(hexchar):
    return f'{int(hexchar, base=16):>04b}'


assert hexchar2bits('0') == '0000'
assert hexchar2bits('1') == '0001'
assert hexchar2bits('F') == '1111'


def hex2bits(hexstr):
    return ''.join(hexchar2bits(char) for char in hexstr)


packet_types = {
    0: 'sum',
    1: 'product',
    2: 'min',
    3: 'max',
    4: 'litteral',
    5: 'gt',
    6: 'lt',
    7: 'eq',
}


def take(n, iterable):
    "Return first n items of the iterable as a list"
    return list(itertools.islice(iterable, n))


def take_bits(n, iterable):
    s = ''.join(itertools.islice(iterable, n))
    if not s:
        raise StopIteration
    return s


def parse_packet(ibits):
    # ibits = iter(bitstr)
    try:
        packet_version = int(take_bits(3, ibits), base=2)
        packet_type_id = int(take_bits(3, ibits), base=2)
    except StopIteration:
        return
    packet_type = packet_types.get(packet_type_id, 'operator')
    if packet_type == 'litteral':
        groups = []
        keep_going = 1
        while keep_going:
            keep_going, group_bits = (int(next(ibits)), take_bits(4, ibits))
            groups.append(group_bits)
        value = int(''.join(groups), base=2)
        yield {
            'packet_version': packet_version,
            'packet_type': packet_type,
            'value': value,
        }
    else:
        try:
            length_type_id = int(next(ibits))
        except StopIteration:
            return
        if length_type_id == 0:
            try:
                sub_packets_totlen = int(take_bits(15, ibits), base=2)
            except StopIteration:
                return
            try:
                sub_packets_bitstr = take_bits(sub_packets_totlen, ibits)
            except StopIteration:
                return
            sub_packets = list(parse_packet(iter(sub_packets_bitstr)))
            yield {
                'packet_version': packet_version,
                'packet_type': packet_type,
                'sub_packets': sub_packets,
            }
        else:
            try:
                num_sub_packets = int(take_bits(11, ibits), base=2)
            except StopIteration:
                return
            try:
                sub_packets = take(num_sub_packets, parse_packet(ibits))
            except StopIteration:
                return
            yield {
                'packet_version': packet_version,
                'packet_type': packet_type,
                'sub_packets': sub_packets,
            }
    yield from parse_packet(ibits)


def parse_packets(bitstr):
    return list(parse_packet(iter(bitstr)))


def recursive_packet_versions(packets):
    for packet in packets:
        yield packet['packet_version']
        sub_packets = packet.get('sub_packets', [])
        yield from recursive_packet_versions(sub_packets)


def part1(hexstr):
    bitstr = hex2bits(hexstr)
    parsed = parse_packets(bitstr)
    # pprint(parsed)
    return sum(recursive_packet_versions(parsed))


functions = {
    'sum': sum,
    'product': lambda iterable: functools.reduce(operator.mul, iterable, 1),
    'min': min,
    'max': max,
    'gt': lambda a: a[0] > a[1],
    'lt': lambda a: a[0] < a[1],
    'eq': lambda a: a[0] == a[1],
}


def evaluate_packet_instructions(packets):
    for packet in packets:
        if packet['packet_type'] == 'litteral':
            yield packet['value']
        else:
            function = functions[packet['packet_type']]
            sub_packets = packet.get('sub_packets', [])
            yield function(list(evaluate_packet_instructions(sub_packets)))


def part2(hexstr):
    bitstr = hex2bits(hexstr)
    packets = parse_packets(bitstr)
    # pprint(packets)
    return next(evaluate_packet_instructions(packets))


print(part1(read_input('input16')))
print(part2(read_input('input16')))
