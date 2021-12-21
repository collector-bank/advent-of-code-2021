import itertools
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


# def hex2bits(hexstr):
#     return f'{int(hexstr, base=16):b}'.zfill(4 * len(hexstr))


packet_types = {
    4: 'litteral',
}
length_types = {
    0: 15,
    1: 11,
}


def take(n, iterable):
    "Return first n items of the iterable as a list"
    return list(itertools.islice(iterable, n))


def take_bits(n, iterable):
    "Return first n items of the iterable as a string"
    s = ''.join(itertools.islice(iterable, n))
    if s:
        return s
    else:
        print(len(s), n)
        raise StopIteration


def parse_packet(ibits):
    # ibits = iter(bitstr)
    try:
        packet_version = int(take_bits(3, ibits), base=2)
        packet_type_id = int(take_bits(3, ibits), base=2)
    except StopIteration:
        return
    packet_type = packet_types.get(packet_type_id, 'operator')
    match packet_type:
        case 'litteral':
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
        case 'operator':
            try:
                length_type_id = int(next(ibits))
            except StopIteration:
                return
            if length_type_id == 0:
                try:
                    sub_packets_totlen = int(take_bits(15, ibits), base=2)
                except StopIteration:
                    return
                print('exakt len', sub_packets_totlen)
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
                print('num sub', num_sub_packets)
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


# def packet_loop(bitstr):
#     while bitstr:
#         try:
#             result = parse_packet(iter(bitstr))
#             bitstr = result['bitstr']
#             print(bitstr)
#         except StopIteration:
#             break
#         else:
#             yield result


def parse_packets(bitstr):
    return list(parse_packet(iter(bitstr)))


def recursive_packet_versions(packets):
    for packet in packets:
        yield packet['packet_version']
        sub_packets = packet.get('sub_packets', [])
        yield from recursive_packet_versions(sub_packets)


def part1(hexstr):
    # print(repr(hexstr))
    bitstr = hex2bits(hexstr)
    # print(repr(bitstr))

    parsed = parse_packets(bitstr)
    pprint(parsed)
    return sum(recursive_packet_versions(parsed))


# print(parse_packet(iter(hex2bits('D2FE28'))))
# # assert parse_packet(iter(hex2bits('D2FE28'))) == ('litteral', 2021, '000')
# # assert parse_packet(hex2bits('D2FE28')) == ('litteral', 2021)
# print(hex2bits('38006F45291200'))
# print(parse_packet(iter('00111000000000000110111101000101001010010001001000000000')))
# print(parse_packet(iter(hex2bits('38006F45291200'))))
# print(parse_packet(iter(hex2bits('EE00D40C823060'))))


print('8A004A801A8002F478 represents an operator packet (version 4) which contains an operator packet (version 1) which contains an operator packet (version 5) which contains a literal value (version 6); this packet has a version sum of 16.')
pprint(part1('8A004A801A8002F478'))
print('620080001611562C8802118E34 represents an operator packet (version 3) which contains two sub-packets; each sub-packet is an operator packet that contains two literal values. This packet has a version sum of 12.')
pprint(part1('620080001611562C8802118E34'))
pprint('C0015000016115A2E0802F182340 has the same structure as the previous example, but the outermost packet uses a different length type ID. This packet has a version sum of 23.')
print(part1('C0015000016115A2E0802F182340'))

print('A0016C880162017C3686B18A3D4780 is an operator packet that contains an operator packet that contains an operator packet that contains five literal values; it has a version sum of 31.')
pprint(part1('A0016C880162017C3686B18A3D4780'))

# print(part1(read_input('input16')))
print(part1('0052E4A00905271049796FB8872A0D25B9FB746893847236200B4F0BCE5194401C9B9E3F9C63992C8931A65A1CCC0D222100511A00BCBA647D98BE29A397005E55064A9DFEEC86600BD002AF2343A91A1CCE773C26600D126B69D15A6793BFCE2775D9E4A9002AB86339B5F9AB411A15CCAF10055B3EFFC00BCCE730112FA6620076268CE5CDA1FCEB69005A3800D24F4DB66E53F074F811802729733E0040E5C5E5C5C8015F9613937B83F23B278724068018014A00588014005519801EC04B220116CC0402000EAEC03519801A402B30801A802138801400170A0046A800C10001AB37FD8EB805D1C266963E95A4D1A5FF9719FEF7FDB4FB2DB29008CD2BAFA3D005CD31EB4EF2EBE4F4235DF78C66009E80293AE9310D3FCBFBCA440144580273BAEE17E55B66508803C2E0087E630F72BCD5E71B32CCFBBE2800017A2C2803D272BCBCD12BD599BC874B939004B5400964AE84A6C1E7538004CD300623AC6C882600E4328F710CC01C82D1B228980292ECD600B48E0526E506F700760CCC468012E68402324F9668028200C41E8A30E00010D8B11E62F98029801AB88039116344340004323EC48873233E72A36402504CB75006EA00084C7B895198001098D91AE2190065933AA6EB41AD0042626A93135681A400804CB54C0318032200E47B8F71C0001098810D61D8002111B228468000E5269324AD1ECF7C519B86309F35A46200A1660A280150968A4CB45365A03F3DDBAE980233407E00A80021719A1B4181006E1547D87C6008E0043337EC434C32BDE487A4AE08800D34BC3DEA974F35C20100BE723F1197F59E662FDB45824AA1D2DDCDFA2D29EBB69005072E5F2EDF3C0B244F30E0600AE00203229D229B342CC007EC95F5D6E200202615D000FB92CE7A7A402354EE0DAC0141007E20C5E87A200F4318EB0C'))

