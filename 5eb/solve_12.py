from collections import defaultdict
from pprint import pprint


def read_input(filename):
    with open(filename) as filehandle:
        lines = filehandle.read().splitlines()
    return lines


def find_path(connections, path, here):
    for place in connections[here]:
        if place not in path or place.isupper():
            new_path = path + [place]
            if place == 'end':
                yield tuple(new_path)
            yield from find_path(connections, new_path, place)


def map_connections(indata):
    connections = defaultdict(list)
    for line in indata:
        a, b = line.split('-')
        connections[a].append(b)
        connections[b].append(a)
    return connections


def part1(filename):
    connections = map_connections(read_input(filename))
    paths = list(find_path(connections, ['start'], 'start'))
    pprint(paths)
    return len(paths)


def find_path2(connections, path, here):
    for place in connections[here]:
        if place == 'start':
            continue
        new_path = path + [place]
        if place == 'end':
            yield tuple(new_path)
        else:
            if any([
                place.isupper(),
                place.islower() and place not in path,
                place.islower() and len([p for p in path if p.islower()]) == len(set(p for p in path if p.islower())),
            ]):
                yield from find_path2(connections, new_path, place)


def part2(filename):
    connections = map_connections(read_input(filename))
    paths = list(find_path2(connections, ['start'], 'start'))
    return len(paths)


def main():
    print(part1('input12_test1'))
    # print(part1('input12_test2'))
    # print(part1('input12_test3'))
    # print(part1('input12'))
    # print(part2('input12_test1'))
    # print(part2('input12_test2'))
    # print(part2('input12_test3'))
    print(part2('input12'))


if __name__ == '__main__':
    main()
