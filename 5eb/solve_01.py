import itertools
import collections


def count_increasing(measurements):
    return sum(a < b for a, b in itertools.pairwise(measurements))


test_data = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
assert count_increasing(test_data) == 7

with open('input01') as fh:
    measurements = [int(n) for n in fh.read().splitlines()]
print(count_increasing(measurements))


def sliding_window(iterable, n):
    # sliding_window('ABCDEFG', 4) -> ABCD BCDE CDEF DEFG
    it = iter(iterable)
    window = collections.deque(itertools.islice(it, n), maxlen=n)
    if len(window) == n:
        yield tuple(window)
    for x in it:
        window.append(x)
        yield tuple(window)


def count_increasing_window(measurements):
    return count_increasing([sum(group) for group in sliding_window(measurements, 3)])


assert count_increasing_window(test_data) == 5
print(count_increasing_window(measurements))
