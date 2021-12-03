from collections import Counter
from itertools import accumulate


test_data = """forward 5
               down 5
               forward 8
               up 3
               down 8
               forward 2
""".splitlines()


with open('in02') as fh:
    indata = fh.read().splitlines()


def course_step(line):
    direction, value = line.split()
    return Counter({direction: int(value)})


def find_destination(lines):
    total = sum([course_step(line) for line in lines], start=Counter())
    return total['forward'], total['down'] - total['up']


assert find_destination(test_data) == (15, 10)
hor, depth = find_destination(indata)
print(hor, depth, hor * depth)


def find_destination2(lines):
    instructions = [course_step(line) for line in lines]
    submarine = Counter()
    for instruction in instructions:
        submarine += instruction
        submarine['depth'] += (submarine['down'] - submarine['up']) * instruction['forward']
    return submarine['forward'], submarine['depth']


assert find_destination2(test_data) == (15, 60)
hor, depth = find_destination2(indata)
print(hor, depth, hor * depth)


def find_destination3(lines):
    """nah, less readable"""
    instructions = [course_step(line) for line in lines]
    agg = list(accumulate(instructions, initial=Counter()))
    depth = sum([
        (agg_state['down'] - agg_state['up']) * instruction['forward']
        for instruction, agg_state in zip(instructions, agg)])
    return agg[-1]['forward'], depth

assert find_destination3(test_data) == (15, 60)
