import statistics
import collections


def read_input(filename):
    with open(filename) as filehandle:
        lines = filehandle.read().splitlines()
    return lines


parenthesis = {
    '(': ')',
    '[': ']',
    '{': '}',
    '<': '>',
}
corrupt_points = {
    ')': 3,
    ']': 57,
    '}': 1197,
    '>': 25137,
}
incomplete_points = {
    ')': 1,
    ']': 2,
    '}': 3,
    '>': 4,
}


def find_wrong_paren(line):
    stack = collections.deque()
    for char in line:
        if char in parenthesis:
            stack.append(char)
        else:
            last_opened = stack.pop()
            expected = parenthesis[last_opened]
            if char != expected:
                return {'expected': expected, 'found': char}
    if stack:
        return {'closed_by': ''.join([parenthesis[char] for char in reversed(stack)])}
    return {}


def corrupt_score(line):
    errors = find_wrong_paren(line)
    return corrupt_points.get(errors.get('found'), 0)


def calc_completion_score(completion_line):
    line_total = 0
    for char in completion_line:
        value = incomplete_points[char]
        line_total *= 5
        line_total += value
    return line_total


def part1(filename):
    return sum(corrupt_score(line) for line in read_input(filename))


def part2(filename):
    completion_lines = [find_wrong_paren(line).get('closed_by', '') for line in read_input(filename)]
    completion_lines = [line for line in completion_lines if line]
    scores = [calc_completion_score(line) for line in completion_lines]
    return statistics.median(scores)


def main():
    print(part1('input10'))
    print(part2('input10'))


if __name__ == '__main__':
    main()
