import statistics


def read_input(filename):
    with open(filename) as filehandle:
        indata = filehandle.read()
    return [int(n) for n in indata.strip().split(',')]


def part1(filename):
    sub_positions = read_input(filename)
    align_to = statistics.median(sub_positions)
    return sum(abs(pos - align_to) for pos in sub_positions)


def fuel_cost(align_to, position):
    distance = abs(position - align_to)
    return sum(range(distance + 1))


def total_fuel_cost(align_to, positions):
    return sum(fuel_cost(align_to, position) for position in positions)


def part2(filename):
    positions = read_input(filename)
    local_search_area = [
        round(statistics.mean(positions)) + diff
        for diff in range(-10, 11)]
    min_cost, best_integer_target = min([
        (total_fuel_cost(target, positions), target)
        for target in local_search_area]
    )
    return min_cost, best_integer_target


def main():
    assert part1('input07_test') == 37
    print(part1('input07'))
    # Part 2:
    assert fuel_cost(5, 16) == 66
    assert fuel_cost(5, 1) == 10
    assert fuel_cost(5, 14) == 45
    assert fuel_cost(5, 0) == 15
    assert total_fuel_cost(5, read_input('input07_test')) == 168
    assert total_fuel_cost(2, read_input('input07_test')) == 206
    print('Mean of test data:', statistics.mean(read_input('input07_test')))
    print('Mean of task data:', statistics.mean(read_input('input07')))
    assert part2('input07_test') == (168, 5)
    print(part2('input07_test'))
    print(part2('input07'))


if __name__ == '__main__':
    main()
