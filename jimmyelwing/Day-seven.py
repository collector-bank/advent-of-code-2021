def main():
    # testInputs = "16,1,2,0,4,2,7,1,2,14".split(",")
    # inputs = list(map(int, testInputs))

    inputs = open("inputs/day-seven.txt", "r").read().split(",")
    inputs = list(map(int, inputs))

    inputs.sort()

    print("Getting results for part one")
    resultPartOne = PartOne(inputs)

    print("Getting results for part two")
    resultPartTwo = PartTwo(inputs)

    print(resultPartOne) # 37 / 325528
    print(resultPartTwo) # 168 / 85015836

def CalculateFuel(start, end):
    fuel = 0
    total = 0
    for _ in list(range(start, end)):
        fuel += 1
        total += fuel
    return total

def PartOne(inputs):
    summary = {}

    for i in range(inputs[0], inputs[-1]):
        count = i
        total = 0
        for j in inputs:
            total += abs(j - count)
        
        summary[count] = total

    moves = list(summary.values())
    moves.sort()
    leastMove = moves[0]

    return leastMove

def PartTwo(inputs):
    totalFuelUsed = {}

    for i in range(inputs[0], inputs[-1]):
        count = i
        total = 0
        for j in inputs:
            start, end = min(count, j), max(count, j)
            fuel = CalculateFuel(start,end)
            total += fuel
        
        if any(totalFuelUsed):
            if list(totalFuelUsed.values())[-1] < total:
                break

        totalFuelUsed[count] = total
    
    totalFuelList = list(totalFuelUsed.values())
    totalFuelList.sort()
    totalFuel = totalFuelList[0]

    return totalFuel

if __name__ == "__main__":
    main()
