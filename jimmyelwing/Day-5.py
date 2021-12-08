def main():
    inputs = open("inputs/day-5.txt", "r").read().split("\n")
    #inputs = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"
    #inputs = inputs.split("\n")
    inputsWithoutWhitespace = list(filter(lambda a: a.strip, inputs))

    lines = []

    for line in inputsWithoutWhitespace:
        x, y = line.split(" -> ")
        lines.append(Line(x, y))

    markedLines = dict()

    for line in lines:
        if line.IsVertical() or line.IsHorizontal():
            positions = line.GetAllPositionsOfLine()
            for x, y in positions:
                AddMarkedLinesToDictionary(markedLines, (x, y))

    partOneAnswer = CountMarkedLines(markedLines)
    markedLines.clear()

    for line in lines:
        positions = line.GetAllPositionsOfLine()
        for x, y in positions:
            AddMarkedLinesToDictionary(markedLines, (x, y))

    partTwoAnswer = CountMarkedLines(markedLines)
    print(partOneAnswer) # 6005
    print(partTwoAnswer) # 23864


def AddMarkedLinesToDictionary(dictionary, xy):
    (x, y) = xy
    if (x, y) in dictionary:
        dictionary[x, y] += 1
    else:
        dictionary[(x, y)] = 1


def CountMarkedLines(markedLines):
    count = 0
    for key, value in markedLines.items():
        if (value > 1):
            count += 1
    return count


class Line:
    def __init__(self, start, end):
        self.Start = Position(start.split(",")[0], start.split(",")[1])
        self.End = Position(end.split(",")[0], end.split(",")[1])

    def IsHorizontal(self):
        if (self.Start.y == self.End.y):
            return True
        return False

    def IsVertical(self):
        if (self.Start.x == self.End.x):
            return True
        return False

    def GetAllPositionsOfLine(self):
        if (self.IsHorizontal()):
            start, end = min(self.Start.x, self.End.x), max(
                self.Start.x, self.End.x)
            return [(allNumbers, self.Start.y) for allNumbers in Range(start, end)]
        elif (self.IsVertical()):
            start, end = min(self.Start.y, self.End.y), max(
                self.Start.y, self.End.y)
            return [(self.Start.x, allNumbers) for allNumbers in Range(start, end)]
        else:
            x_step = -1 if self.Start.x > self.End.x else 1
            y_step = -1 if self.Start.y > self.End.y else 1

            diagonal = zip(range(self.Start.x, self.End.x + x_step, x_step),
                           range(self.Start.y, self.End.y + y_step, y_step))
            positions = []
            for x, y in diagonal:
                positions.append((x, y))
            return positions


class Position:
    def __init__(self, x, y):
        self.x = int(x)
        self.y = int(y)


def Range(start, end):
    return range(start, end + 1)


if __name__ == "__main__":
    main()
