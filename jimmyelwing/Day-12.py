import collections
Counter = collections.Counter


def Main():
    inputs = open("inputs\day-12.txt", "r").read()
    # inputs = "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW"

    caveConnections = inputs.splitlines()

    # defaultdict(list) is a dictionary with a list as default.
    paths = collections.defaultdict(list)

    for row in caveConnections:  # Add all possible connections to dictionary
        start, end = row.split("-")
        paths[start].append(end)
        paths[end].append(start)

    pathsToEnd = GetPaths(["start"], paths, 1)
    print(pathsToEnd)  # 5104
    pathsToEnd = GetPaths(["start"], paths, 2)
    print(pathsToEnd)  # 149220


def GetPaths(visitedCaves, paths, part):
    numberOfPaths = 0
    lastCaveAdded = visitedCaves[-1]
    for cave in paths[lastCaveAdded]:
        if End(cave):
            numberOfPaths += 1
        elif part == 1:
            if Large(cave) or (Small(cave) and not AlreadyVisited(cave, visitedCaves)):
                newPath = visitedCaves + [cave]
                numberOfPaths += GetPaths(newPath, paths, part)
        elif part == 2:
            visitsToSmallCaves = Counter(filter(Small, visitedCaves))
            if not Start(cave) and (Large(cave) or Small(cave) and (not VisitedTwice(visitsToSmallCaves) or not AlreadyVisited(cave, visitedCaves))):
                newPath = visitedCaves + [cave]
                numberOfPaths += GetPaths(newPath, paths, part)

    return numberOfPaths


def Large(cave):
    return cave.isupper()


def Small(cave):
    return cave.islower()


def AlreadyVisited(connection, currentConnections):
    return connection in currentConnections


def VisitedTwice(visits):
    return max(visits.values()) == 2


def End(position):
    return position == "end"


def Start(position):
    return position == "start"


if __name__ == "__main__":
    Main()
