def main():
    # inputs = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
    # inputs = inputs.split("\n")

    inputs = open("inputs/day-9.txt", "r").read().split("\n")

    print(PartOne(inputs))  # 15 / 417
    print(PartTwo(inputs))  # 1134 / 1148965


def PartOne(heightMap):
    heightMap = AddPositionsToHeightMap(heightMap)
    lowPoints = GetLowPoints(heightMap)
    return RiskLevel(lowPoints)


def PartTwo(heightMap):
    heightMap = AddPositionsToHeightMap(heightMap)
    basins = GetBasins(heightMap)
    sortedBasins = sorted(basins, key=len)

    return len(sortedBasins[-1]) * len(sortedBasins[-2]) * len(sortedBasins[-3])


def AddPositionsToHeightMap(heightMap):
    heightMapWithPositions = {}
    countRow = 0
    for row in heightMap:
        heightMapWithPositions.update(GetPositionsOfRow(row, countRow))
        countRow += 1
    return heightMapWithPositions


def GetPositionsOfRow(row, rowNumber):
    positions = {}
    countx = 0
    for number in row:
        x = countx
        positions[x, rowNumber] = int(number)
        countx += 1
    return positions


def GetLowPoints(heightMap):
    lowPoints = []

    for location in heightMap:
        locationHeight = int(heightMap[location])

        adjacentLocations = GetAdjacentLocations(location, heightMap)
        if any(adjacentLocations):
            locationIsLower = False
            for adjacentHeight in adjacentLocations.values():
                if locationHeight > adjacentHeight:
                    locationIsLower = False
                    break
                elif locationHeight < adjacentHeight:
                    locationIsLower = True
            if locationIsLower:
                lowPoints.append(locationHeight)

    return lowPoints


def GetBasins(heightMap):
    uniqueBasins = []
    basins = {}
    locationsToCheck = {}
    locationsAlreadyAdded = {}
    for location in heightMap:
        height = heightMap[location]
        if height == 9:
            continue
        adjacentLocations = GetAdjacentLocations(location, heightMap)
        locationsToCheck.update(GetLocationsToCheck(
            heightMap, locationsAlreadyAdded, adjacentLocations))
        if any(locationsToCheck):
            basins[location] = height
            locationsAlreadyAdded[location] = height

        while any(locationsToCheck):
            for location in list(locationsToCheck):
                height = heightMap[location]
                adjacentLocations = GetAdjacentLocations(location, heightMap)
                locationsToCheck.update(GetLocationsToCheck(
                    heightMap, locationsAlreadyAdded, adjacentLocations))
                basins[location] = heightMap[location]
                locationsAlreadyAdded[location] = heightMap[location]
                locationsToCheck.pop(location)

        if any(basins):
            uniqueBasins.append(list(basins))
        basins.clear()

    return uniqueBasins


def GetAdjacentLocations(location, heightMap):
    adjacentLocations = {}
    numberOfColumns, numberOfRows = max(heightMap)

    x, y = location
    right = x + 1, y
    left = x - 1, y
    down = x, y + 1
    up = x, y - 1

    if FirstColumn(x):
        if FirstRow(y):
            adjacentLocations = GetLocations(heightMap, (down, right))
        elif MiddleRow(y, numberOfRows):
            adjacentLocations = GetLocations(heightMap, (down, right, up))
        elif LastRow(y, numberOfRows):
            adjacentLocations = GetLocations(heightMap, (right, up))
    elif MiddleColumn(x, numberOfColumns):
        if FirstRow(y):
            adjacentLocations = GetLocations(heightMap, (down, right, left))
        elif MiddleRow(y, numberOfRows):
            adjacentLocations = GetLocations(heightMap, (up, right, left, down))
        elif LastRow(y, numberOfRows):
            adjacentLocations = GetLocations(heightMap, (right, left, up))
    elif LastColumn(x, numberOfColumns):
        if FirstRow(y):
            adjacentLocations = GetLocations(heightMap, (down, left))
        elif MiddleRow(y, numberOfRows):
            adjacentLocations = GetLocations(heightMap, (down, left, up))
        elif LastRow(y, numberOfRows):
            adjacentLocations = GetLocations(heightMap, (left, up))

    return adjacentLocations


def GetLocations(heightMap, positions):
    locations = {}
    for i in positions:
        locations[i] = heightMap[i]
    return locations


def RiskLevel(lowPoints):
    return sum([i + 1 for i in lowPoints])


def GetLocationsToCheck(heightMap, locationsAlreadyAdded, adjacentLocations):
    locationsToCheck = {}
    for adjacentLocation in adjacentLocations:
        adjacentHeight = heightMap[adjacentLocation]
        if adjacentHeight != 9 and adjacentLocation not in locationsAlreadyAdded:
            locationsToCheck[adjacentLocation] = adjacentHeight
    return locationsToCheck


def FirstRow(y):
    return y == 0


def MiddleRow(y, numberOfRows):
    return y > 0 and y < numberOfRows


def LastRow(y, numberOfRows):
    return y == numberOfRows


def FirstColumn(x):
    return x == 0


def MiddleColumn(x, numberOfColumns):
    return x > 0 and x < numberOfColumns


def LastColumn(x, numberOfColumns):
    return x == numberOfColumns


if __name__ == "__main__":
    main()
