from collections import defaultdict

def main():
    #inputs = "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5"
    inputs = open("inputs/day-13.txt", "r").read()
    rows = inputs.splitlines()
    rows = list(filter(None, rows))
        
    dots = defaultdict(lambda: ".")
    instructions = []
    for i in rows:
        if i.startswith("fold along"):
            instructions.append(i)
            continue
        x, y = map(int, i.split(","))
        dots[x,y] = "#"
    

    maxColumn = 0
    maxRow = 0
    for y in dots:
        if y[0] > maxColumn:
            maxColumn = y[0]
        if y[1] > maxRow:
            maxRow = y[1]

    instructions = [i.replace("fold along ", "") for i in instructions]
    
    foldNumberAndVisible = defaultdict()
    count = 0
    maxColumn = 0
    maxRow = 0
    maxColumn, maxRow = GetMaxColumnAndMaxRow(dots, maxColumn, maxRow)
    
    for i in instructions:
        print("")
        # PrintPaper(dots, maxColumn, maxRow)
        maxColumn, maxRow = FoldPaper(dots, i, maxColumn, maxRow)
        foldNumberAndVisible[count] = len(dots)
        count += 1
    
    print("Part one: ", foldNumberAndVisible[0])
    print("Part two: ") 
    PrintPaper(dots, maxColumn, maxRow)
    # PrintPaper(dots, maxColumn, maxRow)

def GetMaxColumnAndMaxRow(dots, maxColumn, maxRow):
    for y in dots:
        if y[0] > maxColumn:
            maxColumn = y[0]
        if y[1] > maxRow:
            maxRow = y[1]
    return maxColumn,maxRow

def PrintPaper(dots, maxColumn, maxRow):
    print()
    for y in range(maxRow + 1):
        rowString = ""
        for x in range(maxColumn + 1):
            currentValue = dots[x,y]
            if currentValue != "#":
                rowString += "."
                continue
            rowString += currentValue
        print(rowString)
    print()

def FoldPaper(dots, instruction, maxColumn, maxRow):
    position,number = instruction.split("=")
    number = int(number)
    if position == "y":
        test = [i for i in dots if i[1] > number and dots[i] == '#']
        for i in test:
            y = i[1]
            y = maxRow - i[1]
            dots.pop(i)
            dots[i[0],y] = "#"
        maxRow = number - 1
    if position == "x":
        test = [i for i in dots if i[0] > number and dots[i] == '#']
        for i in test:
            x = i[0]
            x = maxColumn - i[0]
            dots.pop(i)
            dots[x,i[1]] = "#"
        maxColumn = number - 1
        
    return maxColumn, maxRow

if __name__ == "__main__":
    main()