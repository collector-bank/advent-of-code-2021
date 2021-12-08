def main():
    inputs = open("inputs/day-4.txt", "r").read().split("\n")
    drawnNumbersInput = inputs[0].split(",")
    boardInputs = inputs[1:len(inputs)]

    bingoBoards = GetBingoBoards(boardInputs)

    win = True
    winningBoard, drawnNumber = PlayBingo(bingoBoards, drawnNumbersInput, win)
    allNumbersCombined = GetCombinedNumbers(winningBoard)

    print(allNumbersCombined * int(drawnNumber))

    win = False
    loosingBoard, drawnNumber = PlayBingo(bingoBoards, drawnNumbersInput, win)
    allNumbersCombined = GetCombinedNumbers(loosingBoard)

    print(allNumbersCombined * int(drawnNumber))


def GetBingoBoards(boardsInput):
    bingoBoards = []
    boardNumbers = []

    for i in boardsInput:
        if (i != ""):
            boardNumbers += i.split(" ")

    # Remove empty lines
    boardNumbers = list(filter(lambda a: a != "", boardNumbers))

    count = 0
    maxCount = 4
    bingoBoard = BingoBoard()

    boardNumbersInGroupsOfFive = zip(*[iter(boardNumbers)]*5)

    for i in boardNumbersInGroupsOfFive:
        if count == 0:
            bingoBoard.firstRow = list(i)
        elif count == 1:
            bingoBoard.secondRow = list(i)
        elif (count == 2):
            bingoBoard.thirdRow = list(i)
        elif (count == 3):
            bingoBoard.fourthRow = list(i)
        elif (count == 4):
            bingoBoard.fifthRow = list(i)

        if (count == maxCount):
            bingoBoards.append(bingoBoard)
            bingoBoard = BingoBoard()
            count = 0
        else:
            count += 1

    return bingoBoards


def PlayBingo(boards, drawnNumbers, win):
    for number in drawnNumbers:
        MarkNumberOnBingoBoards(number, boards)

        if win:
            winningBoard = GetWinningBoard(boards)
            if not winningBoard:
                continue
            return winningBoard, number

        if not win:
            loosingBoard = GetLoosingBoard(boards)
            if not loosingBoard:
                continue
            return loosingBoard, number


def MarkNumberOnBingoBoards(number, bingoBoards):
    for board in bingoBoards:
        board.MarkNumber(number)


def GetWinningBoard(bingoBoards):
    for board in bingoBoards:
        if board.HasWon():
            return board
    return ""


def GetLoosingBoard(bingoBoards):
    loosingBoards = bingoBoards
    winningBoard = []

    for board in loosingBoards:
        if board.HasWon():
            winningBoard.append(board)
            loosingBoards.remove(board)
    if (len(winningBoard) == 1) and (len(loosingBoards) == 0):
        return winningBoard[0]

    return ""


def GetCombinedNumbers(bingoBoard):
    combinedNumbers = 0

    for number in bingoBoard.firstRow:
        combinedNumbers += GetNumberFromString(number)
    for number in bingoBoard.secondRow:
        combinedNumbers += GetNumberFromString(number)
    for number in bingoBoard.thirdRow:
        combinedNumbers += GetNumberFromString(number)
    for number in bingoBoard.fourthRow:
        combinedNumbers += GetNumberFromString(number)
    for number in bingoBoard.fifthRow:
        combinedNumbers += GetNumberFromString(number)

    return combinedNumbers


def GetNumberFromString(string):
    if string == "":
        return 0
    else:
        return int(string)


class BingoBoard:
    def __init__(self):
        self.firstRow = []
        self.secondRow = []
        self.thirdRow = []
        self.fourthRow = []
        self.fifthRow = []

    def GetAllRows(self):
        return [self.firstRow, self.secondRow, self.thirdRow, self.fourthRow, self.fifthRow]

    def MarkNumber(self, drawnNumber):
        for row in self.GetAllRows():
            MarkRow(row, drawnNumber)

    def HasWon(self):
        for row in self.GetAllRows():
            if RowHasWon(row):
                return True

        columnToCheck = 0
        while columnToCheck < 5:
            if ColumnHasWon(self.firstRow[columnToCheck], self.secondRow[columnToCheck], self.thirdRow[columnToCheck], self.fourthRow[columnToCheck], self.fifthRow[columnToCheck]) == 1:
                return True
            columnToCheck += 1

        return False


def MarkRow(row, drawnNumber):
    for number in row:
        if drawnNumber == number:
            index = row.index(number)
            row[index] = ""


def RowHasWon(row):
    markedNumbers = ""
    for i in row:
        markedNumbers += i
    if markedNumbers == "":
        return True
    return False


def ColumnHasWon(firstRowNumber, secondRowNumber, thirdRowNumber, fourthRowNumber, fifthRowNumber):
    markedNumbers = firstRowNumber + secondRowNumber + thirdRowNumber + fourthRowNumber + fifthRowNumber
    if markedNumbers == "":
        return True
    return False


if __name__ == "__main__":
    main()
