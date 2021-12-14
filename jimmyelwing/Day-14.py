from collections import Counter


def main():
    # inputs = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"
    inputs = open("inputs/day-14.txt", "r").read()

    result1 = PartOne(inputs)
    result2 = PartTwo(inputs)

    print("Part one", result1)  # 1588 / 3247
    print("Part two", result2)  # 2188189693529 / 4110568157153


def PartOne(inputs) -> int:
    polymeterTemplate, *pairInsertions = inputs.splitlines()
    pairInsertions = list(filter(None, pairInsertions))

    newString = polymeterTemplate
    for _ in range(0, 10):
        newString = GetString(newString, pairInsertions)

    result = Counter(newString)
    mostFrequentCount, lessFrequentCount = max(result.values()), min(result.values())

    return mostFrequentCount - lessFrequentCount


def PartTwo(inputs) -> int:
    polymeterTemplate, *pairInsertions = inputs.splitlines()
    pairInsertions = list(filter(None, pairInsertions))

    twoLetterCharacters = Counter()
    twoLetterString = ""
    for i in polymeterTemplate:
        twoLetterString += i
        if len(twoLetterString) == 2:
            twoLetterCharacters[twoLetterString] += 1
            twoLetterString = str.replace(twoLetterString[1], twoLetterString, "")

    inserts = {}
    for i in pairInsertions:
        pair, insert = i.split(" -> ")
        inserts[pair] = insert

    individualCharacters = Counter()
    for i in polymeterTemplate:
        individualCharacters[i] += 1

    for i in range(0, 40):
        twoLetterCharacters = GetCount(twoLetterCharacters, inserts, individualCharacters)

    mostFrequentCount, leastFrequentCount = max(individualCharacters.values()), min(individualCharacters.values())

    return mostFrequentCount - leastFrequentCount


def GetString(polymeterTemplate, pairInsertions) -> str:
    newPosition = 0
    newString = ""
    string = ""
    
    for _ in polymeterTemplate:
        letters = polymeterTemplate[newPosition:newPosition+2]
        for i in pairInsertions:
            letters2, insert = i.split(" -> ")
            if letters2 == letters:
                if string == "":
                    newString = letters[:1] + insert + letters[-1]
                else:
                    newString += insert + letters[-1]
                string = newString
        newPosition += 1

    return string


def GetCount(twoLetterCharacters, inserts, individualCharacters) -> int:
    newCharacters = Counter()

    for letters in twoLetterCharacters:
        if letters in inserts:
            countToAdd = twoLetterCharacters[letters]
            letterToAdd = inserts[letters]
            newCharacters[letters[0] + letterToAdd] += countToAdd
            newCharacters[letterToAdd + letters[1]] += countToAdd
            individualCharacters[letterToAdd] += countToAdd
        else:
            newCharacters[letters] = twoLetterCharacters[letters]

    return newCharacters


if __name__ == "__main__":
    main()
