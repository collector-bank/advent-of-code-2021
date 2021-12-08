def main():
    inputs = open("inputs/day-3.txt", "r").read().split("\n")
    partOneResult = PartOne(inputs)
    partTwoResult = PartTwo(inputs)

    print(f"PartOne {partOneResult}")
    print(f"PartTwo {partTwoResult}")

def PartOne(inputs):
    gammaRate = ""
    epsilonRate = ""

    firstRow = 0

    count = 0
    maxCount = len(inputs[0])


    while (count < maxCount):
        for i in inputs:
            for j in i[count]:
                firstRow += int(j)
        halfAmount = len(inputs)
        if (firstRow < halfAmount / 2):
            gammaRate += "0"
            epsilonRate += "1"
        else:
            gammaRate += "1"
            epsilonRate += "0"
        firstRow = 0
        count += 1

    return int(gammaRate, 2) * int(epsilonRate, 2)

def PartTwo(inputs):
    oxygenGeneratorRating = int(GetBit(inputs, 1, 0), 2)
    CO2ScrubberRating = int(GetBit(inputs, 0, 1), 2)
    return oxygenGeneratorRating * CO2ScrubberRating


def GetBit(inputs, oxygen, CO2):

    count = 0
    inputsToRemove = []
    bitsWithOnes = 0
    bitsWithZeros = 0

    while (count < len(inputs[0])):
        if (len(inputs) == 1):
            break
        for i in inputs:
            if (i[count] == "1"):
                bitsWithOnes += 1
            if (i[count] == "0"):
                bitsWithZeros += 1        
        if (oxygen == 1):
            if (bitsWithOnes >= bitsWithZeros):
                for i in inputs:
                    if (i[count] == "0"):
                        inputsToRemove.append(i)
            if (len(inputsToRemove) > 0):
                inputs = [x for x in inputs if (x not in inputsToRemove)]
            
            inputsToRemove.clear
            
            if (bitsWithOnes < bitsWithZeros):
                for i in inputs:
                    if (i[count] == "1"):
                        inputsToRemove.append(i)
                    
                if (len(inputsToRemove) > 0):
                    inputs = [x for x in inputs if (x not in inputsToRemove)]

            bitsWithZeros = 0
            bitsWithOnes = 0
            inputsToRemove = []
            count += 1

        if (CO2 == 1):
            if (bitsWithZeros > bitsWithOnes):
                for i in inputs:
                    if (i[count] == "0"):
                        inputsToRemove.append(i)
                
            if (len(inputsToRemove) > 0):
                inputs = [x for x in inputs if (x not in inputsToRemove)]
            
            inputsToRemove.clear
            
            if (bitsWithZeros <= bitsWithOnes):
                for i in inputs:
                    if (i[count] == "1"):
                        inputsToRemove.append(i)
                
            if (len(inputsToRemove) > 0):
                inputs = [x for x in inputs if (x not in inputsToRemove)]
                    
            bitsWithZeros = 0
            bitsWithOnes = 0
            inputsToRemove = []
            count += 1

    return inputs[0]

if __name__ == "__main__":
    main()
