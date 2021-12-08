def main():
    # input = "3,4,3,1,2"
    # input = input.split(",")
    input = open("inputs/day-6.txt", "r").read().split(",")
    fishes = list(map(int,input))

    day = 0
    maxDays = 256

    partOneResult = 0
    partTwoResult = 0

    fishes = [[x, fishes.count(x)] for x in set(fishes)]
    while (day < maxDays):
        fishBabies = CreateFishBabies(fishes)
        ChangeFishesAge(fishes)        
        
        if any(fishBabies):
            fishes.append([fishBabies[0], fishBabies[1]])

        ages = []
        for i in fishes:
            ages.append(i[0])

        seen = {}
        dupes = []

        for x in ages:
            if x not in seen:
                seen[x] = 1
            else:
                if seen[x] == 1:
                    dupes.append(x)

        duplicateAges = []
        for x in dupes:
            duplicateAges = [x for n, x in enumerate(fishes) if x[0] in dupes]
        
        for x in duplicateAges:
            for fish in fishes:
                if fish == x:
                    fishes.remove(fish)
                    break
    
        for i in (duplicateAges):
            addedFish = False
            for fish in fishes:
                if fish[0] == i[0]:
                    fish[1] += i[1]
                    addedFish = True
                    break
            if not addedFish:
                fishes.append(i)
        count = 0

        day += 1
        if (day == 80):
            for i in fishes:
                count += i[1]
            partOneResult = count
            
    for i in fishes:
        count += i[1]
    partTwoResult = count
    
    print(partOneResult)
    print(partTwoResult)

def CreateFishBabies(fishes):
    fishBabies = []
    fishesWithZeros = list(x for x in fishes if x[0] < 1)
    if any(fishesWithZeros):
        newFishes = [8, (fishesWithZeros[0])[1]]
        fishBabies += newFishes

    return fishBabies   

def ChangeFishesAge(fishes):
    for i in (fishes):
        if (i[0] == 0):
            i[0] = 6
        else:
            i[0] -= 1 

if __name__ == "__main__":
    main()