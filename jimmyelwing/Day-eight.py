def main():
    inputs = open("inputs/day-eight.txt", "r").read().split("\n")

    # inputs = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
    # inputs = inputs.split("\n")

    inputsOutputs = []

    for i in inputs:
        inputs, outputs = i.split("|")
        inputs_and_outputs = (inputs.strip(), outputs.strip())

        inputsOutputs.append(Entry(inputs_and_outputs))

    print(PartOne(inputsOutputs))  # 530
    print(PartTwo(inputsOutputs))  # 1051087


def PartOne(inputs_and_outputs):
    totalSum = 0
    for x in inputs_and_outputs:
        for y in x.outputs.split():
            if len(y) in [2, 3, 4, 7]:
                totalSum += 1

    return totalSum


def PartTwo(inputsOutputs):
    for i in inputsOutputs:
        i.Decode()

    partTwoResult = 0
    for i in inputsOutputs:
        partTwoResult += int(i.GetResult())

    return partTwoResult


class Entry():
    def __init__(self, inputs_and_outputs):
        inputs, outputs = inputs_and_outputs
        self.inputs = inputs
        self.outputs = outputs
        self.codes = {}
        self.decode_sheet = {}

    def Decode(self):
        decoding = {}

        individualInputs = self.inputs.split()

        for i in individualInputs:
            if len(i) == 2:
                self.codes[1] = i
            elif len(i) == 4:
                self.codes[4] = i
            elif len(i) == 3:
                self.codes[7] = i
            elif len(i) == 7:
                self.codes[8] = i

        for i in individualInputs:
            for x in i:
                if x in decoding:
                    decoding[x] += 1
                else:
                    decoding[x] = 1

        for key, value in decoding.items():
            if 4 == value:
                self.decode_sheet[5] = key
            elif 6 == value:
                self.decode_sheet[2] = key
            elif 9 == value:
                self.decode_sheet[6] = key

        self.decode_sheet[1] = GetNumberForDecodeSheet(
            self.codes[7], self.codes[1])
        self.decode_sheet[3] = GetNumberForDecodeSheet(
            self.codes[1], self.decode_sheet[6])
        self.decode_sheet[4] = GetNumberForDecodeSheet(
            self.codes[4], (self.decode_sheet[2], self.decode_sheet[3], self.decode_sheet[6]))
        self.decode_sheet[7] = GetNumberForDecodeSheet(
            self.codes[8], (self.decode_sheet[1], self.decode_sheet[2], self.decode_sheet[3], self.decode_sheet[4], self.decode_sheet[5], self.decode_sheet[6]))

        self.SetCodeFromDecodeSheet(0, (1, 2, 3, 5, 6, 7))
        self.SetCodeFromDecodeSheet(1, (3, 6))
        self.SetCodeFromDecodeSheet(2, (1, 3, 4, 5, 7))
        self.SetCodeFromDecodeSheet(3, (1, 3, 4, 6, 7))
        self.SetCodeFromDecodeSheet(4, (2, 3, 4, 6))
        self.SetCodeFromDecodeSheet(5, (1, 2, 4, 6, 7))
        self.SetCodeFromDecodeSheet(6, (1, 2, 4, 5, 6, 7))
        self.SetCodeFromDecodeSheet(7, (1, 3, 6))
        self.SetCodeFromDecodeSheet(8, (1, 2, 3, 4, 5, 6, 7))
        self.SetCodeFromDecodeSheet(9, (1, 2, 3, 4, 6, 7))

    def GetResult(self):
        result = ""
        for i in self.outputs.split(" "):
            sortedi = ''.join(sorted(i))

            if sortedi == ''.join(sorted(self.codes[0])):
                result += "0"
            elif sortedi == ''.join(sorted(self.codes[1])):
                result += "1"
            elif sortedi == ''.join(sorted(self.codes[2])):
                result += "2"
            elif sortedi == ''.join(sorted(self.codes[3])):
                result += "3"
            elif sortedi == ''.join(sorted(self.codes[4])):
                result += "4"
            elif sortedi == ''.join(sorted(self.codes[5])):
                result += "5"
            elif sortedi == ''.join(sorted(self.codes[6])):
                result += "6"
            elif sortedi == ''.join(sorted(self.codes[7])):
                result += "7"
            elif sortedi == ''.join(sorted(self.codes[8])):
                result += "8"
            elif sortedi == ''.join(sorted(self.codes[9])):
                result += "9"

        return result

    def SetCodeFromDecodeSheet(self, codeNumber, decode_sheets):
        code = ""
        for number in decode_sheets:
            code += str(self.decode_sheet[number])
        self.codes[codeNumber] = code


def GetNumberForDecodeSheet(code, decode_sheet):
    for letter in code:
        if letter not in decode_sheet:
            return letter


if __name__ == ("__main__"):
    main()
