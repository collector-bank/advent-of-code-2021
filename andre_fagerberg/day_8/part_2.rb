# André Fagerberg
# Advent of Code: day 8, part 2

input = File.readlines('./input.txt', chomp: true).map { |x| x.split(' | ').map { |y| y.split(' ') } }

def deduction(signals)
  input = signals.map { |c| c.chars.sort }
  hash = {}

  hash[1] = input.find { |x| x.size == 2 }
  hash[4] = input.find { |x| x.size == 4 }
  hash[7] = input.find { |x| x.size == 3 }
  hash[8] = input.find { |x| x.size == 7 }
  hash[3] = input.find { |x| x.size == 5 && (x - hash[1]).size == 3 }
  hash[6] = input.find { |x| x.size == 6 && (x - hash[1]).size == 5 }
  hash[9] = input.find { |x| x.size == 6 && (x - hash[4]).size == 2 }
  hash[2] = input.find { |x| x.size == 5 && (x - hash[9]).size == 1 }
  hash[5] = input.find { |x| x.size == 5 && x != hash[2] && x != hash[3] }
  hash[0] = input.find { |x| x.size == 6 && x != hash[6] && x != hash[9] }

  hash
end

def start(input)
  sum = 0
  input.each do |line|
    signals = line[0]
    hash = deduction(signals)
    output = line[1].map { |c| c.chars.sort }
    sum += output.map { |int| hash.key(int) }.join.to_i
  end
  sum
end

puts start(input)
