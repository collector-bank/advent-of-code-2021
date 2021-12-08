# André Fagerberg
# Advent of Code: day 7, part 1

input = File.readlines('./input.txt').first.split(',').map(&:to_i)

def median(input)
  input.sort[input.size / 2 - 1]
end

def start(input)
  target = median(input)

  input.map { |i| (i - target).abs }.sum
end

puts start(input)
