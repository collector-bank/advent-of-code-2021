# André Fagerberg
# Advent of Code: day 7, part 2

input = File.readlines('./input.txt').first.split(',').map(&:to_i)

def median(input)
  (input.reduce(:+) + 1) / input.size
end

def fuel_consumption(position, target)
  steps = (position - target).abs
  steps.times.map { |x| x + 1 }.sum
end

def start(input)
  target = median(input)
  input.map { |pos| fuel_consumption(pos, target) }.sum
end

puts start(input)
