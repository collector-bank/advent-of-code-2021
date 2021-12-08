# André Fagerberg
# Advent of Code: day 6, part 1
# Fulkod stress resultat :)
require 'byebug'

input = File.readlines('./test.txt').first.split(',').map(&:to_i)

def num_new_fish(num)
  Array.new(num, 8)
end

def transform(input, new_fish)
  input.each_with_index do |_, i|
    if input[i] == 0
      input[i] = 7
    end
    input[i] -= 1
  end
  input += new_fish unless new_fish == 0
  puts input.to_s
  input
end

def start(input, days)
  new_fish = 0
  days.times do
    input = transform(input, new_fish)
    new_fish = num_new_fish(input.count(0))
  end
  input.size
end

puts start(input, 80)
