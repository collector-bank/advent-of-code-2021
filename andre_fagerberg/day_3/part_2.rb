# André Fagerberg
# Advent of Code: day 3, part 2

input = File.readlines('./input.txt', chomp: true)

def calc_oxygen_rate(input, bit = 0)
  bit_group = []

  input.map { |x| bit_group << x[bit] }
  most_significant = most_significant_bit(bit_group)
  input = input.select { |x| x[bit] == most_significant }
  bit += 1

  return input.first if input.size == 1

  calc_oxygen_rate(input, bit)
end

def calc_scrubber_rate(input, bit = 0)
  bit_group = []

  input.map { |x| bit_group << x[bit] }
  least_significant = least_significant_bit(bit_group)
  input = input.select { |x| x[bit] == least_significant }
  bit += 1

  return input.first if input.size == 1

  calc_scrubber_rate(input, bit)
end

def most_significant_bit(bit_group)
  return '1' if bit_group.size == 2

  bit_group.max_by { |i| bit_group.count(i) }
end

def least_significant_bit(bit_group)
  return '0' if bit_group.size == 2

  bit_group.min_by { |i| bit_group.count(i) }
end

oxygen = calc_oxygen_rate(input)
scrubber = calc_scrubber_rate(input)

puts "Result: #{oxygen.to_i(2) * scrubber.to_i(2)}"
