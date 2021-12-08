# André Fagerberg
# Advent of Code: day 3, part 1

@bit = 0
@gamma = ''
@epsilon = ''

def calc_rate(bit_group)
  @gamma[@bit] = bit_group.max_by { |i| bit_group.count(i) }
  @epsilon[@bit] = bit_group.min_by { |i| bit_group.count(i) }
end

input = File.readlines('./input.txt', chomp: true)
num_length = input.first.size

while @bit < num_length
  bit_group = []
  input.map { |x| bit_group << x[@bit] }
  calc_rate(bit_group)
  @bit += 1
end

puts "Result: #{@gamma.to_i(2) * @epsilon.to_i(2)}"
