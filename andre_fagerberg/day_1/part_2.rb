# André Fagerberg
# Advent of Code: day 1, part 2

BLOCK_SIZE = 3
@input = File.readlines('./input.txt', chomp: true).map(&:to_i)
@result = 0

def generate_block(index)
  block = []

  [*0..BLOCK_SIZE - 1].to_a.each do |n|
    block << @input[index + n]
  end
  block.compact
end

def compare_depth(current_block, next_block)
  @result += 1 if current_block.sum < next_block.sum
end

@input.each_with_index do |_, i|
  current_block = generate_block(i)
  next_block = generate_block(i + 1)

  compare_depth(current_block, next_block)
end

puts "Result: #{@result}"
