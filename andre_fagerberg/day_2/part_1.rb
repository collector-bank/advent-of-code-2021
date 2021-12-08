# André Fagerberg
# Advent of Code: day 2, part 1

@input = File.readlines('./input.txt', chomp: true)
@depth = 0
@position = 0

def calculate_position(val, dir)
  case dir
  when 'forward'
    @position += val
  when 'down'
    @depth += val
  when 'up'
    @depth -= val
  end
end

@input.each do |line|
  direction = line.split(' ')[0]
  value = line.split(' ')[1].to_i

  calculate_position(value, direction)
end

puts "Result: #{@position * @depth}"
