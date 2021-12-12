# André Fagerberg
# Advent of Code: day 8, part 2

input = File.readlines('./input.txt', chomp: true).map { |x| x.split(' | ') }

def start(input)
  occurances = 0

  input.each do |entry|
    entry[1].split(' ').each do |sig|
      occurances += 1 if [2, 4, 3, 7].include? sig.size
    end
  end
  occurances
end

puts start(input)
