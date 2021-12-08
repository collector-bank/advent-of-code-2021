# André Fagerberg
# Advent of Code: day 6, part 1 & 2

input = File.readlines('./input.txt').first.split(',').map(&:to_i)
@hash = Hash.new(0)
input.map { |i| @hash[i] += 1 }

def start(days)
  days.times do
    tmp = @hash[0]

    (0..7).map { |i| @hash[i] = @hash[i + 1] }
    @hash[6] += tmp
    @hash[8] = tmp
  end
  @hash.values.sum
end

puts start(256)
