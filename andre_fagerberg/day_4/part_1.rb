# André Fagerberg
# Advent of Code: day 4, part 1

@input = File.readlines('./input.txt', chomp: true)
@draw_nums = @input.first.split(',').map(&:to_i)

def build_boards
  boards = []
  @input[1..-1].reject(&:empty?).each_slice(5) do |board|
    boards << board.map(&:split).flatten.map(&:to_i)
  end
  boards
end

def mark_boards(boards, num, winning_board)
  boards.each_with_index do |board, index|
    next if winning_board.include?(index)

    number_index = board.find_index(num)
    boards[index][number_index] = nil unless number_index.nil?
  end
end

def winning_combination?(board)
  board_row_col = board.each_slice(5).to_a
  return true if board_row_col.any? { |row| row.compact.empty? }
  return true if board_row_col.transpose.any? { |col| col.compact.empty? }

  false
end

def winner_index(boards)
  boards.find_index do |board|
    winning_combination?(board)
  end
end

def start_game(all_boards)
  @draw_nums.each do |num|
    winning_board = []
    mark_boards(all_boards, num, winning_board)
    winner_board = winner_index(all_boards)

    return all_boards[winner_board].compact.sum * num if winner_board
  end
end

puts "Result: #{start_game(build_boards)}"
