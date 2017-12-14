#!/usr/bin/env ruby

MOVES = [:naught, :cross]
BOARD_SIZE = 3
CELLS = BOARD_SIZE ** 2
MAGIC_SUM = 15
MAGIC_SQUARE = [8,1,6,3,5,7,4,9,2]
SYMBOLS = [:naught, :cross]

Game = Struct.new(:board, :next_symbol, :winner) do
  def self.start
    Game.new(Game.new_board, MOVES.sample, nil)
  end

  def self.new_board
    (1..CELLS).inject([]) do |arr|
      arr + [:empty]
    end
  end

  def finished?
    return true unless winner.nil?
    return true if board.select { |m| m != :empty }.count == CELLS
    false
  end

  # Position is a natural number between 1 and 9 inclusive
  # representing the square we'd like to place a piece. 1
  # is top left, 9 is bottom right.
  def move(position)
    index = position - 1
    target = board[index]

    if target.nil? || target != :empty
      self
    else
      new_board = board.clone
      new_board[index] = next_symbol
      new_symbol = next_symbol == :naught ? :cross : :naught
      Game.new(new_board, new_symbol)
    end
  end

  def draw
    bars = '-' * (3 * BOARD_SIZE + (BOARD_SIZE - 1))
    intercalate(Game.rows(board).map { |r| Game.draw_row(r) }, bars).join("\n")
  end

  # [Move] -> Int -> Maybe Move
  def winner
    winning_pair =
      board.zip(MAGIC_SQUARE).
            select { |(s,m)| SYMBOLS.include?(s) }.
            group_by { |(s,m)| s }.
            select { |s, ms|
              ms.map(&:last).permutation(3).any? { |triples| triples.reduce(&:+) == MAGIC_SUM
            }}.
            first
    winning_pair.nil? ? nil : winning_pair.first
  end

  def self.draw_row(row)
    ' ' + row.map { |c| "#{draw_symbol(c)}" }.join(' | ')
  end

  def self.draw_symbol(s)
    case s
    when :empty
      ' '
    when :cross
      'x'
    when :naught
      'o'
    else
      "INVALID SYMBOL #{s.to_s}"
    end
  end

  def self.rows(board)
    board.each_slice(BOARD_SIZE).to_a
  end

  def with_winner(w)
    Game.new(board, next_symbol, w)
  end
end

def intercalate(l, e)
  l.inject([]) { |a, x| a << x << e }[0..-2]
end

puts `clear`
puts "Let's play!"
gets
puts `clear`
game = Game.start
until game.finished?
  puts game.draw
  puts
  puts "It's #{game.next_symbol}'s move."
  puts "move (1-9): "
  move = gets.chomp
  puts `clear`
  if move =~ /[1-9]/
    game = game.move(move.to_i)
  else
    puts "'#{move}' is not a valid move. Try 1-9."
  end
end

puts game.draw
puts
puts "Game over!"
puts "Congratulations #{game.winner}!!!" if game.winner
gets

