#!/usr/bin/env ruby

MOVES = [:naught, :cross]
BOARD_SIZE = 3
CELLS = BOARD_SIZE ** 2
MAGIC_SUM = 15
MAGIC_SQUARE = [8,1,6,3,5,7,4,9,2]
SYMBOLS = [:naught, :cross]

Game = Struct.new(:board, :next_symbol, :winner)

def start(first_player)
  return nil if !MOVES.include?(first_player)
  Game.new(new_board, first_player, nil)
end

def new_board
  (1..CELLS).inject([]) do |arr|
    arr + [:empty]
  end
end

def finished?(game)
  return true unless game.winner.nil?
  return true if board_full?(game.board)
  false
end

def board_full?(board)
  board.select { |s| s != :empty }.count == CELLS
end

# Position is a natural number between 1 and 9 inclusive
# representing the square we'd like to place a piece. 1
# is top left, 9 is bottom right.
def move(game, position)
  index = position - 1
  target = game.board[index]

  if target.nil? || target != :empty
    self
  else
    new_board = game.board.clone
    new_board[index] = game.next_symbol
    new_symbol = game.next_symbol == :naught ? :cross : :naught
    Game.new(new_board, new_symbol, winner(new_board))
  end
end

def draw(board)
  bars = '-' * (3 * BOARD_SIZE + (BOARD_SIZE - 1))
  intercalate(rows(board).map { |r| draw_row(r) }, bars).join("\n")
end

def winner(board)
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

def draw_row(row)
  ' ' + row.map { |c| "#{draw_symbol(c)}" }.join(' | ')
end

def draw_symbol(s)
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

def rows(board)
  board.each_slice(BOARD_SIZE).to_a
end

def intercalate(l, e)
  l.inject([]) { |a, x| a << x << e }[0..-2]
end

##########################################
# HERE'S WHERE WE GET REFERENTIALLY OPAQUE
##########################################

puts `clear`
puts "Let's play!"
gets
puts `clear`
game = start(MOVES.sample)
until finished?(game)
  puts draw(game.board)
  puts
  puts "It's #{game.next_symbol}'s move."
  puts "move (1-9): "
  move = gets.chomp
  puts `clear`
  if move =~ /[1-9]/
    game = move(game, move.to_i)
  else
    puts "'#{move}' is not a valid move. Try 1-9."
  end
end

puts draw(game.board)
puts
puts "Game over!"
puts "Congratulations #{game.winner}!!!" if game.winner
gets

