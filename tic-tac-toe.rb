#!/usr/bin/env ruby

module TicTacToe
  BOARD_SIZE = 3
  CELLS = BOARD_SIZE ** 2
  MAGIC_SUM = 15
  MAGIC_SQUARE = [8,1,6,3,5,7,4,9,2]
  SYMBOLS = [:naught, :cross]

  Game = Struct.new(:board, :next_symbol, :winner)

  def self.start(first_player)
    return nil if !SYMBOLS.include?(first_player)
    Game.new(new_board, first_player, nil)
  end

  def self.new_board
    (1..CELLS).inject([]) do |arr|
      arr + [:empty]
    end
  end

  def self.finished?(game)
    return true unless game.winner.nil?
    return true if board_full?(game.board)
    false
  end

  def self.board_full?(board)
    board.select { |s| s != :empty }.count == CELLS
  end

  # Position is a natural number between 1 and 9 inclusive
  # representing the square we'd like to place a piece. 1
  # is top left, 9 is bottom right.
  def self.move(game, position)
    index = position - 1
    target = game.board[index]

    if target.nil?
      [game, "Move out of bounds: #{position}"]
    elsif target != :empty
      [game, "Position #{position} is already occupied"]
    else
      new_board = game.board.clone
      new_board[index] = game.next_symbol
      new_symbol = game.next_symbol == :naught ? :cross : :naught
      [Game.new(new_board, new_symbol, winner(new_board)), ""]
    end
  end

  def self.draw(board)
    bars = '-' * (3 * BOARD_SIZE + (BOARD_SIZE - 1))
    intercalate(rows(board).map { |r| draw_row(r) }, bars).join("\n")
  end

  def self.winner(board)
    # If we think of the board as a magic square, if any 3 squares held by the one player
    # sum to the value of the magic square, they have row, column, or diagonal.
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
      raise "Asked to draw invalid symbol #{s.to_s}. This wouldn't happen if we had real sum types"
    end
  end

  def self.rows(board)
    board.each_slice(BOARD_SIZE).to_a
  end

  def self.intercalate(l, e)
    # This uses mutation, but only on a value created within the function, hence
    # it is not observable outside the function and therefore maintains referential
    # transparency.
    l.inject([]) { |a, x| a << x << e }[0..-2]
  end
end

##########################################
# HERE'S WHERE WE GET REFERENTIALLY OPAQUE
##########################################

puts `clear`
puts "Let's play!"
gets
puts `clear`
game = TicTacToe.start(TicTacToe::SYMBOLS.sample)
error = ""
until TicTacToe.finished?(game)
  puts `clear`
  puts error
  puts
  puts TicTacToe.draw(game.board)
  puts
  puts "It's #{game.next_symbol}'s move."
  puts "move (1-9): "
  move = gets.chomp
  if move =~ /^[1-9]$/
    (game, error) = TicTacToe.move(game, move.to_i)
  else
    error = "'#{move}' is not a valid move. Try 1-9."
  end
end

puts `clear`
puts
puts
puts TicTacToe.draw(game.board)
puts
puts "Game over!"
puts "Congratulations #{game.winner}!!!" if game.winner
gets

