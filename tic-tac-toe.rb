MOVES = [:naught, :cross]
BOARD_SIZE = 3
MAGIC_SUM = 15
MAGIC_SQUARE = [8,1,6,3,5,7,4,9,2]
SYMBOLS = [:naught, :cross]

def new_board
  (1..BOARD_SIZE**2).inject([]) do |arr|
    arr + [:empty]
  end
end

def draw_board(board)
  bars = '-' * (3 * BOARD_SIZE + (BOARD_SIZE - 1))
  intercalate(rows(board).map { |r| draw_row(r) }, bars).join("\n")
end

def intercalate(l, e)
  l.inject([]) { |a, x| a << x << e }[0..-2]
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

# Position is a natural number between 1 and 9 inclusive
# representing the square we'd like to place a piece. 1
# is top left, 9 is bottom right.
def move(board, symbol, position)
  return board if !MOVES.include?(symbol)

  index = position - 1
  target = board[position]

  if target.nil? || target != :empty
    board
  else
    new_board = board.clone
    new_board[index] = symbol
    new_board
  end
end

def rows(board)
  board.each_slice(BOARD_SIZE).to_a
end

def cols(board)
  rows(board).transpose
end

# [Move] -> Int -> Maybe Move
def won(board)
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

def test_move(b, m, p)
  new_b = move(b, m, p)
  puts draw_board(new_b)
  puts won(new_b)
  new_b
end

def wait
  gets
  puts `clear`
end

b = new_board
b2 = test_move(b, :naught, 1)
wait
b3 = test_move(b2, :naught, 2)
wait
b4 = test_move(b3, :naught, 3)
wait

