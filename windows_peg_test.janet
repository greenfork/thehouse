(def level-grammar
  ~{:space "."
    :block "B"
    :hero "@"
    :door "D"
    :exit-door "d"
    :cell (+ :block :space :hero :door :exit-door)
    :newline (+ "\n" "\r\n" "\r")
    # :row (* (some :cell) (? :newline))
    :row (* (some :cell) (any (if-not :cell 1)))
    :main (some :row)})

(def hallway-ascii ``.......DD.........
.BBBBBB..BBBBBBBB.
.B..............B.
D@..............B.
D................d
.B...............d
.B..............B.
.B..............B.
.BBBBBBB..BBBBBBB.
........DD........``)

(printf "%q" (peg/match level-grammar hallway-ascii))
