(def level-grammar
  ~{:space "."
    :block "B"
    :hero "@"
    :door "D"
    :exit-door "d"
    :cell (+ :block :space :hero :door :exit-door)
    :row (* (some :cell) (? "\n"))
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
........DD........
``)

(printf "%q" (peg/match level-grammar hallway-ascii))
