(ns crosscram.game
  "Game knowledge.

In this documentation, \"vector\" means any indexed, sequential collection.

A board is a 2-dimensional matrix. Create with #'board, access with
#'location-empty? Addressed by row, then column.

A square on the board is addressed by a vector of [row column], zero-indexed.

A domino is a vector of two squares: [[r0 c] [r1 c]]. Note that the
first coordinate (row) always differs by 1 between the two squares, but
the column coordinate is always the same. That is, every player always
thinks they are playing vertical. (The game engine arranges for this.)
The order of the squares in a vector is not important, but the game engine
will not alter it.

A game history is a vector of dominoes in order of play.

The board contains an alternate view of history. Each cell contains either
the ordinal (from the history vector) of the move that covered that square,
or nil if the square is open.

A gamestate value (which is provided to the bot) is a map of:
:board - a board value, as defined above
:dims - a vector of [row-count column-count]
:history - a history value, as defined above

This will sometimes simply be called a game value.")


;; Implementation details

;; In 2 dimensions:
;; - A board is a 2-level nesting of vectors. The top-level vector contains
;;   the row vectors.
;; - The order of the squares in a domino is not currently important to
;;   the game engine, but the history will contain the same ordering
;;   the bots provide.

;; TODO(timmc:2012-05-23) Should we canonicalize the order of the squares in
;; the domino on receipt from a bot?

