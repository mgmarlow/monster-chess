;; Monster Chess Notation (MCN)
;;
;; From left-to-right, space-delimited:
;; - # columns
;; - # rows
;; - FEN-style board state
;;   - "x" is a impassable wall
;;   - lowercase letters are black pieces
;;   - uppercase letters are white pieces
;; - Par score
;;
;; Example:
;; "5 5 1q3/p4/5/5/3B1 2"

{
 :boardstr-to-table (fn boardstr-to-table [boardstr]
                      (let [board []]
                        (each [row (string.gmatch boardstr "[^/]+")]
                          (let [pieces []]
                            (each [char (string.gmatch row ".")]
                              (if (string.match char "[0-9]")
                                  (for [i 1 (tonumber char)]
                                    (table.insert pieces "."))
                                  (table.insert pieces char)))
                            (table.insert board pieces)))
                        board))

 :string-to-table (fn string-to-table [mcn]
                    (let [parts (icollect [s (string.gmatch mcn "[^ ]+")] s)
                          cols (tonumber (. parts 1))
                          rows (tonumber (. parts 2))
                          boardstr (. parts 3)
                          par (tonumber (. parts 4))]
                      { :cols cols
                        :rows rows
                        :board (boardstr-to-table boardstr)
                        :par par }))

 :board-to-string (fn board-to-string [board]
                    (var result "")
                    (each [ri row (ipairs board)]
                      (var counter 0)
                      (each [_ char (ipairs row)]
                        (if (= char ".")
                            (set counter (+ counter 1))
                            (do
                              (when (> counter 0)
                                (set result (.. result counter))
                                (set counter 0))
                              (set result (.. result char)))))
                      (when (> counter 0)
                        (set result (.. result counter))
                        (set counter 0))
                      (when (not (= ri (length board)))
                        (set result (.. result "/"))))
                    result)
 }
