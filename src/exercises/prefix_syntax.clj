(ns exercises.prefix-syntax)

; ex1
(+ 2 2)
(+ 2 2 2 3)
(* 2 (+ 3 (- 4 (* 2 2))))

; ex2
(mod 5 (inc 2))

; ex3
(subs "abcdefg"
	  (count "hij")
	  (+ 3 (mod 6 4)))
