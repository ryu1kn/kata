(defn fizzbuzz [n]
  (cond
    (= (mod n 15) 0) "FizzBuzz"
    (= (mod n 3) 0) "Fizz"
    (= (mod n 5) 0) "Buzz"
    :else (str n)))

(assert (= (fizzbuzz 1) "1") "Return the same number")
(assert (= (fizzbuzz 3) "Fizz") "Return \"Fizz\" for x3 numbers")
(assert (= (fizzbuzz 5) "Buzz") "Return \"Buzz\" for x5 numbers")
(assert (= (fizzbuzz 15) "FizzBuzz") "Return \"Buzz\" for x15 numbers")

