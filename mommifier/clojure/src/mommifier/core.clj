(ns mommifier.core
  (:require [clojure.string :as s])
  (:gen-class))

(def vowels (char-array "aeiou"))

(defn vowel? [c] (some #(= % c) vowels))

(defn vowel-again? [s c] (and (vowel? (last s)) (vowel? c)))

(defn append-unless [pred coll item]
  (if (pred coll item) coll (conj coll item)))

(defn mommy [c] (if (vowel? c) "mommy" (str c)))

(defn collapse-vowels [cs]
  (reduce (partial append-unless vowel-again?) [] cs))

(defn count' [pred coll] (count (filter pred coll)))

(defn should-mommify? [s]
  (and
    (not (empty? s))
    (> (/ (count' vowel? s) (count s)) 0.3)))

(def mommify' (comp (partial s/join "") (partial map mommy) collapse-vowels char-array))

(defn mommify [s]
  (if (should-mommify? s) (mommify' s) s))

(defn -main [] (println "Hello, World!"))
