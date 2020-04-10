(ns mommifier.core
  (:require [clojure.string :as s])
  (:gen-class))

(def vowels (char-array "aeiou"))

(defn vowel? [c] (some #{c} vowels))

(defn vowel-again? [s c] (and (vowel? (last s)) (vowel? c)))

(defn append-unless [pred coll item]
  (if (pred coll item) coll (conj coll item)))

(defn mommy [c] (if (vowel? c) "mommy" (str c)))

(def collapse-vowels
  (partial reduce (partial append-unless vowel-again?) []))

(def count-by (comp count filter))

(def vowel-ratio (comp (partial reduce /) (juxt (partial count-by vowel?) count)))

(defn should-mommify? [s]
  (and (not-empty s) (> (vowel-ratio s) 0.3)))

(def mommify' (comp (partial s/join "") (partial map mommy) collapse-vowels char-array))

(defn mommify [s]
  (if (should-mommify? s) (mommify' s) s))

(defn -main [] (println "Hello, World!"))
