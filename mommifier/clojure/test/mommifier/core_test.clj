(ns mommifier.core-test
  (:require [clojure.test :refer :all]
            [mommifier.core :refer :all]))

(deftest mommify-test
  (testing "empty"
    (is (= "" (mommify ""))))

  (testing "b"
    (is (= "b" (mommify "b"))))

  (testing "with more than 30% vowels"
    (is (= "mommy" (mommify "a"))))

  (testing "consecutive vowels"
    (is (= "mommy" (mommify "aa"))))

  (testing "with less than 30% vowels"
    (is (= "bash" (mommify "bash"))))
  )
