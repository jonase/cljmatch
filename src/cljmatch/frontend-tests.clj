(ns cljmatch.frontend.tests
  (:use cljmatch.frontend
	clojure.test))

(defmatcher m1 (a (?? :x) b))

(deftest test-m1
  (is (= (m1 '(a 1 2 b))
	 {:x '(1 2)}))
  (is (nil? (m1 '(a 1 2 b 1)))))

(defmatcher m2 (a ((? :b) 2 3) (? :b) c))

(deftest test-m2
  (is (= (m2 '(a (1 2 3) 1 c))
	 {:b 1}))
  (is (nil? (m2 '(a (1 2 3) 2 c)))))

(defmatcher m3 (a (?? :x) y (?? :x) c))

(deftest test-m3 
  (is (= (m3 '(a b b b y b b b c))
	 {:x '(b b b)}))
  (is (= (m3 '(a b c y b c c))
	 {:x '(b c)})))

; :y must be odd and greater than 10
(defmatcher m4 (:x (? :y odd? #(< 10 %)) :x))

(deftest test-m4
  (is (= (m4 '(3 11 3))
	 {:x 3 :y 11}))
  (is (nil? (m4 '(3 12 3))))
  (is (nil? (m4 '(3 9 3)))))