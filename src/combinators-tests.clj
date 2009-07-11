;;
;; cljmatch, pattern matching in clojure 
;;

; Copyright (c) 2009 Jonas Enlund
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.


(ns cljmatch.combinators.tests
  (:use clojure.test
	cljmatch.combinators))


(deftest match-equal
  (let [e1   (match:equal 1)
	en   (match:equal 'n)
	e:k  (match:equal :k)
	estr (match:equal "Hello, World!")
	elst (match:equal '(a b c))
	evec (match:equal [1 2 3])]
    (is (= (e1 '(1) {} list)
	   (list {} 1)))
    (is (= (en '(n) {} list)
	   (list {} 1)))
    (is (= (e:k '(:k) {} list)
	   (list {} 1)))
    (is (= (estr '("Hello, World!") {} list)
	   (list {} 1)))
    (is (= (elst '((a b c)) {} list)
	   (list {} 1)))
    (is (= (evec '([1 2 3]) {} list)
	   (list {} 1)))
    (is (nil? (e1 '(2) {} list)))
    (is (nil? (en '(m) {} list)))
    (is (nil? (e:k '(:l) {} list)))
    (is (nil? (estr '("Goodbye, Cruel World!") {} list)))
    (is (nil? (elst '((c b a)) {} list)))
    (is (nil? (evec '([3 2 1]) {} list)))))


(deftest match-element
  (let [m (match:element :m)
	k (match:element 'k)
	odd (match:element :x integer? odd?)
	even (match:element :x even?)]
    (is (= (m '(1) {} list)
	   (list {:m 1} 1)))
    (is (nil? (m '(2) {:m 1} list)))
    
    (is (= (k '(:x) {} list)
	   (list {'k :x} 1)))
    (is (nil? (k '("a string") {'k "b string"} list)))
    
    (is (= (odd '(5) {} list)
	   (list {:x 5} 1)))
    (is (nil? (odd '(6) {} list)))
    (is (nil? (odd '("string") {} list)))
	
    (is (= (even '(6) {} list)
	   (list {:x 6} 1)))
    (is (nil? (even '(5) {} list)))
    (is (thrown? java.lang.ClassCastException 
		 (even '("a string") {} list)))))


(deftest match-anything
  (let [a (match:anything)
	i (match:anything integer?)
	s (match:anything string?)
	o (match:anything integer? odd?)]
    (is (= (a '(2) {} list)
	   (list {} 1)))
    (is (= (i '(2) {} list)
	   (list {} 1)))
    (is (nil? (i '("string") {} list)))
    (is (= (s '("string") {} list)
	   (list {} 1)))
    (is (= (o '(3) {} list)
	   (list {} 1)))
    (is (nil? (o '("string") {} list)))))


(deftest match-list
  (let [oeo (match:list (match:anything odd?) 
			(match:anything even?)
			(match:anything odd?))
	
	hel (match:list (match:equal "Hello")
			(match:equal "World"))
	
	big (match:list (match:anything integer?)
			(match:element :x integer? (fn [x] (> x 10)))
			(match:anything integer? even?))]
    
    (is (= (oeo '((3 2 9)) {} list)
	   (list {} 1)))
    (is (nil? (oeo '((3 2 8)) {} list)))
    (is (nil? (oeo '((3 2)) {} list)))
    (is (nil? (oeo '((3 2 9 0)) {} list)))

    (is (= (hel '(("Hello" "World")) {} list)
	   (list {} 1)))

    (is (= (big '((1 12 8)) {} list)
	   (list {:x 12} 1)))
    (is (nil? (big '((1 9 8)) {} list)))))


(deftest match-segment
  (let [xyx (match:list (match:segment :x)
			(match:element :y)
			(match:segment :x))
	x4y (match:list (match:segment :x)
			(match:equal   4)
			(match:segment :z))]
    (is (= (xyx '((1 2 3 4 1 2 3)) {} list)
	   (list {:x '(1 2 3) :y 4} 1)))
    (is (nil? (xyx '((1 2 3 4 3 2 1)) {} list)))
	
    (is (= (x4y '((1 2 3 4 3 2 1)) {} list)
	   (list {:x '(1 2 3) :z '(3 2 1)} 1)))
    (is (= (x4y '((4 1 2 3)) {} list)
	   (list {:x '() :z '(1 2 3)} 1)))
    (is (= (x4y '((1 2 3 4)) {} list)
	   (list {:x '(1 2 3) :z '()} 1)))
    (is (nil? (x4y '((1 2 3 5 3 2 1)) {} list)))))

(deftest match-choice
  (let [oddorstr (match:choice (match:anything integer? odd?)
			       (match:anything string?))]
    (is (= (oddorstr '("Hello, World!") {} list)
	   (list {} 1)))
    (is (= (oddorstr '(5) {} list)
	   (list {} 1)))
    (is (nil? (oddorstr '(6) {} list)))))


