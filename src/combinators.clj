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

;; These matcher combinators are inspired by:
;; http://groups.csail.mit.edu/mac/users/gjs/6.945/psets/ps05/ps.txt

(ns cljmatch.combinators)

; Matches if the pattern-constant is equal to
; (the first element of) the data. 
(defn match:equal [pattern-constant]
  (fn [data dictionary succeed]
    (when (and (seq data)
	       (= (first data) 
		  pattern-constant))
      (succeed dictionary 1))))


; Helper function.
(defn satisfy-all? [predicates data]
  (every? (fn [p] (p data)) predicates))

; When (the first element of) the data satisfies all prediacates two cases are 
; considered:
; 1. The dictionary does not contain the 'variable' (as key). The match is 
;    successful with a new binding in the dictionary
; 2. When the dictionary contains the variable (as key) the match is successful 
;    only if (the first element of) the data is equal to the associated value 
;    in the dictionary. 
(defn match:element [variable & predicates]
  (fn [data dictionary succeed]
    (when (and (seq data)
	       (satisfy-all? predicates 
			     (first data)))
      (if (contains? dictionary variable)
	(when (= (dictionary variable)
		 (first data))
	  (succeed dictionary 1))
	(succeed (assoc dictionary variable (first data)) 1)))))

; When (the first element of) the data satisfies all predicates the match 
; succeeds.
(defn match:anything [& predicates]
  (fn [data dictionary succeed]
    (when (and (seq data)
	       (satisfy-all? predicates
			     (first data)))
      (succeed dictionary 1))))

; The match is successful when (the first element of) the data is a list which
; successfully matches the match-combinators.
(defn match:list [& match-combinators]
  (fn [data dictionary succeed]
    (when (and (seq data)
	       (list? (first data)))
      (letfn [(lp [dt matchers dict]
		  (cond 
		   (seq matchers) 
		   ((first matchers) dt dict (fn [new-dictionary n]
					       (lp (drop n dt)
						   (rest matchers)
						   new-dictionary)))
		   (empty? dt) (succeed dict 1)))]
	(lp (first data)
	    match-combinators
	    dictionary)))))


(defn match:segment [variable]
  (fn [data dictionary succeed]
    (when (seq data)
      (if (contains? dictionary variable)
	(loop [dt data
	       pattern (dictionary variable)
	       n 0]
	  (cond (seq pattern)
		(when (and (seq dt)
			   (= (first dt) (first pattern)))
		  (recur (rest dt) (rest pattern) (+ n 1)))
		(seq pattern) nil
		:else (succeed dictionary n)))
	(let [n (count data)]
	  (loop [i 0]
	    (when (<= i n)
	      (or (succeed (assoc dictionary variable (take i data)) i)
		  (recur (+ i 1))))))))))

(defn match:choice [& match-combinators]
  (fn [data dictionary succeed]
    (loop [combinators match-combinators]
      (when (seq combinators)
	(or ((first combinators) data dictionary succeed)
	    (recur (rest combinators)))))))

