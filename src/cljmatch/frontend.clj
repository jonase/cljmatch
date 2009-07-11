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

;; 
; Simple front end compiler
;;

; <anything> ::= _ 
;              | (? _ <predicate>*)
; <element>  ::= <variable> 
;              | (? <variable> <predicate>*)
; <list>     ::= (list <pattern>*)
; <pattern>  ::= <anything> 
;              | <element>
;              | <list>
; <variable> ::= <keyword> \ _

(ns cljmatch.frontend
  (:use cljmatch.combinators))

(declare match:compile)

(defn match:anything? [pattern]
  (or (and (symbol? pattern)
	   (= pattern '_))
      (and (list? pattern)
	   (= (first pattern)
	      '?)
	   (= (second pattern)
	      '_))))

(defn match:anything-predicates [pattern]
  (if (list? pattern)
    (map eval (drop 2 pattern))
    (list)))

(defn match:element? [pattern]
  (or (keyword? pattern)
      (and (list? pattern)
	   (= (first pattern)
	      '?)
	   (keyword? (second pattern)))))

(defn match:element-variable [pattern]
  (if (list? pattern)
    (second pattern)
    pattern))

(defn match:element-predicates [pattern]
  (if (list? pattern)
    (map eval (drop 2 pattern))
    (list)))

(defn match:list? [pattern]
  (list? pattern))

(defn match:segment? [pattern]
  (and (list? pattern)
       (= (first pattern)
	  '??)
       (keyword? (second pattern))))
	  
(defn match:segment-variable [pattern]
  (second pattern))

(defn match:choice? [pattern]
  (and (list? pattern)
       (= (first pattern)
	  'either)))

(defn match:choice-combinators [pattern]
  (map match:compile (rest pattern)))

(defn dispatch [pattern]
  (cond (match:anything? pattern) :Anything
	(match:element?  pattern) :Element
	(match:segment?  pattern) :Segment
	(match:choice?   pattern) :Choice
	(match:list?     pattern) :List))
	

(defmulti match:compile dispatch)

(defmethod match:compile :Anything [pattern]
  (apply match:anything (match:anything-predicates pattern)))

(defmethod match:compile :Element [pattern]
  (apply match:element (cons (match:element-variable pattern) 
			     (match:element-predicates pattern))))



(defmethod match:compile :List [pattern]
  (apply match:list 
	 (map match:compile
	      pattern)))

(defmethod match:compile :Segment [pattern]
  (match:segment (match:segment-variable pattern)))

(defmethod match:compile :default [pattern]
  (match:equal pattern))

(defn matcher [pattern]
  (fn [data]
    ((match:compile pattern) (list data) {} (fn [d i] d))))


(defmacro defmatcher [s pattern]
  (let [combinator (matcher pattern)]
    `(def ~s ~combinator)))

    

  