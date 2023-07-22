;; Copyright © 2022 CicadaBank

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(ns bips.stack-operations
  (:refer-clojure :exclude [+ - * / cond])
  (:require [bips.hashes :as h]
            [bips.ec-utils :as ecu]
            [better-cond.core :refer [cond]]
            [clojure.math.numeric-tower :as nt]))


;; Base stack operations
(defn s-push
  [stack item]
  (swap! stack conj item))

(defn s-pop
  ([stack]
   (let [top (peek @stack)]
     (swap! stack pop)
     top))
  ([stack n]
   (reverse (vec (for [_ (range n)]
                   (let [top (peek @stack)]
                     (swap! stack pop)
                     top))))))

(defn s-peek
  ([stack]
   (peek @stack))
  ([stack n]
   (if (<= n (count @stack))
     (vec (take-last n @stack))
     nil)))

(defn op-dup 
  [stack]
  (if (< (count @stack) 1)
    false
    (s-push stack (s-peek @stack))))

(defn op-hash160
  [stack]
  (if (< (count @stack) 1)
    false
    (s-push stack (h/hash160 (s-pop @stack)))))

(defn op-verify
  [stack]
  (if (or (< (count @stack) 1) (= 0 (s-pop stack)))
    false
    true))

(defn op-equals
  [stack]
  (if (< (count @stack) 2)
    false
    (let [a (s-pop @stack)
          b (s-pop @stack)]
      (if (= a b)
        (s-push stack 1) ;; Investigate how to encode 1 and 0
        (s-push stack 0)))))

(defn op-equal-verify
  [stack]
  (and (op-verify stack)
       (op-equals stack)))

((defn op-checksig
  [stack z] ;; TODO
   ))
