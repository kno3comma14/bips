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

(ns bips.bip350)

(def encoding-charset "qpzry9x8gf2tvdw0s3jn54khce6mua7l")
(def decoding-charset [-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                       -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                       -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                       15 -1 10 17 21 20 26 30 7 5 -1 -1 -1 -1 -1 -1
                       -1 29 -1 24 13 25 9 8 23 -1 18 22 31 27 19 -1
                       1 0 3 16 11 28 12 14 6 4 2 -1 -1 -1 -1 -1
                       -1 29 -1 24 13 25 9 8 23 -1 18 22 31 27 19 -1
                       1 0 3 16 11 28 12 14 6 4 2 -1 -1 -1 -1 -1])

(def bech32-constant 1)
(def bech32m-constant 0x2bc830a3)
(def encoding-type {:bech32 0 :bech32m 1})

(defn bech32-hrp-expand
  "Expansion of the hrp. To execute this process we get hrp parameter and apply the
  bit shift right 5 operatior to the first part, add the component zero to the resulting
  vector then add another vector at the end with and operation with the constant 31.
  Reference: ``"
  [hrp]
  (let [first-part (for [i hrp]
                     (bit-shift-right (int i) 5))
        connector (list 0)
        second-part (for [i hrp] (bit-and (int i) 31))]
    (into [] (concat first-part connector second-part))))


(def chk (atom 1))

(defn bech32-polymod
  [values]
  (loop [values-index 0]
    (if (>= values-index (count values))
      @chk
      (do
        (let [c0 (bit-shift-right @chk 25)
              _ (reset! chk (bit-xor (bit-shift-left (bit-and @chk 0x1ffffff) 5) (bit-and (nth values values-index) 0xff)))]
          (when (not= (bit-and c0 1) 0)
            (reset! chk (bit-xor @chk 0x3b6a57b2)))
          (when (not= (bit-and c0 2) 0)
            (reset! chk (bit-xor @chk 0x26508e6d)))
          (when (not= (bit-and c0 4) 0)
            (reset! chk (bit-xor @chk 0x1ea119fa)))
          (when (not= (bit-and c0 8) 0)
            (reset! chk (bit-xor @chk 0x3d4233dd)))
          (when (not= (bit-and c0 16) 0)
            (reset! chk (bit-xor @chk 0x2a1462b3))))
        (recur (+ values-index 1))))))

(defn bech32-verify-checksum
  [hrp, data]
  (let [polymod (bech32-polymod (into [] (concat (bech32-hrp-expand hrp) data)))]
    (when (= polymod 1)
      (:bech32 encoding-type))
    (when (= polymod bech32m-constant)
      (:bech32m encoding-type))))


(comment
  (bech32-polymod [3 3 3 0 1 2 3]))
