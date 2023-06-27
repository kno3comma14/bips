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

(ns bips.bip350
  (:import org.bitcoinj.core.Bech32))

(def encoding-type {:bech32 1 :bech32m 0x2bc830a3})

(def characters-reference "qpzry9x8gf2tvdw0s3jn54khce6mua7l")

(def reversed-characters-reference
  [-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
   -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
   -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
   15 -1 10 17 21 20 26 30  7  5 -1 -1 -1 -1 -1 -1
   -1 29 -1 24 13 25  9  8 23 -1 18 22 31 27 19 -1
   1  0   3 16 11 28 12 14  6  4  2 -1 -1 -1 -1 -1
   -1 29 -1 24 13 25  9  8 23 -1 18 22 31 27 19 -1
   1  0   3 16 11 28  12 14 6  4  2 -1 -1 -1 -1 -1])

(defn bech32-hrp-expand
  "Expansion of the hrp. To execute this process we get hrp parameter and apply the
  bit shift right 5 operation to the first part, add the component zero to the resulting
  vector then add another vector at the end with and operation with the constant 31.
  Reference: `https://github.com/bitcoin/bips/blob/master/bip-0350.mediawiki`"
  [hrp]
  (let [first-part (for [i hrp]
                     (bit-shift-right (int i) 5))
        connector (list 0)
        second-part (for [i hrp] (bit-and (int i) 31))]
    (into [] (concat first-part connector second-part))))


(def chk
  "Value to partially save polymod - Atom"
  (atom 1))

(defn bech32-polymod
  "Function to calculate the bench32 polymod value"
  [values]
  (loop [values-index 0]
    (if (>= values-index (count values))
      @chk
      (do
        (let [c0 (bit-and (unsigned-bit-shift-right @chk 25) 0xff)
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
  "Verifies if the checksum can be created using bech32 or bech32m methods."
  [hrp, data]
  (let [polymod (bech32-polymod (into [] (concat (bech32-hrp-expand hrp) data)))]
    (reset! chk 1)
    (case polymod
      1 :bech32
      0x2bc830a3 :bech32m
      :not-valid-bech32)))

(defn bech32-create-checksum
  "Creates the checksum given the encoding type. Useful for encoding/decoding functions"
  [encoding hrp data]
  (let [expanded-hrp (bech32-hrp-expand hrp)
        encoded-vec (into [] (concat expanded-hrp data [0 0 0 0 0 0]))
        mod-value (bit-xor (bech32-polymod encoded-vec) (encoding encoding-type))]
    (reset! chk 1)
    (for [i (range 6)]
      (byte (bit-and (unsigned-bit-shift-right mod-value (* 5 (- 5 i))) 31)))))

(defn bech32-encode
  "Encode to bech32 using encoding type, hrp and data as parameters"
  [encoding hrp data]
  (let [lower-case-hrp (clojure.string/lower-case hrp)
        checksum (bech32-create-checksum encoding lower-case-hrp data)
        combined-vector (into [] (concat data checksum))
        partial-result (str lower-case-hrp "1")]
    (reduce (fn [acc, item] (str acc (nth characters-reference item)))
            partial-result
            combined-vector)))

(defn bech32-decode
  "Given a compatible bech32 or bech32m string, this function decode the string
  to map that contains the data associated to a bech32 map"
  [target]
  (let [pos (clojure.string/index-of target "1")
        data-length (- (count target) 1 pos)
        values (for [i (range data-length)]
                 (nth reversed-characters-reference (int (nth target (+ i pos 1)))))
        hrp (clojure.string/lower-case (subs target 0 pos))
        encoding (bech32-verify-checksum hrp values)]
    {:encoding encoding
     :hrp hrp
     :data (subvec (vec values) 0 (- (count values) 6))}))
