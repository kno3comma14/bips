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

(ns bips.bip350-test
  (:require [bips.bip350 :as bip350]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]))

(def test-vectors
  (-> "test/bips/fixtures/bip350-vectors.edn"
      slurp
      edn/read-string))

(def encode-input-samples (:encode-input-data test-vectors))
(def decode-input-samples (:decode-input-data test-vectors))

(deftest bech32-encode-test
  (testing "bech32-encode for bech32m valid inputs"
    (let [samples (subvec (:bech32m encode-input-samples) 0 3)
          expected-values (subvec (:bech32m decode-input-samples) 1 4)
          real-values (for [s samples]
                        (bip350/bech32-encode (:encoding s) (:hrp s) (:data s)))]
      (is (= expected-values (vec real-values)))))
  (testing "bech32-encode for bech32 valid inputs"
    (let [samples (subvec (:bech32 encode-input-samples) 0 3)
          expected-values (subvec (:bech32 decode-input-samples) 1 4)
          real-values (for [s samples]
                        (bip350/bech32-encode (:encoding s) (:hrp s) (:data s)))]
      (is (= expected-values (vec real-values))))))

(deftest bech32-decode-test
  (testing "bech32-decode for bech32m valid inputs"
    (let [samples (subvec (:bech32m decode-input-samples) 1 4)
          expected-values (subvec (:bech32m encode-input-samples) 0 3)
          real-values (for [s samples]
                        (bip350/bech32-decode s))]
      (is (= expected-values (vec real-values)))))
  (testing "bech32-decode for bech32 valid inputs"
    (let [samples (subvec (:bech32 decode-input-samples) 1 4)
          expected-values (subvec (:bech32 encode-input-samples) 0 3)
          real-values (for [s samples]
                        (bip350/bech32-decode s))]
      (is (= expected-values (vec real-values))))))
