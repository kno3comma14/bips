(ns bips.ec-utils
  (:refer-clojure :exclude [+ - * / cond])
  (:require [bips.hashes :as h]
            [better-cond.core :refer [cond]]
            [clojure.math.numeric-tower :as nt]
            [buddy.core.nonce :as nonce]
            [buddy.core.codecs :refer :all]
            [buddy.core.bytes :as bytes]))

(defn bytes->num "Interprets as unsigned 256-bit number" [bs]
  (BigInteger. (byte-array (into [0] bs))))

;; Elyptic curve base operations
;; Using FieldOps definition from Project Alchemy

(declare S256Point?)
(def P (clojure.core/- (nt/expt 2 256) (nt/expt 2 32) 977))
(def N 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141)

(defn mod-pow [number power modulus]
  (.modPow (biginteger number) (biginteger power) (biginteger modulus)))

(defn mod-inverse [number modulus]
  (.modInverse (biginteger number) (biginteger modulus)))

(defprotocol FieldOps
 (+ [x y])
 (- [x y])
 (* [x y])
 (/ [x y])
 (expt [x exponent])
 (zero [x]))

(defn scalar-multiply [n z]
  (if (zero? n) (zero z)
      (loop [n n, y (zero z), z z]
        (let [t (even? n), n (quot n 2)]
          (cond
            t (recur n y (+ z z))
            (zero? n) (+ z y)
            :else (recur n (+ z y) (+ z z)))))))

(extend-type Number
  FieldOps
  (+ [x y] (+' x y))
  (- [x y] (-' x y))
  (* [x y] (cond
             (number? y) (*' x y)
             (S256Point? y) (scalar-multiply (mod x N) y)
             :else (scalar-multiply x y)))
  (/ [x y] (clojure.core// x y))
  (zero [x] 0)
  (expt [x exponent] (nt/expt x exponent)))

(defrecord FieldElement [num prime]
  FieldOps
  (+ [x {num2 :num, prime2 :prime}]
    (assert (= prime prime2) "Cannot add number from two different fields")
    (FieldElement. (mod (+' num num2) prime) prime))
  (- [x {num2 :num, prime2 :prime}]
    (assert (= prime prime2) "Cannot subtract number from two different fields")
    (FieldElement. (mod (-' num num2) prime) prime))
  (* [x {num2 :num, prime2 :prime}]
    (assert (= prime prime2) "Cannot multiply number from two different fields")
    (FieldElement. (mod (*' num num2) prime) prime))
  (/ [x {num2 :num, prime2 :prime}]
    (assert (= prime prime2) "Cannot divide number from two different fields")
    (FieldElement. (mod (*' num (mod-inverse num2 prime)) prime) prime))
  (zero [x] (FieldElement. 0 prime))
  (expt [x exponent]
    (let [exponent (mod exponent (dec prime))]
      (FieldElement. (mod-pow num exponent prime) prime))))

(defn ->FieldElement [num prime]
  (assert (and (<= 0 num) (< num prime)))
  (FieldElement. num prime))

(defrecord Point [x y a b]
  FieldOps
  (zero [p] (Point. nil nil a b))
  (+ [p1 {x2 :x y2 :y a2 :a b2 :b :as p2}]
    (assert (and (= a a2) (= b b2)) "Points must be on the same curve")
    (cond
      (nil? x) p2
      (nil? x2) p1
      (and (= x x2) (not= y y2)) (Point. nil nil a b)
      (= p1 p2) (if (= y (zero x)) (Point. nil nil a b)
                    (let [s (/ (+ (* 3 (expt x 2)) a) (* 2 y)),
                          x3 (- (expt s 2) (* 2 x))
                          y3 (- (* s (- x x3)) y)]
                      (Point. x3 y3 a b)))
      :else (let [s (/ (- y2 y) (- x2 x)),
                  x3 (- (- (expt s 2) x) x2),
                  y3 (- (* s (- x x3)) y)]
              (Point. x3 y3 a b)))))

(defn valid-point? [x y a b]
  (or (= x y nil)
      (= (expt y 2) (+ (+ (expt x 3) (* a x)) b))))

(defn ->Point [x y a b]
  (assert (valid-point? x y a b) "Point not on curve")
  (Point. x y a b))

(def A (->FieldElement 0 P))
(def B (->FieldElement 7 P))

(defn ->S256Point
  "Takes two numbers from 0 to P-1, passes through nil or FieldElements"
  [x y]
  (let [x (if (number? x) (->FieldElement x P) x),
        y (if (number? y) (->FieldElement y P) y)]
    (->Point x y A B)))

(defn S256Point? [{:keys [a b]}] (and (= a A) (= b B)))

(def G (->S256Point
        0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798,
        0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8))

(defrecord Signature [r s])

(defn verify-signature
  "Takes a public-key (point on secp256k1 curve), z (256-bit hash of some data), and a signature (comprised of r and s)"
  [public-key z {:keys [r s]}]
  (let [s-inv (mod-inverse s N),
        u (mod (* z s-inv) N),
        v (mod (* r s-inv) N),
        total (+ (* u G) (* v public-key))]
    (= (-> total :x :num) r)))

(defrecord PrivateKey [secret point])

(defn ->PrivateKey [secret]
  (PrivateKey. secret (* secret G)))

(defn rand-256 "Generates a random 256 bit number" []
  (bytes->num (nonce/random-bytes 32)))

(defn rand-N []
  (let [i (rand-256)]
    (if (< i N)
      i
      (recur))))

