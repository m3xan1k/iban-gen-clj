(ns iban-gen
  (:require [clojure.string :as str]))

(def country-codes
  {"Austria" "AT" "Belgium" "BE" "Bulgaria" "BG"
   "Croatia" "HR" "Cyprus" "CY" "Czech Republic" "CZ"
   "Denmark" "DK" "Estonia" "EE" "Finland" "FI"
   "France" "FR" "Germany" "DE" "Greece" "GR"
   "Hungary" "HU" "Ireland" "IE" "Italy" "IT"
   "Latvia" "LV" "Lithuania" "LT" "Luxembourg" "LU"
   "Malta" "MT" "Netherlands" "NL" "Poland" "PL"
   "Portugal" "PT" "Romania" "RO" "Slovakia" "SK"
   "Slovenia" "SI" "Spain" "ES" "Sweden" "SE"
   "Switzerland" "CH" "United Kingdom" "GB" "Vatican City" "VA"})

(def country-lengths
  {"AL" 28 "AD" 24 "AT" 20 "BE" 16 "BG" 22
   "HR" 21 "CY" 28 "CZ" 24 "DK" 18 "EE" 20
   "FI" 18 "FR" 27 "DE" 22 "GR" 27 "HU" 28
   "IS" 26 "IE" 22 "IT" 27 "LV" 21 "LI" 21
   "LT" 20 "LU" 20 "MT" 31 "MC" 27 "NL" 18
   "MK" 19 "NO" 15 "PL" 28 "PT" 25 "RO" 24
   "SM" 27 "SK" 24 "SI" 19 "ES" 24 "SE" 24
   "CH" 21 "GB" 22 "VA" 22})

(defn get-country-code [country]
  (if (= (count country) 2)
    country
    (get country-codes country)))

(defn get-iban-length [country-code]
  (get country-lengths country-code))

(defn random-4-digits []
  (rand-int 10000))

(defn random-10-digits []
  (rand-int 1000000000))

(defn concat-numbers [& args]
  (apply str args))

(defn make-bban [bank-code account-number iban-length]
  (let [bban (concat-numbers bank-code account-number)
        padding-length (- iban-length 4 (count bban))]
    (str (apply str (repeat padding-length "0")) bban)))

(defn swap-iban [iban]
  (str (subs iban 4) (subs iban 0 4)))

(defn char->iban-digit [c]
  (if (Character/isDigit c)
    (str c)
    (str (- (int c) (int \A) 10))))

(defn make-iban-numeric [iban]
  (apply str (map char->iban-digit iban)))

(defn make-check-digits [iban-numeric]
  (letfn [(char->digit [c]
            (- (int c) (int \0)))
          (process-digits [rem index]
            (if (< index (count iban-numeric))
              (let [c (nth iban-numeric index)
                    digit (char->digit c)
                    new-rem (mod (+ (* rem 10) digit) 97)]
                (process-digits new-rem (inc index)))
              rem))
          (string-remainder [s divisor]
            (process-digits 0 0))]
    (let [check (- 98 (string-remainder iban-numeric 97))]
      (if (< check 10)
        (str "0" check)
        (str check)))))


(defn generate-iban [country-code iban-length bank-code account-number]
  (let [bban (make-bban bank-code account-number iban-length)
        iban (str country-code "00" bban)
        iban-swapped (swap-iban iban)
        iban-numeric (make-iban-numeric iban-swapped)
        check-digits (make-check-digits iban-numeric)]
    (str country-code check-digits bban)))

(defn get-user-input []
  (println "Enter country name or country code: ")
  (read-line))

(defn validate-country-input [input]
  (let [country-code (get-country-code input)]
    (if country-code
      country-code
      (do
        (println (format "Country not found: %s" input))
        false))))

(defn validate-iban-length [country-code]
  (let [iban-length (get-iban-length country-code)]
    (if iban-length
      iban-length
      (do
        (println (format "IBAN length not found for country code: %s" country-code))
        false))))

(defn generate-and-display-iban [country-code iban-length]
  (let [bank-code (random-4-digits)
        account-number (random-10-digits)
        iban (generate-iban
               (str/upper-case country-code)
               iban-length
               bank-code
               account-number)]
    (println (format "Generated IBAN: %s" iban))))

(defn -main []
  (let [user-input (get-user-input)
        country-code (validate-country-input user-input)]
    (when country-code
      (let [iban-length (validate-iban-length country-code)]
        (when iban-length
          (generate-and-display-iban country-code iban-length))))))

(-main)
