(ns mime.header
  "RFC 5322 §2.2 header fields, RFC 2045 §5 parameters, RFC 2047
  encoded-words. Pure; input is a binary string (see `mime.codec`)."
  (:require [clojure.string :as str]
            [mime.codec :as codec]))

;; ------------------------------------------------------------ unfolding

(defn split-lines*
  "Split on CRLF or bare LF. Real mail arrives with both, sometimes in
  the same message."
  [s]
  (str/split s #"\r\n|\n" -1))

(defn unfold
  "RFC 5322 §2.2.3: a header field may be folded across lines, and every
  continuation line begins with whitespace. Join them back, keeping one
  space at the seam."
  [lines]
  (reduce (fn [acc line]
            (if (and (seq acc) (re-find #"^[ \t]" line))
              (conj (vec (butlast acc)) (str (last acc) " " (str/triml line)))
              (conj acc line)))
          []
          lines))

;; ------------------------------------------------------- encoded-words

(def ^:private encoded-word-re
  #"=\?([^?]+)\?([BbQq])\?([^?]*)\?=")

(defn decode-encoded-words
  "RFC 2047: `=?charset?B?…?=` / `=?charset?Q?…?=` inside a header.

  Whitespace *between* two adjacent encoded-words is deleted, per §6.2 —
  that is how a long subject split across several words rejoins without
  spurious spaces. Whitespace between an encoded-word and ordinary text
  is kept."
  ([s] (decode-encoded-words s nil))
  ([s decoder]
   (let [decode-one (fn [[_ charset enc text]]
                      (let [raw (if (contains? #{"B" "b"} enc)
                                  (codec/decode-base64 text)
                                  (codec/decode-quoted-printable text true))]
                        (codec/decode-charset raw charset decoder)))]
     (loop [rest- s out [] prev-was-word? false]
       (if-let [m (re-find encoded-word-re rest-)]
         (let [whole (first m)
               idx (str/index-of rest- whole)
               before (subs rest- 0 idx)
               gap-only? (and (seq before) (str/blank? before))
               after (subs rest- (+ idx (count whole)))]
           (recur after
                  (conj out
                        (if (and prev-was-word? gap-only?) "" before)
                        (decode-one m))
                  true))
         (apply str (conj out rest-)))))))

;; ------------------------------------------------------------- parsing

(defn parse-headers
  "Raw header block (already split from the body) -> ordered vector of
  `[lower-case-name raw-value]`.

  A vector, not a map: `Received:` legitimately repeats, and the order of
  those is the delivery path. `header`/`headers` read from it."
  [header-block]
  (->> (split-lines*  header-block)
       unfold
       (keep (fn [line]
               (when-let [i (str/index-of line ":")]
                 (let [name (str/lower-case (str/trim (subs line 0 i)))]
                   (when (seq name)
                     [name (str/triml (subs line (inc i)))])))))
       vec))

(defn header
  "First value for `name`, decoded. Header names are case-insensitive
  (RFC 5322 §1.2.2)."
  ([hs name] (header hs name nil))
  ([hs name decoder]
   (some (fn [[k v]] (when (= (str/lower-case name) k)
                       (decode-encoded-words v decoder)))
         hs)))

(defn headers
  "Every value for `name`, in order, decoded."
  ([hs name] (headers hs name nil))
  ([hs name decoder]
   (->> hs
        (filter (fn [[k _]] (= (str/lower-case name) k)))
        (mapv (fn [[_ v]] (decode-encoded-words v decoder))))))

(defn raw-header
  "First value for `name`, NOT decoded — for fields like Content-Type
  whose structure must be parsed before any encoded-word could apply."
  [hs name]
  (some (fn [[k v]] (when (= (str/lower-case name) k) v)) hs))

;; ---------------------------------------------------------- parameters

(defn parse-parameters
  "RFC 2045 §5.1 structured field: `value; name=val; name=\"quoted val\"`
  -> `{:value \"value\" :params {\"name\" \"val\"}}`.

  Splits on semicolons that are not inside a quoted string — a
  `name=\"a;b\"` parameter is legal and splitting naively corrupts it."
  [s]
  (if (str/blank? (or s ""))
    {:value "" :params {}}
    (let [n (count s)
          segments (loop [i 0 start 0 in-q? false esc? false out []]
                     (if (>= i n)
                       (conj out (subs s start))
                       (let [c (nth s i)]
                         (cond
                           esc? (recur (inc i) start in-q? false out)
                           (= \\ c) (recur (inc i) start in-q? true out)
                           (= \" c) (recur (inc i) start (not in-q?) false out)
                           (and (= \; c) (not in-q?))
                           (recur (inc i) (inc i) false false (conj out (subs s start i)))
                           :else (recur (inc i) start in-q? false out)))))
          unquote (fn [v] (let [v (str/trim v)]
                            (if (and (> (count v) 1) (str/starts-with? v "\"") (str/ends-with? v "\""))
                              (-> (subs v 1 (dec (count v))) (str/replace "\\\"" "\""))
                              v)))]
      {:value (str/trim (first segments))
       :params (into {}
                     (keep (fn [seg]
                             (when-let [i (str/index-of seg "=")]
                               [(str/lower-case (str/trim (subs seg 0 i)))
                                (unquote (subs seg (inc i)))])))
                     (rest segments))})))

(defn content-type
  "-> `{:type \"text/plain\" :charset \"utf-8\" :boundary \"…\" :params {…}}`.
  Defaults to text/plain per RFC 2045 §5.2 when the field is absent."
  [hs]
  (let [{:keys [value params]} (parse-parameters (or (raw-header hs "content-type") "text/plain"))]
    {:type (str/lower-case (if (str/blank? value) "text/plain" value))
     :charset (get params "charset")
     :boundary (get params "boundary")
     :params params}))

(defn content-disposition
  "-> `{:disposition \"attachment\" :filename \"…\" :params {…}}`, or nil."
  [hs]
  (when-let [raw (raw-header hs "content-disposition")]
    (let [{:keys [value params]} (parse-parameters raw)]
      {:disposition (str/lower-case value)
       :filename (or (get params "filename") (get params "filename*"))
       :params params})))
