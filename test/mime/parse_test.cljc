(ns mime.parse-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [mime.address :as address]
            [mime.codec :as codec]
            [mime.header :as header]
            [mime.parse :as parse]))

(defn- crlf [& lines] (str/join "\r\n" lines))

;; ---------------------------------------------------------------- codec

(deftest base64-round-trips-including-padding-cases
  (doseq [s ["" "a" "ab" "abc" "abcd" "hello kotobase" (apply str (map char (range 0 256)))]]
    (is (= s (codec/decode-base64 (codec/encode-base64 s))) (pr-str (count s)))))

(deftest base64-ignores-the-line-wrapping-real-mail-has
  (is (= "hello kotobase"
         (codec/decode-base64 (str "aGVsbG8g\r\n" "a290b2Jhc2U=")))))

(deftest quoted-printable-decodes-escapes-and-soft-breaks
  (is (= "a=b" (codec/decode-quoted-printable "a=3Db")))
  (testing "a soft line break joins, it does not become a newline"
    (is (= "onelong" (codec/decode-quoted-printable "one=\r\nlong")))
    (is (= "onelong" (codec/decode-quoted-printable "one=\nlong"))))
  (testing "underscore is a space only in the RFC 2047 Q variant"
    (is (= "a_b" (codec/decode-quoted-printable "a_b")))
    (is (= "a b" (codec/decode-quoted-printable "a_b" true))))
  (testing "a malformed escape is kept, not dropped"
    (is (= "=zz" (codec/decode-quoted-printable "=zz")))))

(deftest utf8-decodes-multibyte-and-never-throws
  ;; "日本語" as UTF-8 bytes, presented the way the wire presents them:
  ;; one char per byte
  (let [wire (apply str (map char [0xe6 0x97 0xa5 0xe6 0x9c 0xac 0xe8 0xaa 0x9e]))]
    (is (= "日本語" (codec/decode-charset wire "utf-8"))))
  (testing "a truncated sequence yields U+FFFD rather than an exception"
    (is (string? (codec/decode-charset (str (char 0xe6)) "utf-8")))))

(deftest a-charset-this-cannot-decode-is-reported-not-hidden
  (is (codec/decodable? "utf-8"))
  (is (codec/decodable? "ISO-8859-1"))
  (is (not (codec/decodable? "iso-2022-jp"))
      "the charset half of Japanese mail needs the host's decoder, and a
       caller must be able to find that out before it renders mojibake")
  (testing "an injected decoder is used"
    (is (= "DECODED" (codec/decode-charset "raw" "iso-2022-jp" (fn [_ _] "DECODED"))))))

;; --------------------------------------------------------------- header

(deftest folded-headers-are-rejoined
  (let [hs (header/parse-headers (crlf "Subject: one" " two" "\tthree" "To: a@b.c"))]
    (is (= "one two three" (header/header hs "subject")))
    (is (= "a@b.c" (header/header hs "to")))))

(deftest header-lookup-is-case-insensitive-and-keeps-repeats
  (let [hs (header/parse-headers (crlf "Received: one" "RECEIVED: two" "X-A: v"))]
    (is (= ["one" "two"] (header/headers hs "received"))
        "Received legitimately repeats and the order is the delivery path")
    (is (= "v" (header/header hs "x-a")))))

(deftest encoded-words-decode-and-adjacent-ones-do-not-gain-a-space
  (testing "base64 encoded-word"
    (is (= "日本語" (header/decode-encoded-words "=?UTF-8?B?5pel5pys6Kqe?="))))
  (testing "Q encoded-word, where _ is a space"
    (is (= "a b" (header/decode-encoded-words "=?utf-8?Q?a_b?="))))
  (testing "RFC 2047 §6.2: whitespace BETWEEN two encoded-words is deleted"
    (is (= "日本語" (header/decode-encoded-words
                     "=?UTF-8?B?5pel?= =?UTF-8?B?5pys6Kqe?="))))
  (testing "whitespace next to ordinary text is kept"
    (is (= "Re: 日本語" (header/decode-encoded-words "Re: =?UTF-8?B?5pel5pys6Kqe?="))))
  (testing "text with no encoded-word is untouched"
    (is (= "plain subject" (header/decode-encoded-words "plain subject")))))

(deftest content-type-parameters-survive-quoting
  (let [hs (header/parse-headers
            "Content-Type: multipart/mixed; boundary=\"=_a;b_=\"; charset=utf-8")]
    (is (= "multipart/mixed" (:type (header/content-type hs))))
    (is (= "=_a;b_=" (:boundary (header/content-type hs)))
        "a semicolon inside a quoted parameter is not a separator")
    (is (= "utf-8" (:charset (header/content-type hs))))))

(deftest an-absent-content-type-defaults-to-text-plain
  (is (= "text/plain" (:type (header/content-type (header/parse-headers "X: y"))))))

;; -------------------------------------------------------------- address

(deftest an-address-list-splits-on-real-separators-only
  (testing "a comma inside a quoted display name is not a separator"
    (let [parsed (address/parse-list "\"Doe, John\" <j@e.com>, a@e.com")]
      (is (= 2 (count parsed)))
      (is (= "Doe, John" (:name (first parsed))))
      (is (= ["j@e.com" "a@e.com"] (address/addresses parsed)))))
  (testing "display names and bare addresses mix"
    (let [parsed (address/parse-list "Jun Kawasaki <jun@e.com>, plain@e.com")]
      (is (= "Jun Kawasaki" (:name (first parsed))))
      (is (nil? (:name (second parsed))))))
  (testing "comments are dropped, not treated as the name"
    (is (= "a@e.com" (:address (address/parse-one "a@e.com (Some Comment)")))))
  (testing "an encoded-word display name decodes"
    (is (= "日本語" (:name (address/parse-one "=?UTF-8?B?5pel5pys6Kqe?= <a@e.com>")))))
  (testing "an unparseable entry is carried, not dropped"
    (is (= "garbage" (:address (address/parse-one "garbage"))))))

;; ---------------------------------------------------------------- parse

(def simple-message
  (crlf "From: Jun Kawasaki <jun@example.com>"
        "To: alice@example.com, bob@example.com"
        "Subject: =?UTF-8?B?5pel5pys6Kqe?="
        "Message-ID: <abc@example.com>"
        "Content-Type: text/plain; charset=utf-8"
        ""
        "hello"))

(deftest a-simple-message-parses-to-the-shape-mail-inbound-takes
  (let [parts (parse/message-parts (parse/parse simple-message))]
    (is (= "jun@example.com" (:from parts)))
    (is (= ["alice@example.com" "bob@example.com"] (:to parts)))
    (is (= "日本語" (:subject parts)))
    (is (= "<abc@example.com>" (:message-id parts)))
    (is (= "hello" (:text parts)))
    (is (nil? (:html parts)))
    (is (= [] (:attachments parts)))))

(def multipart-alternative
  (crlf "From: a@e.com"
        "To: b@e.com"
        "Subject: alt"
        "Content-Type: multipart/alternative; boundary=BOUND"
        ""
        "preamble text that must be ignored"
        "--BOUND"
        "Content-Type: text/plain; charset=utf-8"
        ""
        "plain version"
        "--BOUND"
        "Content-Type: text/html; charset=utf-8"
        ""
        "<p>html version</p>"
        "--BOUND--"
        "epilogue that must be ignored"))

(deftest multipart-alternative-takes-the-last-alternative-not-the-first
  (let [parts (parse/message-parts (parse/parse multipart-alternative))]
    (is (= "plain version" (:text parts)))
    (is (= "<p>html version</p>" (:html parts)))
    (testing "preamble and epilogue are not part of any body"
      (is (not (str/includes? (str (:text parts)) "preamble")))
      (is (not (str/includes? (str (:html parts)) "epilogue"))))))

(def with-attachment
  (crlf "From: a@e.com"
        "To: b@e.com"
        "Subject: has a file"
        "Content-Type: multipart/mixed; boundary=OUTER"
        ""
        "--OUTER"
        "Content-Type: text/plain; charset=utf-8"
        ""
        "see attached"
        "--OUTER"
        "Content-Type: application/pdf; name=\"report.pdf\""
        "Content-Disposition: attachment; filename=\"report.pdf\""
        "Content-Transfer-Encoding: base64"
        ""
        (codec/encode-base64 "%PDF-1.4 fake")
        "--OUTER--"))

(deftest an-attachment-is-decoded-and-separated-from-the-body
  (let [parts (parse/message-parts (parse/parse with-attachment))]
    (is (= "see attached" (:text parts)))
    (is (= 1 (count (:attachments parts))))
    (let [a (first (:attachments parts))]
      (is (= "report.pdf" (:filename a)))
      (is (= "application/pdf" (:content-type a)))
      (is (= "%PDF-1.4 fake" (:bytes a)))
      (is (= 13 (:size a))))))

(deftest nested-multipart-is-walked
  (let [raw (crlf "From: a@e.com"
                  "To: b@e.com"
                  "Content-Type: multipart/mixed; boundary=OUT"
                  ""
                  "--OUT"
                  "Content-Type: multipart/alternative; boundary=IN"
                  ""
                  "--IN"
                  "Content-Type: text/plain"
                  ""
                  "inner plain"
                  "--IN"
                  "Content-Type: text/html"
                  ""
                  "<b>inner html</b>"
                  "--IN--"
                  "--OUT--")
        parts (parse/message-parts (parse/parse raw))]
    (is (= "inner plain" (:text parts)))
    (is (= "<b>inner html</b>" (:html parts)))))

(deftest a-quoted-printable-body-decodes-with-its-charset
  (let [raw (crlf "From: a@e.com"
                  "To: b@e.com"
                  "Content-Type: text/plain; charset=utf-8"
                  "Content-Transfer-Encoding: quoted-printable"
                  ""
                  "=E6=97=A5=E6=9C=AC=E8=AA=9E")]
    (is (= "日本語" (:text (parse/message-parts (parse/parse raw)))))))

(deftest authentication-results-become-keywords
  (let [raw (crlf "From: a@e.com"
                  "To: b@e.com"
                  "Authentication-Results: mx.example.com; spf=pass smtp.mailfrom=e.com; dkim=fail header.d=e.com; dmarc=none"
                  ""
                  "body")
        parts (parse/message-parts (parse/parse raw))]
    (is (= :pass (:spf parts)))
    (is (= :fail (:dkim parts)))
    (is (= :none (:dmarc parts)))
    (testing "absent means nil, not a guess"
      (is (nil? (:spf (parse/message-parts (parse/parse simple-message))))))))

(deftest bare-lf-messages-parse-too
  ;; the spec says CRLF; plenty of senders and every unix pipe say LF
  (let [raw (str/join "\n" ["From: a@e.com" "To: b@e.com" "Subject: lf" "" "body here"])
        parts (parse/message-parts (parse/parse raw))]
    (is (= "lf" (:subject parts)))
    (is (= "body here" (:text parts)))))

(deftest a-multipart-without-a-boundary-does-not-lose-the-message
  (let [raw (crlf "From: a@e.com" "To: b@e.com"
                  "Content-Type: multipart/mixed"
                  ""
                  "orphaned body")
        root (parse/parse raw)]
    (is (= :multipart-without-boundary (:malformed (first (:parts root)))))
    (is (= "orphaned body" (:text (parse/message-parts root))))))

(deftest a-message-with-no-body-still-parses
  (let [parts (parse/message-parts (parse/parse "From: a@e.com\r\nSubject: empty"))]
    (is (= "empty" (:subject parts)))
    (is (= "" (:text parts)))))
