(ns concurtimes.core
  (:require [clojure.tools.cli :refer [cli]]
            [clojure.java.io :as io]
            [clojure.core.async
             :refer [go go-loop chan >! <! close!]
             :as async]
            [clojure.string :as str])
  (:gen-class))

#_
(println (-> (java.io.File. ".")
             .getCanonicalPath))
#_
(.list (io/file "resources"))

#_
(with-open [rdr (io/reader "resources/its-in-the-hole.txt")]
  (doall (map #(println :line %) (line-seq rdr))))

(def hole "resources/its-in-the-hole.txt")
(def himom "hi

mom

boom
")


(defn char-seq 
   [^java.io.Reader rdr]
   (let [chr (.read rdr)]
     (when (>= chr 0)
       (cons chr (lazy-seq (char-seq rdr))))))

#_
(with-open [rdr (clojure.java.io/reader hole)]
  (loop [n 0]
    (let [c (.read rdr)]
      (when (and (< n 1000)
              (not= c -1))
        (print (char c))
        (recur (inc n))))))
            
(apply str (repeat 5 \space))

(with-open [rdr (clojure.java.io/reader hole)]
  (let [col-width 20
        line-out (fn [c b]
                   (print (apply str b (repeat (- col-width (count b)) \space)))
                   (println \|))]
    (loop [state :nl
           column 0
           last-ws nil
           buff ""]
      (if (>= column col-width)
        (do
          (condp = state
            :ws (do
                  (line-out \| buff)
                  (recur :nl 0 nil ""))
            :tx (if last-ws
                  (do
                    (line-out \#
                      (subs buff 0 (inc last-ws)))
                    (let [new-buff (str/triml (subs buff last-ws))]
                      (recur :tx (count new-buff) nil new-buff)))
                  ;; whoa, that is a big word
                  (do
                    (line-out \& (str (subs buff 0 (dec (count buff))) \-))
                    (recur :tx 1 nil (str (last buff)))))))
        (let [c (.read rdr)]
          (condp = c
            -1 :eof
            10 (do (line-out \^ buff)
                   (recur :nl 0 nil ""))
            13 (do (line-out \@ buff)
                   (recur :nl 0 nil ""))
            9 (recur :ws (inc column) column (str buff \space))
            32 (if (= state :nl)
                 (recur :nl 0 nil "")
                 (recur :ws (inc column) column (str buff \space)))
            (recur :tx (inc column) last-ws (str buff (char c))))))))
  (println :eof))

(apply str ["bo" \x \y])
#_
(let [out (chan)
      raw (slurp "resources/gettysburgh.txt")
      splitter #"[^a-zA-Z\d-]"
      words (str/split raw splitter)]
  (go
    (doseq [c words]
      (>! out c))
    (close! out))

  (go-loop []
    (let [c (<! out)]
      (if (nil? c)
        (println)
        (do
          (println c)
          (recur)))))
  :eoj)

(defn bah []
  (loop [[c & rs] himom
         offset 0
         state :init ;; FSM: :init, :ws (whitespace), :wd (word)
         newlines [] ;; offsets
         words [] ;; word start offsets
         ]
    (if (nil? c)
      [newlines words]
      (let [next-off (inc offset)]
        (condp = c
          \space (recur rs next-off :ws newlines words)
          \tab (recur rs next-off :ws newlines words)
          \newline (recur rs next-off :ws (conj newlines offset) words)
          (recur rs next-off :wd newlines
            (if (= state :wd)
              words
              (conj words offset))))))))
#_
(map (fn [n c]
       (println n c (int c)))
  (range 40)
  (slurp "resources/gettysburgh.txt"))

(defn -main [& args]
  (let [[opts args banner]
        (cli args
          ["-h" "--help" "Usage: (concurtimes <tex-file-path>)"
           :default false :flag true])]
    (cond
      (:help opts) (println banner)
      :default
      (try
        (let [raw (slurp (first args))
              splitter #"[^a-zA-Z\d-]"
              word-freq (frequencies
                          (map str/lower-case
                            (remove empty?
                              (str/split raw splitter))))]

            (doseq [[word freq] (reverse (sort-by val word-freq))]
              (println (format "%9d %s" freq word))))

        (catch Exception e (str "caught exception: " (.getMessage e)))))))

#_
(time (-main "cells-manifesto.txt"))

