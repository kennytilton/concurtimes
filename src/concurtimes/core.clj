(ns concurtimes.core
  (:require [clojure.tools.cli :refer [cli]]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]
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
            
 (Thread/setDefaultUncaughtExceptionHandler
     (reify Thread$UncaughtExceptionHandler
       (uncaughtException [_ thread ex]
         (log/error {:what :uncaught-exception
                     :exception ex
                     :where (str "Uncaught exception on" (.getName thread))}))))

(defn column-feeder [file col-width]
  (let [out (chan)
        filler (apply str (repeat col-width \space))
        rdr (clojure.java.io/reader file)]
    (go-loop [state :nl
              column 0
              last-ws nil
              buff ""]
      (let [pad-out (fn [b]
                      ;;b #_
                      (subs (str b filler) 0 col-width))]
        (cond
          (>= column col-width)
          (condp = state
            :nl (do
                  (>! out (pad-out "WHOA-NL"))
                  (recur :nl 0 nil ""))
            :ws (do
                  (>! out (pad-out buff))
                  (recur :nl 0 nil ""))
            :tx (if last-ws
                  (do
                    (>! out (pad-out
                              (subs buff 0 (inc last-ws))))
                    (let [new-buff (str/triml (subs buff last-ws))]
                      (recur :tx (count new-buff) nil new-buff)))
                  ;; whoa, that is a big word
                  (do
                    (>! out (pad-out (str (subs buff 0 (dec (count buff))) \-)))
                    (recur :tx 1 nil (str (last buff))))))
          
          :default
          (let [c (.read rdr)]
            (condp = c
              -1 (do
                   (close! out)
                   (.close rdr))
              10 (do (>! out (pad-out buff))
                     (recur :nl 0 nil ""))
              13 (do (>! out (pad-out "WHOA!" #_ buff))
                     (recur :nl 0 nil ""))
              9 (recur :ws (inc column) column (str "TAB!" #_ buff \space))
              32 (if (= state :nl)
                   (recur :nl 0 nil "")
                   (recur :ws (inc column) column (str buff \space)))
              (recur :tx (inc column) last-ws (str buff 
                                                (if (< 32 c 128)
                                                  (char c) \space))))))))
    out))

(go-cols "its-in-the-hole" "mobydick" "gettysburgh"
  "cells-manifesto" "obama-wright")

#_
(with-open [out (clojure.java.io/writer "stuff.txt")]
  (.write out "kkkkk"))

(defn go-cols [& filenames]
  (let [out (clojure.java.io/writer "stuff.txt")
        col-width 25
        pad "  "
        filler (apply str (repeat col-width \space))
        feeders (map #(column-feeder (str "resources/" % ".txt") col-width)
                  filenames)
        wnew #(.newLine out)
        wout (fn [x]
               (.write out x))]
      (go-loop []
        (let [chunks (loop [[f & rf] feeders
                            taken []]
                       (if (nil? f)
                         taken
                         (recur rf (conj taken (<! f)))))]
          (cond
            (every? nil? chunks)
            (do
              (wnew)
              (wout "The End")
              (.close out))
          
            :default
            (do 
              (doseq [c chunks]
                (wout (or (when c (str/replace c \newline \!)) filler))
                (wout pad))
              (wnew)
              (recur)))))
      nil))

#_
(let [out (chan)
      words (range 5)]
  (go
    (doseq [c words]
      (>! out c))
    (close! out))

  (go-loop []
    (let [c (<! out)]
      (if (nil? c)
        (do
          (println :fini)
          (println :again (<! out)))
        (do
          (println c)
          (recur)))))
  :eoj)
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

