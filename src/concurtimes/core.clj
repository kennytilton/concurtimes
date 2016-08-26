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

#_
(let [files [
             "its-in-the-hole"
             "mobydick"
             "gettysburgh"
             "cells-manifesto"
             ;;"obama-wright"
             ]]
  (apply go-cols 80 2 files))

(defn go-cols [w p & filenames]
  (let [
        col-spacing (or p 2)
        file-ct (max 1 (count filenames)) ;; avoid div by zero if no files
        col-width (int (Math/floor (/ (- (or w 132)
                                        (* (dec file-ct) col-spacing))
                                     file-ct)))
        col-pad (apply str (repeat col-spacing \space))
        filler (apply str (repeat col-width \space))
        feeders (map #(column-feeder (str "resources/" % ".txt") col-width)
                  filenames)
        wnew #(print \newline) ;; #(.newLine out)
        wout (fn [x] (println x)
               #_(.write out x))
        ]
    
    (go-loop []
      (let [chunks (loop [[f & rf] feeders taken []]
                     ;; asynch does not play well with fns or even FORs
                     ;; so we have to gather in LOOP
                     (if (nil? f) taken
                         (recur rf (conj taken (<! f)))))]
        (cond
          (every? nil? chunks)
          (do
            (wnew)
            (wout "The End"))
          
          :default
          (do 
            (wout (str/join col-pad (map #(or % filler) chunks)))
            (recur)))))
    nil))


#_
(defn -main [& args]
  (let [cli (parse-opts args cli-options)]
    (pp/pprint cli)))


(defn -main [& args]
  (let [[opts args banner]
        (cli args
          ["-h" "--help" "Usage: (concurtimes [<tex-file-path>)"
           :default false :flag true])]
    (println :opts opts)
    (println :args args)
    (println :banner banner)
    #_
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

