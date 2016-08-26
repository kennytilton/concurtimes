(ns concurtimes.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [clojure.core.async
             :refer [go go-loop chan >! <! <!! >!! close!]
             :as async]
            [clojure.string :as str]
            [clojure.pprint :as pp])
  (:gen-class))

#_          
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
                      (subs (str b filler) 0 col-width))]
        (cond
          (>= column col-width)
          (condp = state
            :ws (do
                  (>! out (pad-out buff))
                  (recur :nl 0 nil ""))
            :tx (if last-ws
                  (do
                    (>! out (pad-out
                              (subs buff 0 (inc last-ws))))
                    (let [new-buff (str/triml (subs buff last-ws))]
                      (recur :tx (count new-buff) nil new-buff)))
                  ;; whoa, that is a big word. Hyphenate badly...
                  ;; TODO: hyphenate better
                  (do
                    (>! out (pad-out (str (subs buff 0 (dec (count buff))) \-)))
                    (recur :tx 1 nil (str (last buff))))))
          
          :default
          (let [c (.read rdr)]
            (condp = c
              -1 (do
                   (when (pos? (count buff))
                     (>! out (pad-out buff)))
                   (close! out)
                   (.close rdr))
              10 (do (>! out (pad-out buff))
                     (recur :nl 0 nil ""))
              13 (do (>! out (pad-out buff))
                     (recur :nl 0 nil ""))
              9 (recur :ws (inc column) column (str buff \space))
              32 (if (= state :nl)
                   (recur :nl 0 nil "")
                   (recur :ws (inc column) column (str buff \space)))
              (recur :tx (inc column) last-ws (str buff 
                                                (if (< 32 c 128)
                                                  (char c) \space))))))))
    out))

#_
(-main "-w80" "-t0" "-s2" )

(defn print-in-columns [w p filepaths done]
  (println :entry filepaths (empty? filepaths))
  (cond
    (empty? filepaths) (do
                         (println :sending-done)
                         (>!! done :no-files))
    :default
    (try
      (let [
            col-spacing (or p 2)
            file-ct (count filepaths)
            col-width (int (Math/floor (/ (- (or w 132)
                                            (* (dec file-ct) col-spacing))
                                         file-ct)))
            col-pad (apply str (repeat col-spacing \space))
            filler (apply str (repeat col-width \space))
            feeders (for [fp filepaths]
                      (column-feeder fp col-width))
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
                (println "The End")
                (>! done 42))
              
              :default
              (do 
                (wout (str/join col-pad (map #(or % filler) chunks)))
                (recur))))))
      (catch Exception e
        (println :error e)
        (>!! done :abend)))))


(def times-cli
  [["-w" "--width PAGEWIDTH" "Page width in number of characters"
    :default 80
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 10 % 256) "Must be a number from 10 to 256"]]

   ["-s" "--spacing COLUMNSPACING" "Column spacing in number of spaces"
    :default 4
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 0 % 132) "Must be a number from 0 to 232"]]

   ["-t" "--test TESTFILECOUNT" "Number of test files to use"
    :default 0
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 0 % 5) "Must be a number from 0 to 5"]]

   ;; A boolean option defaulting to nil
   ["-h" "--help"]]
  )

#_
(-main "-w30" "-t0" "-s2" )

(def known ["its-in-the-hole"
             "mobydick"
             "gettysburgh"
             "cells-manifesto"
             "obama-wright"])

(defn -main [& args]
  (Thread/setDefaultUncaughtExceptionHandler
     (reify Thread$UncaughtExceptionHandler
       (uncaughtException [_ thread ex]
         (log/error {:what :uncaught-exception
                     :exception ex
                     :where (str "Uncaught exception on" (.getName thread))}))))

  (let [input (parse-opts args times-cli)
        {:keys [options arguments summary errors]} input
        {:keys [width spacing test help]} options
        filepaths (concat arguments
                    (subvec (vec (for [f known] (str "resources/" f ".txt")))
                      0 test))]
    
    (cond
      errors (doseq [e errors]
               (println e))
      help (println "\nUsage:\n\n    concurtimes options* files*\n\n"
             "Options:\n" (subs summary 1))

      (not (empty? filepaths))
      (do
        (println :top filepaths)

        (let [done (chan)]
            (print-in-columns width spacing filepaths done)
            (println :waiting-on-done)
            (<!! done))))
    
    #_(pp/pprint input)
    ))


