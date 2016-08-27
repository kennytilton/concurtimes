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


(def times-cli
  [["-w" "--width PAGEWIDTH" "Page width in number of characters"
    :default 80
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 2 % 256) "Must be a number from 2 to 256"]]

   ["-s" "--spacing COLUMNSPACING" "Column spacing in number of spaces"
    :default 4
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 0 % 132) "Must be a number from 0 to 232"]]

   ["-t" "--test TESTFILECOUNT" "Number of test files (from ./resources) to use"
    :default 0
    :parse-fn #(Integer/parseInt %)
    :validate [#(>= % 0) "Must be a number from 0 to the number of files in ./resources."]]

   ;; A boolean option defaulting to nil
   ["-h" "--help"]]
  )

(declare column-feeder print-in-columns file-found?)

#_
(-main "-w2" "-t4" "-s2" )

#_
(-main "-w80" "-t3" "-s2" )

(defn -main [& args]
  #_
  (Thread/setDefaultUncaughtExceptionHandler
     (reify Thread$UncaughtExceptionHandler
       (uncaughtException [_ thread ex]
         (log/error {:what :uncaught-exception

                     :exception ex
                     :where (str "Uncaught exception on" (.getName thread))}))))

  (let [input (parse-opts args times-cli)
        {:keys [options arguments summary errors]} input
        {:keys [width spacing test help]} options
        built-ins (let [known (vec (rest (file-seq 
                                           (clojure.java.io/file "./resources"))))]
                    (when (> test (count known))
                      (println (format "\nWarning: only %d test files exist in ./resources\n\n"
                        (count known))))
                    (map #(.getPath %)
                      (subvec known 0 (min test (count known)))))
        filepaths (concat arguments built-ins)]
    
    (cond
      errors (doseq [e errors]
               (println e))

      help (println "\nUsage:\n\n    concurtimes options* files*\n\n"
             "Options:\n" (subs summary 1))

      (empty? filepaths) (do)

      (not (every? file-found? arguments)) (do)

      :default
      (let [fct (count filepaths)
            min-width (+ (* 2 fct)
                        (* (dec fct) spacing))]
        (cond
          (< width min-width)
          (println 
            (format "We will need width of at least %d for %d files and %d spacing."
              min-width fct spacing))

          :default (let [done (chan)]
                     (print-in-columns filepaths width spacing done)
                     (<!! done)))))
    
    #_(pp/pprint input)))

(defn file-found? [path]
  (or (.exists (io/as-file path))
    (do
      (println (format "\nSuggested file <%s> not found.\n" path))
      false)))

#_
(-main "-w80" "-s2" "-t3" "LICENSE")

(defn print-in-columns [filepaths page-width col-spacing done-msg-chan]
  (cond
    ;; this check is now superfluous, but protects the unwitting developer...
    (empty? filepaths) (do
                         (println :sending-done)
                         (>!! done-msg-chan :no-files))
    :default
    (try
      (let [
            file-ct (count filepaths)
            col-width (int (Math/floor (/ (- page-width
                                            (* (dec file-ct) col-spacing))
                                         file-ct)))
            col-pad (apply str (repeat col-spacing \space))
            filler (apply str (repeat col-width \space))
            feeders (for [fp filepaths]
                      (column-feeder fp col-width))
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
                (println "\nThe End\n")
                (>! done-msg-chan true))
              
              :default
              (do 
                (println (str/join col-pad (map #(or % filler) chunks)))
                (recur))))))

      (catch Exception e
        (println :error e)
        (>!! done-msg-chan :abend)))))

(defn column-feeder
  "Return a channel from which an output function
can pull left-justified column lines of width <col-width>
extracted from the file at <filepath>."
  [filepath col-width]

  (let [out (chan)
        filler (apply str (repeat col-width \space))
        rdr (clojure.java.io/reader filepath)]

    (go-loop [state :nl
              column 0
              last-ws nil
              buff ""]

      (let [pad-out (fn [b]
                      ;; we cannot put to the channel from inside
                      ;; a function (async limitation) so just pad
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

              9 ;; TODO: allow new TAB width parameter and space intelligently
              (recur :ws (inc column) column (str buff \space))

              32 (if (= state :nl)
                   (recur :nl 0 nil "")
                   (recur :ws (inc column) column (str buff \space)))

              (recur :tx (inc column) last-ws
                (str buff
                  (if (< c 32) ;; TODO: support Unicode
                    \space (char c)))))))))
    out))
