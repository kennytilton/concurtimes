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
(-main "-w80" "-s4" "-t2" "LICENSE")

(defn -main [& args]
  #_ ;; uncomment during development so errors get through when async in play
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
            (format "We will need a width of at least %d for %d files and %d spacing."
              min-width fct spacing))

          :default (print-in-columns filepaths width spacing))))

    #_(pp/pprint input)))


(defn file-found? [path]
  (or (.exists (io/as-file path))
    (do
      (println (format "\nSuggested file <%s> not found.\n" path))
      false)))

#_
(-main "-w40" "-t2" "LICENSE")

(defn print-in-columns [filepaths page-width col-spacing]
  (when-not (empty? filepaths)
    (try
      (let [
            file-ct (count filepaths)
            col-width (int (Math/floor (/ (- page-width
                                            (* (dec file-ct) col-spacing))
                                         file-ct)))
            col-pad (apply str (repeat col-spacing \space))
            filler (apply str (repeat col-width \space))
            channels (repeat (count file-paths) (chan))
            futures (map #(future
                            (column-feeder-ex %1 %2 col-width))
                      channels filepaths)
            ]

          (loop []
            (let [chunks (loop [[c & rc] channels taken []]
                           ;; asynch does not play well with fns or even FORs
                           ;; so we have to gather in LOOP
                           (if c
                             (recur rf (conj taken (<!! c)))
                             taken))]
              (cond
                (every? nil? chunks)
                (println "\nThe End\n")

                :default
                (do
                  (println (str/join col-pad (map #(or % filler) chunks)))
                  (recur))))))

        (catch Exception e
          (println "Uh-oh...")))))

#_
(Thread/setDefaultUncaughtExceptionHandler
     (reify Thread$UncaughtExceptionHandler
       (uncaughtException [_ thread ex]
         (log/error {:what :uncaught-exception
                     :exception ex
                     :where (str "Uncaught exception on" (.getName thread))}))))

(defn column-feeder-ex
  "Return a channel from which an output function
can pull left-justified column lines of width <col-width>
extracted from the file at <filepath>."
  [out filepath col-width]
  (println :cfex filepath)
  (let [filler (apply str (repeat col-width \space))
        rdr (clojure.java.io/reader filepath)]

    (loop [
           ;; valid states:
           ;; :nl - at start of line
           ;; :tx - in the middle of non-whitespace text
           ;; :ws - in the middle of whitespace
           state :nl
           column 0
           last-ws-col nil
           buffer ""]

      (let [pad-out (fn [b]
                      ;; we cannot put to the channel from inside
                      ;; a function (async limitation) so just pad
                      (subs (str b filler) 0 col-width))]
        (cond
          (>= column col-width) ;; > should not occur, but...
          (condp = state
            :ws (do
                  (>!! out (pad-out buffer))
                  (recur :nl 0 nil ""))
            :tx (if last-ws-col
                  (do
                    (>!! out (pad-out
                              (subs buffer 0 (inc last-ws-col))))
                    (let [new-buffer (str/triml (subs buffer last-ws-col))]
                      (recur :tx (count new-buffer) nil new-buffer)))
                  ;; whoa, big word. Hyphenate badly...
                  ;; TODO: hyphenate better
                  (do
                    (>!! out (pad-out (str (subs buffer 0 (dec (count buffer))) \-)))
                    (recur :tx 1 nil (str (last buffer))))))

          :default
          (let [c (.read rdr)]
            (condp = c
              -1 (do
                   (when (pos? (count buffer))
                     (>!! out (pad-out buffer)))
                   (close! out)
                   (.close rdr))

              10 (do (>!! out (pad-out buffer))
                     (recur :nl 0 nil ""))

              13 ;; maybe on Windows?
              (do (>!! out (pad-out buffer))
                  (recur :nl 0 nil ""))

              9 ;; TODO: allow new TAB width parameter and space intelligently
              (recur :ws (inc column) column (str buffer \space))

              32 (if (= state :nl)
                   (recur :nl 0 nil "")
                   (recur :ws (inc column) column (str buffer \space)))

              (recur :tx (inc column) last-ws-col
                (str buffer
                  (if (< c 32)
                    \space (char c)))))))))
    out))
