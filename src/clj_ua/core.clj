(ns clj-ua.core
  (:require [clojure.string :as string]))

(import java.util.Arrays)

(defrecord Browser [name version])
(defrecord Platform [name version])
(defrecord UserAgent [#^Browser browser
                      #^Platform platform])

(def unknown-user-agent
  (UserAgent. (Browser. "unknown" "unknown") (Platform. "unknown" "unknown")))

(defn- log 
  [data]
  (prn (merge {:ns "clj-ua"} data)))

(defn- parse-version-from-comment 
  [comment]
  (let [version (re-find #"([0-9]+)[[_.]([A-Za-z0-9]+)]+" comment)]
    (if (nil? version)
      ; then
      (let [os-x (re-find #"OS X" (.toUpperCase comment))]
        (if (not (nil? os-x))
          os-x))
      ; else
      (.replaceAll (nth version 0) "_" "."))))

(defn- handle-comment 
  [parts]
  (if (and (not (nil? parts)) (< 0 (alength parts)))
    (let [part (nth parts 0)]
      (if (< 1 (.length part))
        (Platform. part (parse-version-from-comment part))
        (handle-comment (Arrays/copyOfRange parts 1 (count parts)))))))

(defn- parse-browser 
  "Get the browser information from the parsed user agent string."
  [seq]
  (let [part (last seq)]
    (if (or (nil? part) (.equals (nth part 0) ""))
      (parse-browser (subvec seq 0 (- (count seq) 1))) ;subvec is exclusive of the end
      (Browser. (nth part 1) (nth part 3)))))

(defn- parse-platform 
  "Get the platform information from the parsed user agent string."
  [seq]
  (let [part (first seq)]
    (if (< 6 (count part))
      ; then
      (let [comment (nth part 6)]
        (if (not (nil? comment))
          (let [parts (.split comment "; ")]
            (if (< 1 (alength parts))
              (handle-comment (Arrays/copyOfRange parts 1 (count parts)))
              (handle-comment parts)))))
      ; else
      (parse-platform (rest seq)))))

(defn parse 
  "Parse a user agent string and return a UserAgent record containing browser
   and platform entries."
  [#^String agent]
  #_(log {:fn "parse" :agent agent})
  (if (and (not (nil? agent)) (not (.equals agent "")))
    (let [seq (vec (re-seq #"([^/\s]*)(/([^\s]*))?(\s*\[[a-zA-Z][a-zA-Z]\])?\s*(\((([^()]|(\([^()]*\)))*)\))?\s*" agent))]
      (UserAgent. (parse-browser seq) (parse-platform seq)))
    unknown-user-agent))

(defn -main [& args]
  (if args
    (let [agent (nth args 0)]
      (parse agent))
    (prn "Please pass a user-agent to parse.")))
