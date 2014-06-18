(ns pom-sexp.core
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :refer [postwalk]]))

;;; maven pom cleanup experiment

(def tpom (xml/parse (io/reader "/home/ian/src/learning-clojure/traversals/pom.xml")))

;;; TODO Compare with following `case' version
;; (defmulti xml->sexp class)
;; (defmethod xml->sexp java.lang.String [s] s)
;; (defmethod xml->sexp clojure.data.xml.Element [e]
;;   (cons (symbol (name (:tag e))) (map xml->sexp (:content e))))

(defn xml->sexp [x]
  (let [c (-> x class (.getName) symbol)]
    (case c
        java.lang.String x
        clojure.data.xml.Element (cons (symbol (name (:tag x)))
                                       (map xml->sexp (:content x))))))
(def spom (xml->sexp tpom))
;; postwalk translation function
(defn safe-first [x] (and (seq? x) (first x)))
(defmulti clean-pom safe-first)
(defmethod clean-pom :default [x] x)
;;(defmethod clean-pom 'project [x])

(defn split-with-syms [syms seq-of-pairs]
  [;; we must filter out non-leaf nodes so into {} doesn't explode
   (into {} (map vec (filter #(= 2 (count %)) seq-of-pairs)))
   (filter #((complement (set syms)) (first %)) seq-of-pairs)])

;; (defmacro with-leaf-bindings [[leaves tree] & body]
;;   (let [[leaves tree] [`(~@leaves) (eval tree)]
;;         [leaf-map rem] (split-with-syms leaves tree)
;;         bs (->> leaves (select-keys leaf-map) (into []) flatten vec)]
;;     `(let ~(conj bs 'rest-tree `(quote ~rem)) ~@body)))

(defmacro with-leaf-bindings
  "anaphora: rest-tree"
  [[leaves tree] & body]
  (let [lm (gensym "leaf-map")
        bs `[[~lm ~'rest-tree] (split-with-syms (quote~leaves) ~tree)]
        bs (reduce #(conj % %2 `(~lm (quote ~%2))) bs `(~@leaves))]
    `(let ~bs ~@body)))

;; (defmethod clean-pom 'dependency [x]
;;   (let [[dm rem] (split-with-syms '(groupId artifactId version) (rest x))
;;         abbrev [(str (dm 'groupId) "/" (dm 'artifactId)) (dm 'version)]]
;;     (if (not-empty rem)
;;       (cons abbrev rem)
;;       abbrev)))

;;; now using with-leaf-bindings
(defmethod clean-pom 'dependency [x]
  (with-leaf-bindings [[groupId artifactId version] (rest x)]
    (let [abbrev [(str groupId "/" artifactId) version]]
      (if (not-empty rest-tree)
        (cons abbrev rest-tree)
        abbrev))))

(defn get-excls [tree] (->> tree (filter #(= 'exclusions (first %))) first))
(defn clean-exclusions [excls]
  (->> (rest excls)
       (map rest)
       (map #(->> % (map vec) (into {})))
       (map #(str (% 'groupId) "/" (% 'artifactId)))
       vec))

(defmethod clean-pom 'dependency [x]
  (with-leaf-bindings [[groupId artifactId version scope] (rest x)]
    (let [abbrev [(str groupId "/" artifactId) version]
          excls  (clean-exclusions (get-excls rest-tree))
          abbrev (if (not-empty excls) (conj abbrev :exclude excls) abbrev)
          abbrev (if scope [(keyword scope) abbrev] abbrev)]
      ;; (if (not-empty rest-tree)
      ;;   (cons abbrev rest-tree)
      ;;   abbrev)
      abbrev
      )))

(def depsex '(dependencies
              ["org.clojure/clojure" "1.6.0"]
              ["org.clojure/data.xml" "0.0.6"]
              [:test
               ["org.clojure/tools.nrepl"
                "0.2.3"
                :exclude
                ["org.clojure/clojure"]]]
              [:test
               ["clojure-complete/clojure-complete"
                "0.2.3"
                :exclude
                ["org.clojure/clojure"]]]))

(defmethod clean-pom 'dependencies [x]
  (let [test? (fn [t] (= :test (first t)))
        deps (filter (complement test?) (rest x))
        testdeps (filter test? (rest x))]
    (concat ['dependencies] deps [(cons 'test (mapcat rest testdeps))])))

(defmethod clean-pom 'project [x]
  (with-leaf-bindings [[groupId artifactId version name description url] (rest x)]
    (let [pinfo (str name " - " description " (" url ")")
          vinfo [(str groupId "/" artifactId) version]]
      ;;(concat `(~'project ~pinfo ~vinfo) rest-tree)
      (list* 'project (list 'about pinfo) (list 'gav vinfo) rest-tree))))

(defn kinder [ks k] (if (= "true" (eval (second (second k)))) (conj ks (first k)) ks))
(defmethod clean-pom 'repository [x]
  (with-leaf-bindings [[id url] (rest x)]
    (let [;;kinds (if (= "true" (second snapshots)) (conj kinds 'snapshots) kinds)
          ;;kinds (if (= "true" (second releases))  (conj kinds 'releases) kinds)
          kinds (reduce kinder [] rest-tree)]
      [(str id " - " url) :for kinds])))

(def repex '(repository
             (id "central")
             (url "http://repo1.maven.org/maven2/")
             (snapshots (enabled "false"))
             (releases (enabled "true"))))

(def depex '(dependency
             (groupId "org.clojure")
             (artifactId "tools.nrepl")
             (version "0.2.3")
             (exclusions
              (exclusion (groupId "org.clojure") (artifactId "clojure")))
             (scope "test")))

;;(clean-pom depex)

;; (with-leaf-bindings [[artifactId groupId version] (rest depex)]
;;   {:bs [groupId artifactId version]
;;    :rest rest-tree}
;;   )

(defn pprint-pom
  "pom xml from clojure.data.xml"
  ([pom] (pprint-pom pom *out*))
  ([pom writer]
     (pprint (postwalk clean-pom (xml->sexp pom))
             writer)))

;; (pprint-pom tpom)

(defn write-pom-sexp [pom-xml fname]
  (with-open [pom-sexp-writer (io/writer fname)]
    (pprint-pom pom-xml pom-sexp-writer)))

;; (binding [clojure.pprint/*print-right-margin* 90]
;;   (write-pom-sexp tpom "/home/ian/src/learning-clojure/traversals/pom.sexp"))

