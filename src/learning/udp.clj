(ns learning.udp
  (:refer-clojure :exclude [get])
  (:gen-class))

(defn beget
  "Takes a map and associates its prototype reference to another map, returning a new map"
  [this proto]
  (assoc this ::prototype proto))

(defn get
  "Retrieves values anywhere along the prototype chain in a map"
  [m k]
  (when m
    (if-let [[_ v] (find m k)]
      v
      (recur (::prototype m) k))))

(def put
  "Inserts values on the supplied map"
  assoc)

;; -----------------------------
;; application to oses

(defmulti compiler :os)
(defmethod compiler ::unix [m] (get m :c-compiler))
(defmethod compiler ::osx [m] (get m :llvm-compiler))

(defmulti home :os)
(defmethod home ::unix [m] (get m :home))
(defmethod home ::bsd [m] "/home")
(derive ::osx ::unix)
(derive ::osx ::bsd)
(prefer-method home ::unix ::bsd)

(def clone (partial beget {}))
(def unix {:os ::unix
           :c-compiler "cc"
           :home "/home"
           :dev "/dev"})
(def osx (-> (clone unix)
             (put :os ::osx)
             (put :llvm-compiler "clang")
             (put :home "/Users")))

(defmulti compile-cmd (juxt :os compiler))
(defmethod compile-cmd [::osx "gcc"]
  [m]
  (str "/usr/bin/" (get m :c-compiler)))
(defmethod compile-cmd :default [m]
  (str "Unsure where to locate " (get m :c-compiler)))

(compile-cmd osx)
