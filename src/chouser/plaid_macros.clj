(ns chouser.plaid-macros
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defmacro defdraw [& fn-tail]
  (let [code-meta (meta &form)
        {:keys [file line end-line]} code-meta
        text (str/join "\n"
                       (->> (slurp file)
                            (.split #"\n")
                            (drop (dec line))
                            (take (- end-line line -1))))]
    `(do
       (swap! chouser.plaid/app-state assoc
              :code-meta '~(meta &form)
              :code '~(str/split text #"(?<=\n)"))
       (defn ~'draw ~@fn-tail))))

(defn meta-ize [form x args]
  `(~x ~(first args) ~(meta form) ~@(rest args)))

(defmacro rect [& args] (meta-ize &form 'chouser.plaid/rect* args))
(defmacro v  [& args] (meta-ize &form 'chouser.plaid/v* args))
(defmacro vc [& args] (meta-ize &form 'chouser.plaid/vc* args))
(defmacro h  [& args] (meta-ize &form 'chouser.plaid/h* args))
(defmacro hc [& args] (meta-ize &form 'chouser.plaid/hc* args))
(defmacro r  [& args] (meta-ize &form 'chouser.plaid/r* args))
(defmacro rc [& args] (meta-ize &form 'chouser.plaid/rc* args))
