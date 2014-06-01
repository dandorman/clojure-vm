(ns clojure-vm.core)

(defn comment? [command]
  (re-find #"^//" command))

(defn parse [parsed command]
  (cond
    (comment? command)
    parsed

    :else
    (conj parsed command)))

(defn parse-commands
  ([commands]
   (parse-commands commands []))
  ([commands parsed]
   (if (empty? commands)
     parsed
     (parse-commands (rest commands)
                     (parse parsed (first commands))))))

(defn translate [command]
  command)

(defn -main []
  (let [vm-commands (line-seq (java.io.BufferedReader. *in*))
        parsed (parse-commands vm-commands)]
    (doseq [command parsed]
      (let [asm (translate command)]
        (println asm)))))
