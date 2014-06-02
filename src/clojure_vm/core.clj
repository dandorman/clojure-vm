(ns clojure-vm.core)

(defn comment? [command]
  (re-find #"^//" command))

(defn whitespace? [command]
  (re-find #"^\s*$" command))

(defn parse [parsed command]
  (cond
    (or (comment? command) (whitespace? command))
    parsed

    :else
    (conj parsed (clojure.string/split command #"\s+"))))

(defn parse-commands
  ([commands]
   (parse-commands commands []))
  ([commands parsed]
   (if (empty? commands)
     parsed
     (parse-commands (rest commands)
                     (parse parsed (first commands))))))

(defn translate [[command & args]]
  (cond
    (= "push" command)
    [(str "@" (last args))
     "D=A"
     "@SP"
     "A=M"
     "M=D"
     "A=A+1"
     "D=A"
     "@SP"
     "M=D"]

    (= "add" command)
    ["@SP"
     "A=M"
     "A=A-1"
     "D=A"
     "@SP"
     "M=D"
     "A=D"
     "D=M"
     "@R14"
     "M=D"

     "@SP"
     "A=M"
     "A=A-1"
     "D=A"
     "@SP"
     "M=D"
     "A=D"
     "D=M"
     "@R13"
     "M=D"

     "@R13"
     "D=M"
     "@R14"
     "D=D+M"
     "@SP"
     "A=M"
     "M=D"
     "A=A+1"
     "D=A"
     "@SP"
     "M=D"]

    :else
    [command]))

(defn -main []
  (let [vm-commands (line-seq (java.io.BufferedReader. *in*))
        parsed (parse-commands vm-commands)]
    (doseq [command parsed]
      (let [asm-lines (translate command)]
        (doseq [asm asm-lines]
          (println asm))))))
