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

(defn pop-stack []
  ["@SP"
   "A=M"
   "A=A-1"
   "D=A"
   "@SP"
   "M=D"])

(defn pop-and-store [addr]
  (concat (pop-stack) ["A=D"
                       "D=M"
                       (str "@" addr)
                       "M=D"]))

(defn push-data-onto-stack []
  ["@SP"
   "A=M"
   "M=D"
   "A=A+1"
   "D=A"
   "@SP"
   "M=D"])

(defn add [x y]
  [(str "@" x)
   "D=M"
   (str "@" y)
   "D=D+M"])

(defn translate [[command & args]]
  (cond
    (= "push" command)
    (concat [(str "@" (last args))
             "D=A"]
            (push-data-onto-stack))

    (= "add" command)
    (concat (pop-and-store "R14")
            (pop-and-store "R13")
            (add "R13" "R14")
            (push-data-onto-stack))

    :else
    [command]))

(defn -main []
  (let [vm-commands (line-seq (java.io.BufferedReader. *in*))
        parsed (parse-commands vm-commands)]
    (doseq [command parsed]
      (let [asm-lines (translate command)]
        (doseq [asm asm-lines]
          (println asm))))))
