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
    (conj parsed [(clojure.string/split command #"\s+") (count parsed)])))

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

(defn sub [x y]
  [(str "@" x)
   "D=M"
   (str "@" y)
   "D=D-M"])

(defn neg [y]
  [(str "@" y)
   "D=M"
   "D=-D"])

(defn eq [x y i]
  [(str "@" x)
   "D=M"
   (str "@" y)
   "D=D-M"
   (str "@EQ_" i)
   "D;JEQ"
   "D=0"
   (str "@EQ_" i "_END")
   "0;JMP"
   (str "(EQ_" i ")")
   "D=-1"
   (str "(EQ_" i "_END)")])

(defn gt [x y i]
  [(str "@" x)
   "D=M"
   (str "@" y)
   "D=D-M"
   (str "@GT_" i)
   "D;JGT"
   "D=0"
   (str "@GT_" i "_END")
   "0;JMP"
   (str "(GT_" i ")")
   "D=-1"
   (str "(GT_" i "_END)")])

(defn lt [x y i]
  [(str "@" x)
   "D=M"
   (str "@" y)
   "D=D-M"
   (str "@LT_" i)
   "D;JLT"
   "D=0"
   (str "@LT_" i "_END")
   "0;JMP"
   (str "(LT_" i ")")
   "D=-1"
   (str "(LT_" i "_END)")])

(defn _and [x y]
  [(str "@" x)
   "D=M"
   (str "@" y)
   "D=D&M"])

(defn _or [x y]
  [(str "@" x)
   "D=M"
   (str "@" y)
   "D=D|M"])

(defn _not [y]
  [(str "@" y)
   "D=M"
   "D=!D"])

(defn translate [[[command & args] i]]
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

    (= "sub" command)
    (concat (pop-and-store "R14")
            (pop-and-store "R13")
            (sub "R13" "R14")
            (push-data-onto-stack))

    (= "neg" command)
    (concat (pop-and-store "R13")
            (neg "R13")
            (push-data-onto-stack))

    (= "eq" command)
    (concat (pop-and-store "R14")
            (pop-and-store "R13")
            (eq "R13" "R14" i)
            (push-data-onto-stack))

    (= "gt" command)
    (concat (pop-and-store "R14")
            (pop-and-store "R13")
            (gt "R13" "R14" i)
            (push-data-onto-stack))

    (= "lt" command)
    (concat (pop-and-store "R14")
            (pop-and-store "R13")
            (lt "R13" "R14" i)
            (push-data-onto-stack))

    (= "and" command)
    (concat (pop-and-store "R14")
            (pop-and-store "R13")
            (_and "R13" "R14")
            (push-data-onto-stack))

    (= "or" command)
    (concat (pop-and-store "R14")
            (pop-and-store "R13")
            (_or "R13" "R14")
            (push-data-onto-stack))

    (= "not" command)
    (concat (pop-and-store "R13")
            (_not "R13")
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
