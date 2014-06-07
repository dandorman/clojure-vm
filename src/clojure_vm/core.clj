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
   "M=D"
   "A=D"
   "D=M"])

(defn pop-and-store [addr]
  (concat (pop-stack) [(str "@" addr)
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

(defn _compare [op x y i]
  [(str "@" x)
   "D=M"
   (str "@" y)
   "D=D-M"
   (str "@" op "_" i)
   (str "D;J" op)
   "D=0"
   (str "@" op "_" i "_END")
   "0;JMP"
   (str "(" op "_" i ")")
   "D=-1"
   (str "(" op "_" i "_END)")])

(defn eq [x y i]
  (_compare "EQ" x y i))

(defn gt [x y i]
  (_compare "GT" x y i))

(defn lt [x y i]
  (_compare "LT" x y i))

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

(def segments {"local"    "LCL"
               "argument" "ARG"
               "this"     "THIS"
               "that"     "THAT"
               "temp"     "R5"})

(defn reference-segment [segment]
  (if (= "temp" segment)
    [(str "@" (get segments segment))]
    [(str "@" (get segments segment))
     "A=M"]))

(defn handle-push [segment index]
  (cond
    (= "constant" segment)
    (concat [(str "@" index)
            "D=A"]
            (push-data-onto-stack))

    (= "pointer" segment)
    (cond
      (= "0" index)
      (concat ["@THIS"
               "D=M"]
              (push-data-onto-stack))

      (= "1" index)
      (concat ["@THAT"
               "D=M"]
              (push-data-onto-stack)))

    (some (set (keys segments)) [segment])
    (concat [(str "@" index)
             "D=A"]
            (reference-segment segment)
            ["A=D+A"
             "D=M"]
            (push-data-onto-stack))))

(defn handle-pop [segment index]
  (cond
    (= "pointer" segment)
    (cond
      (= "0" index)
      (pop-and-store "THIS")

      (= "1" index)
      (pop-and-store "THAT"))

    (some (set (keys segments)) [segment])
    (concat (pop-and-store "R13")
            [(str "@" index)
             "D=A"]
            (reference-segment segment)
            ["D=D+A"
             "@R14"
             "M=D"
             "@R13"
             "D=M"
             "@R14"
             "A=M"
             "M=D"])))

(defn translate [[[command & args] i]]
  (cond
    (= "push" command)
    (let [[segment index] args]
      (handle-push segment index))

    (= "pop" command)
    (let [[segment index] args]
      (handle-pop segment index))

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
