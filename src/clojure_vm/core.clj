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

(defmulti handle-push (fn [segment _] segment))
(defmethod handle-push "constant" [_ index]
  (concat [(str "@" index)
           "D=A"]
          (push-data-onto-stack)))
(defmethod handle-push "local" [_ index]
  (concat [(str "@" index)
           "D=A"
           "@LCL"
           "A=M"
           "A=D+A"
           "D=M"]
          (push-data-onto-stack)))
(defmethod handle-push "argument" [_ index]
  (concat [(str "@" index)
           "D=A"
           "@ARG"
           "A=M"
           "A=D+A"
           "D=M"]
          (push-data-onto-stack)))
(defmethod handle-push "this" [_ index]
  (concat [(str "@" index)
           "D=A"
           "@THIS"
           "A=M"
           "A=D+A"
           "D=M"]
          (push-data-onto-stack)))
(defmethod handle-push "that" [_ index]
  (concat [(str "@" index)
           "D=A"
           "@THAT"
           "A=M"
           "A=D+A"
           "D=M"]
          (push-data-onto-stack)))
(defmethod handle-push "temp" [_ index]
  (concat [(str "@" index)
           "D=A"
           "@R5"
           "A=D+A"
           "D=M"]
          (push-data-onto-stack)))

(defmulti handle-pop (fn [segment _] segment))
(defmethod handle-pop "local" [_ index]
  (concat (pop-and-store "R13")
          [(str "@" index)
           "D=A"
           "@LCL"
           "A=M"
           "D=D+A"
           "@R14"
           "M=D"
           "@R13"
           "D=M"
           "@R14"
           "A=M"
           "M=D"]))
(defmethod handle-pop "argument" [_ index]
  (concat (pop-and-store "R13")
          [(str "@" index)
           "D=A"
           "@ARG"
           "A=M"
           "D=D+A"
           "@R14"
           "M=D"
           "@R13"
           "D=M"
           "@R14"
           "A=M"
           "M=D"]))
(defmethod handle-pop "this" [_ index]
  (concat (pop-and-store "R13")
          [(str "@" index)
           "D=A"
           "@THIS"
           "A=M"
           "D=D+A"
           "@R14"
           "M=D"
           "@R13"
           "D=M"
           "@R14"
           "A=M"
           "M=D"]))
(defmethod handle-pop "that" [_ index]
  (concat (pop-and-store "R13")
          [(str "@" index)
           "D=A"
           "@THAT"
           "A=M"
           "D=D+A"
           "@R14"
           "M=D"
           "@R13"
           "D=M"
           "@R14"
           "A=M"
           "M=D"]))
(defmethod handle-pop "temp" [_ index]
  (concat (pop-and-store "R13")
          [(str "@" index)
           "D=A"
           "@R5"
           "D=D+A"
           "@R14"
           "M=D"
           "@R13"
           "D=M"
           "@R14"
           "A=M"
           "M=D"]))

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
