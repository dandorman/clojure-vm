(ns clojure-vm.core
  (:require [clojure.java.io :as io])
  (:gen-class))

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

(defn label [identifier]
  [(str "(" identifier ")")])

(defn goto [identifier]
  [(str "@" identifier)
   "0;JMP"])

(defn if-goto [identifier]
  (concat (pop-stack)
          [(str "@" identifier)
           "D;JNE"]))

(def segments {"local"    "LCL"
               "argument" "ARG"
               "this"     "THIS"
               "that"     "THAT"
               "temp"     "R5"})

(def pointers {"0" "THIS"
               "1" "THAT"})

(defn reference-segment [segment]
  (if (= "temp" segment)
    [(str "@" (get segments segment))]
    [(str "@" (get segments segment))
     "A=M"]))

(defn handle-push [segment index file]
  (cond
    (= "constant" segment)
    (concat [(str "@" index)
            "D=A"]
            (push-data-onto-stack))

    (= "pointer" segment)
    (let [pointer (get pointers index)]
      (concat [(str "@" pointer)
               "D=M"]
              (push-data-onto-stack)))

    (= "static" segment)
    (concat [(str "@" file "." index)
             "D=M"]
            (push-data-onto-stack))

    (some (set (keys segments)) [segment])
    (concat [(str "@" index)
             "D=A"]
            (reference-segment segment)
            ["A=D+A"
             "D=M"]
            (push-data-onto-stack))))

(defn handle-pop [segment index file]
  (cond
    (= "pointer" segment)
    (pop-and-store (get pointers index))

    (= "static" segment)
    (pop-and-store (str file "." index))

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

(defn translate [[[command & args] i] file]
  (cond
    (= "push" command)
    (let [[segment index] args]
      (concat [(str "// push " segment " " index)]
              (handle-push segment index file)))

    (= "pop" command)
    (let [[segment index] args]
      (concat [(str "// pop " segment " " index)]
              (handle-pop segment index file)))

    (= "add" command)
    (concat ["// add"]
            (pop-and-store "R14")
            (pop-and-store "R13")
            (add "R13" "R14")
            (push-data-onto-stack))

    (= "sub" command)
    (concat ["// sub"]
            (pop-and-store "R14")
            (pop-and-store "R13")
            (sub "R13" "R14")
            (push-data-onto-stack))

    (= "neg" command)
    (concat ["// neg"]
            (pop-and-store "R13")
            (neg "R13")
            (push-data-onto-stack))

    (= "eq" command)
    (concat ["// eq"]
            (pop-and-store "R14")
            (pop-and-store "R13")
            (eq "R13" "R14" i)
            (push-data-onto-stack))

    (= "gt" command)
    (concat ["// gt"]
            (pop-and-store "R14")
            (pop-and-store "R13")
            (gt "R13" "R14" i)
            (push-data-onto-stack))

    (= "lt" command)
    (concat ["// lt"]
            (pop-and-store "R14")
            (pop-and-store "R13")
            (lt "R13" "R14" i)
            (push-data-onto-stack))

    (= "and" command)
    (concat ["// and"]
            (pop-and-store "R14")
            (pop-and-store "R13")
            (_and "R13" "R14")
            (push-data-onto-stack))

    (= "or" command)
    (concat ["// or"]
            (pop-and-store "R14")
            (pop-and-store "R13")
            (_or "R13" "R14")
            (push-data-onto-stack))

    (= "not" command)
    (concat ["// not"]
            (pop-and-store "R13")
            (_not "R13")
            (push-data-onto-stack))

    (= "label" command)
    (let [identifier (first args)]
      (concat [(str "// label " identifier)]
              (label identifier)))

    (= "goto" command)
    (let [identifier (first args)]
      (concat [(str "// goto " identifier)]
              (goto identifier)))

    (= "if-goto" command)
    (let [identifier (first args)]
      (concat [(str "// if-goto " identifier)]
              (if-goto identifier)))

    :else
    [command]))

(defn -main [_]
  (doseq [filename *command-line-args*]
    (with-open [rdr (io/reader filename)]
      (let [vm-commands (line-seq rdr)
            parsed (parse-commands vm-commands)]
        (doseq [command parsed]
          (let [basename (re-find #"^(?:[^.]+)" (.getName (io/file filename)))
                asm-lines (translate command basename)]
            (doseq [asm asm-lines]
              (println asm))))))))
