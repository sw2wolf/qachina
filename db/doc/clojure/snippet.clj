
;;;
;; project_classpath=`lein classpath`
;; echo $project_classpath
;; java -cp $project_classpath  clojure.main -i ./src/foreclojure/mongo.clj -e "(use 'foreclojure.mongo) (prepare-mongo) (shutdown-agents)"

;; #!/bin/bash
;; java -cp `lein classpath` clojure.main -e \
;;     "(do (require 'foreclojure.core) (foreclojure.core/-main))"

;;;
(defn get-list [start]
    (loop [n start]
       (let [url (str "http://www.pythonchallenge.com/pc/def/linkedlist.php?nothing=" n)
              data (first (line-seq (clojure.java.io/reader url)))
              target (re-seq #"\d+" data)]
            (println url data target)
            (recur (first target)))))
;一组java对象的seq
(repeatedly 100 #(SomeClass. ))

;;;
;Multiple Each Item in a List by 2
(map #(* % 2) (range 1 11))

;Sum a List of Numbers
(reduce + (range 1 1001))

;Verify if Word Exists in a String
;I used a regex here, because I believe that’s the right way to do it.
(def tweet "This is an example tweet talking about clojure and emacs.")
(def regex (re-pattern (apply str (interpose "|" ["clojure" "logic" "compojure" "emacs" "macros"]))))
(re-seq regex tweet) ; Gives me the actual matches instead of just true/false

;As suggested by a commentator, this problem can be solved without using regex by leveraging Clojure’s sets.
(def tweet "This is an example tweet talking about clojure and emacs.")
(def is-word? (set ["clojure" "logic" "compojure" "emacs" "macros"]))
(not (nil? (some is-word? (.split tweet " ")))) ; Returns true/false

;Read a File
(def file-text (slurp "data.txt")) ; Reads the whole file
(def file-lines (clojure.contrib.io/read-lines "data.txt")) ; Reads as a sequence of lines

;Since Clojure Contrib has been deprecated for future Clojure releases, clojure.contrib.io/read-lines can be rewritten as (line-seq (clojure.java.io/reader (clojure.java.io/file “data.txt”))) in Clojure 1.3 onwards. Thanks to Aaron for pointing it out.

;Happy Birthday to You
(doseq [l (map #(str "Happy Birthday " (if (= % 2) "dear Rich" "to You")) (range 4))] (println l))
;Alternate version -
(dotimes [n 4] (println "Happy Birthday " (if (= n 2) "dear Rich" "to You")))

;Filter List of Numbers
(partition-by #(> % 60) [49 58 76 82 88 90])

;Fetch and Parse XML Web Service
(clojure.xml/parse "http://search.twitter.com/search.atom?&q=clojure")

;Find Maximum (or Minimum) in a List
(reduce max [14 35 -7 46 98])
(reduce min [14 35 -7 46 98])
;; Now both together
((juxt #(reduce max %) #(reduce min %)) [14 35 -7 46 98]) ; Returns [98 -7]

;Parallel Processing
;; Assuming process-line to be a CPU intensive function that operates on a line
(pmap process-line lines) ; Note the "p" in front of map

;Sieve of Eratosthenes
;I don’t I have a sufficiently good (in terms of performance & beauty) one line implementation of SoE. I would recommend checking out Christophe Grand’s treatise on the subject titled Everybody loves the Sieve of Eratosthenes for a great discussion on writing real world prime sieves in Clojure.

;Solve FizzBuzz
(map #(cond (zero? (mod % 15)) "FizzBuzz" (zero? (mod % 3)) "Fizz" (zero? (mod % 5)) "Buzz" :else %) (range 1 101))`

;;;
;协议是一个功能强大的工具：它们有效地为您提供将新方法插入现有类的能力，而没有名称冲突且无需修改原始代码。
(defprotocol IVec2D
    (vadd [this v] "produces 2d vector sum of given vectors"))
   
(defrecord Vec2D [^double x ^double y]
    IVec2D
    (vadd [this v] (Vec2D. (+ x (:x v)) (+ y (:y v)))))
        
(defrecord VerletParticle2D [^double x ^double y ^double mass behaviors constraints]
    IVec2D
    (vadd [this v]
        (VerletParticle2D. (+ x (:x v)) (+ y (:y v)) mass behaviors constraints)))
                
(def v (Vec2D. 23 42))
(def p (VerletParticle2D. 11 22 3 [] []))
                    
(vadd v p) ; => #:user.Vec2D{:x 34.0, :y 64.0}
(vadd p v) ; => #:user.VerletParticle2D{:x 34.0, :y 64.0, :mass 3.0, :behaviors [], :constraints []}
                         
;The other option I thought of is using assoc instead of explicitly
;creating new instances and then injecting these implementations via
;the extend function:
                              
(def IVec2D-impl {
    :vadd (fn [this v]
        (assoc this :x (+ (:x this) (:x v)) :y (+ (:y this) (:y v)))) })
                                    
(defrecord Vec2D [^double x ^double y])
(defrecord VerletParticle2D [^double x ^double y ^double mass behaviors constraints])
                                         
(extend Vec2D IVec2D IVec2D-impl)
(extend VerletParticle2D IVec2D IVec2D-impl)

;;;;;;
(reduce conj #{} (for [i (range 10) j (range 10)] [i j]))

(defn serializable? [v]
    (instance? java.io.Serializable v))

(defn serialize
    "Serializes value, returns a byte array"
    [v]
    (let [buff (java.io.ByteArrayOutputStream. 1024)]
        (with-open [dos (java.io.ObjectOutputStream. buff)]
            (.writeObject dos v))
        (.toByteArray buff)))

(defn deserialize 
  "Accepts a byte array, returns deserialized value"
    [bytes]
    (with-open [dis (java.io.ObjectInputStream.  (java.io.ByteArrayInputStream. bytes))]
        (.readObject dis)))

(use '[clojure.contrib.io :only (to-byte-array input-stream)])
(import '[java.security MessageDigest]
        '[java.io StringWriter]
        '[org.apache.commons.codec.binary Base64])
(require '[clojure.contrib.base64 :as base64])
(def md5sum (..
    (java.security.MessageDigest/getInstance "MD5")
    (digest (to-byte-array "foobar"))))

(let [output (StringWriter.)]
    (base64/encode (input-stream md5sum) output base64/*base64-alphabet* nil)
    (.toString output))

;;;;;;
1: how to read an entire file into memory.

(slurp "/tmp/test.txt")

Not recommended when it is a really big file.

2: how to read a file line by line.

(use 'clojure.java.io)
(with-open [rdr (reader "/tmp/test.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))

3: how to write to a new file.

(use 'clojure.java.io)
(with-open [wrtr (writer "/tmp/test.txt")]
  (.write wrtr "Line to be written"))

4: append a line to an existing file.

(use 'clojure.java.io)
(with-open [wrtr (writer "/tmp/test.txt" :append true)]
  (.write wrtr "Line to be appended"))

;;;;;;
If the file fits into memory you can read and write it with slurp and spit:
(def s (slurp "filename.txt"))
(s now contains the content of a file as a string)

(spit "newfile.txt" s)
This creates newfile.txt if it doesnt exit and writes the file content. If you want to append to the file you can do
(spit "filename.txt" s :append true)

To read or write a file linewise you would use Java's reader and writer. They are wrapped in the namespace clojure.java.io:

(ns file.test
  (:require [clojure.java.io :as io]))

(let [wrtr (io/writer "test.txt")]
  (.write wrtr "hello, world!\n")
  (.close wrtr))

(let [wrtr (io/writer "test.txt" :append true)]
  (.write wrtr "hello again!")
  (.close wrtr))

(let [rdr (io/reader "test.txt")]
  (println (.readLine rdr))
  (println (.readLine rdr)))

(defn sum-by-type [in]
  (->> in
    (group-by #(get % "Type"))
    (map (fn [[k vs]]
      {"Type" k 
       "Value" (reduce (fn [acc v] (+ acc (Double/parseDouble (get v "Value")))) 0 vs)
      }))))

(require 'clojure.java.io)
(defn load-props
  [file-name]
  (with-open [^java.io.Reader reader (clojure.java.io/reader file-name)] 
    (let [props (java.util.Properties.)]
      (.load props reader)
      (into {} (for [[k v] props] [(keyword k) (read-string v)])))))

(load-props "test.properties")

(ns sqlitetest
    (:use [clojure.java.jdbc]))
(def db { :classname "org.sqlite.JDBC"
          :subprotocol "sqlite"
          :subname "E:/temp/chinook.db"})

(defmacro get-sql-metadata [db method & args]
    `(with-connection ~db 
        (doall 
            (resultset-seq (~method 
                (.getMetaData (connection)) ~@args)))))

;So now you can call the metadata with the metadata method and its own parameters like so:
(get-sql-metadata db .getTables nil nil nil (into-array ["TABLE" "VIEW"]))
;or
(get-sql-metadata db .getColumns nil nil nil nil)

;;;;;;
(into {} (java.util.HashMap. {"foo" "bar" "baz" "quux"}))

(into {}
  (map (juxt
      #(keyword (key %))
      #(val %))
  (java.util.HashMap. {"foo" "bar" "baz" "quux"})))

=>{:baz "quux", :foo "bar"}

;;;;;;
(defn timed-agent [limit f]
  (let [a (agent 0)
        t (java.util.Timer.)
        tt (proxy [java.util.TimerTask] [] (run [] (send-off a (fn [v] (f) (inc v)))))]
    (set-validator! a #(> limit %))
    (.scheduleAtFixedRate t tt 1000 1000) a))
  
(await (timed-agent 20 #(println "running")))

(rand-int 100) ; 得到一个[0 100) 的随机数
(rand-nth (range 100)) ; 每次运行结果会变

(with-precision 10 (/ 1M 3)) ; 0.3333333333M

(subs "12345" 1) ; "2345" 相当于 (.substring "12345" 1)
(subs "12345" 1 3) ; "23"相当于 (.substring "12345" 1 3)

(apply str (interpose " + " [1 2 3 4 5])) ; "1 + 2 + 3 + 4 + 5"
(interleave "abc" "123") ; (\a \1 \b \2 \c \3)
(apply str (interleave "abc" "123")) ; "a1b2c3"

(seq "hello") ; (\h \e \l \l \o)
(vec "hello") ; [\h \e \l \l \o]
(vector "hello") ; ["hello"]

(loop [i 1] (if (< i 5) (println i) (recur (inc i)))) ; 错误 仅打印1
(loop [i 1] (if (< i 5) (do (println i) (recur (inc i))))) ; 正确
(loop [i 1] (when (< i 5) (println i) (recur (inc i)))) ; 正确 when把条件判断后的所有都执行

(defn jumper [x]
  (case x
    "JoC"   :a-book
    "Fogus" :a-boy
    :eggs   :breakfast
    42      (+ x 100)
    [42]    :a-vector-of-42
    "The default"))

(dotimes [i 5] (println i)) ; 打印 0 1 2 3 4
(doseq [i (range 5)] (println i))
(dorun (map println [2 4 6 8]))

(def t1 (Thread. (proxy [Runnable] [] (run [] (println "hello")))))
; [Runnable]是接口类列表 []是接口构造函数的参数
(.start t1)
也可直接：
(def t1 (Thread. #(println "hello")))

Clojure中调用系统shell命令：
(use '[clojure.java.shell :only [sh]])
 (sh "java")
 (sh "java" "-version")

从Clojure中调用web浏览器访问指定url：
(use 'clojure.java.browse)
(browse-url "http://clojuredocs.org")

调用web浏览器查看在线java类或者对象的javadoc：
(javadoc String)
(def d (java.util.Date.))
(javadoc d)

(import 'java.util.concurrent.Executors)
(import 'java.util.concurrent.TimeUnit)

(.scheduleAtFixedRate (Executors/newScheduledThreadPool 1) 
  #(println "Hello") 0 5 TimeUnit/SECONDS)

(last 
  (for [i (range 1000) 
        j (range 1000)
        :let [n (* i j)]
        :when (and (= (mod n 13) 0) 
                   (= (mod i 7) 0))]
    n))

(set! *warn-on-reflection* true)

这个函数随机地产生n个从1到m的整数数组：

(defn rnm [n m]
    "Generates a n-size int array. The number ranges from 1 to m [1, m]."
    (vec (repeatedly n #(+ 1 (rand-int m)))))

(bit-shift-left 2 38)
