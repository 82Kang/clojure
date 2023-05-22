;; This buffer is for text that is not saved, and for Lisp evaluation.
;;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; Localizing helper function in a block
(letfn [(sum [a b] (+ a b))
        (sub [a b] (- a b))]
  (sum 8 7)
  (sub 88 3))

;; nil punning
;; In the template below, it was recommended to use (rest s) instead of (next s)
;; The reason being (rest 2) returns empty sequence instead of nil while the other
;; return nil and never return empty sequence.
;; Functinality wise they both looks fine to me, but not sure of the reasoning.

(defn print-seq [s]
  (when (seq s)
    (prn (first s))
    (recur (rest s))))

(defn print-seq [s]
  (when (seq s)
    (prn (first s))
    (recur (next s))))

;;  The Clojure forms named with do at the start (doseq, dotimes, do, and so on) are
;;  intended for side effects in their bodies and generally return nil as their results.

;; An important point to mention is that it would be best to use doseq to
;; iterate over the collection rather than an explicit recursion

(let [range-vec (vec (range 10))
      [a b c & more :as all] range-vec]
  (println "a b c are:" a b c)
  (println "more is:" more)
  (println "all is:" all))

;; & more -> creates as sequence of the rest of the elements of the collection.
;; :as all -> won't touch the collection, vector stays vector and list as list.


;; Destructuring
;; 1. Positional
;; 2. Map
;; 3. Associative

;; Way to have a default value of a key which isn't present in the map

(def guys-name-map {:f-name "Guy" :m-name "Lewis" :l-name "Steele"})

(let [{:keys [title f-name l-name] :or {title "Mr."}} guys-name-map]
  (println title f-name l-name))

;; Interestingly, these destructuring of maps also work fine with lists
;; But that would cause the list to be pured into a map in order to destructre it

(def name-list '(:f-name "Guy" :l-name "Word"))

(let [{:keys [title f-name l-name] :or {title "Mr."}} name-list]
  (println title f-name l-name))

;; Example of Associative destructuring
;; Destructuring a vector with a map, index wise
;; This can be done with the vector as they have index semantics 
(let [{fourth 4 zeroth 0} [1 2 3 4 5]]
  [zeroth fourth])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      Chapter 4 On Scalers      ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See, clojure allows variables to be promoted when it knows that the value can't be stored
;; in the current type.
;; But BEWARE that this is clojure, which was designed to be host agnostic. The java interop
;; still behvaes java.
;; The default starts at double and Long for the integer values in clojure, which is enough
;; to accomodate most of the values.

;; Integer starts at Long -> promotes to BigInt automatically 
(def x 34)
(class x)
(def y 23423423423423423235)
(class y)

;; Floating point starts at Double -> Doesn't get promoted to BigDecimal automatically,
;; need M suffix. If the value doesn't fit in the double, the truncation occurs.

(def a 34.9)
(class a)
(def b 34.3983372398723948237492384723947)
;; see the truncation of y and notice how it is different from the Integer promotion shown above.
b
(class y)
;; We need to specify the suffix in order to treat this as a BigDecimal
(def c 34.3983372398723948237492384723947M)
(class c)
c

;; The promotion, doesn't happen during the calculation, even for integers
(+ Long/MAX_VALUE Long/MAX_VALUE) ;; causes an exception
;; The operator stores the data in the bigger sized type
;; The below code won't give an exception
(+ Long/MAX_VALUE (inc Long/MAX_VALUE))

;; even if we use the clojure constant values (not referring java), it gives exception
(def maxval 9223372036854775807)
(def maxval2 9223372036854775807)
(+ maxval maxval2)

;; but if the maxval had been a BigInt value, the operation would have passed
(def bigmaxval 9223372036854775808)
(class bigmaxval) ;;Now it's a BigInt
(+ bigmaxval maxval) ;; Note the suffix N here, this won't cause exception as we have one BigInt

;; A caveat with floating point numbers
;; If any double appears anywhere in the expression the result is double, even after using suffix M for BigDecimal
(+ 0.1M 0.1M 0.1M 0.1 0.1M 0.1M 0.1M 0.1M 0.1M 0.1M)

;; Is it the case with integers? No, we have seen above, lets try with the suffix
(def maxvalsuffix 9223372036854775807N)
(class maxvalsuffix)
(+ 2323 maxvalsuffix)
;; It can be seen that the Integers take the approach where, the bigger data type is the result type.

;; Rational in clojure
;; Useful when accuracy is more important than the speed itself.
;; Although we are much safer with BigDecimal but still it can trip us in some domains.
;; NOTE: BigDecimal are not truncated, they throw exception if overwritten
;; Floating points are neither associative nor distributive

(def a 1.0e50)
(def b -1.0e50)
(def c 17.0e00)

;; Tested it in C# as well and it does show the same behaviour!!!
(+ (+ a b) c) ;; eval to 17.0 
(+ a (+ b c)) ;; eval to 0.0 !!!

;; So what's the way out
;; Clojure offer Rational numbers with helper functions like rational?, ration?, rationalizez
;; Although all bets are off with numbers like pi, which are irrational

(def a (rationalize 1.0e50))
(def b (rationalize -1.0e50))
(def c (rationalize 17.0e00))
(+ (+ a b) c)
(+ a (+ b c))

;;; 4.3 Keywords
;; Evaluates to self
:keys
{:keyword 8}

:small
:medium
:large

(map inc [1 3 4 5])

(seq {:name "bayant" :lname "kang"})
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;     Chapter 5         ;;;;;
;;;;;    Collections        ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collections are divided into three categories
;; 1. Sequentials - '() [] and everything that implements java.util.List
;; 2. Maps - array-map, hash-map, sorted-map and persistentstructmap 
;;           By default we get array-map, where insertion order is maintained and is upgraded to
;;           hash-map after the threshold.
;;           sorted-map and persistentstructmap are created explicitly.
;; 3. Sets

;; Equality, if two sequential have same contents, they are considered equal
(= '(1 2 4) [1 2 4])
;; Even if two collections have same contents but belong to different classes are considered different
(= #{1 2 3} [1 2 3])

(class {:a 3})
(class (hash-map :a 3))
(= (array-map :a 1 :b 2) (hash-map :a 1 :b 2))
(= (sorted-map :a 1 :b 2) (hash-map :a 1 :b 2))

;; There is also a PersistentStructMap but is not used very often. The idea was to provide fast access to
;; most used keys and is depricated in favour of records.

;; In order to be considered as a sequence, one need to implement first and rest function, that's it.
;; This forms the basis of powerful library functions: filter, map, doseq, take, partition...and so on.
;; Interesting that we only need these two functions to support such range of operations that seem
;; not that related.
;; For walking the element every clojure collection provide a function that gives a sequence over it.
;; This is given with the seq function.
;; Some provide more of such flavored functions, like vector has rseq and maps has keys and vals

;; On the simlilar lines, we have different type of sequences

(class (seq (hash-map :a 3)))
(class (seq (array-map :a 3)))
(class (keys {:a 3 :b 4}))

;; but at the end these are all sequences and are treated the same way, if we think abstractly.


;; Big O notation
;; lookup is not constant time in clojure??? in vector it is said log(n) time. What the heck???

(vec (range 10))

(let [my-vector [:a :b :c]]
  (into my-vector (range 10)))

(vector 1 2  4 5 5  5 5)
;; vec and vector are two different things

;; we can have some type based vectors as well
;; NOTE: All the caveats of overflow and underflows apply here as well
(into  (vector-of :int) [1 2 4 5.54343]) 

;; this one throws exception if we try to add a BigInt to it
(into  (vector-of :int) [1 2 4 2392309723502394832048340423])

;; Vector addition of values at the end is done by treating the vector as a stack
;; Insertion and indexing in clojure persistent vector is log32n which is almost constant time.

(def a-to-j (vec (map char (range 65 75))))

;; indexing a vector, 3 ways

(nth a-to-j 4) ;; if index is out of range throws exception by default but a default value can be given 
(get a-to-j 4) ;; returns nil if vector is nil or index is out of range  and support a default value as well.
(a-to-j 4)     ;; throws exception if the vector is nil or index is out of range.

;; As they are indexed vectors can easily be traversed backward and forward 
(seq [1 2 4 5])
(rseq [1 2 4 5])

;; value can be updated at any index in essentially constant time
;; NOTE: assoc works for the vector length or 1 item beyond the end, which is interesting
;; below assoc does essential an append operation for the vector
(assoc [1 2 3 4] 4 100)
;; Its more common that the vectors are grown using the conj function

;; NOTE: Nested vectors are far from the most efficient way of storage and processing data
;; for the sake of example consider the nested vectors

(get-in [[1 2 3 4] [5 6 7 8] [9 10 11 12]] [1 2])
(assoc-in [[1 2 3 4] [5 6 7 8] [9 10 11 12]] [1 2] 100)
;;update-in is like assoc-in but instead takes a function
(update-in [[1 2 3 4] [5 6 7 8] [9 10 11 12]] [1 2] (fn [a b c d] (+ a b c d)) 8 9 10)

(apply (fn [a b] (+ a b)) [1 3])


;; Vectors also have, peek along with last, conj along with assoc and pop along with disassoc.
;; If we are using a vector in a stack context, we better be using the, push, pop and conj
;; terminology. Peek is also faster than the last in that last need to traverse the whole
;; vector to get to the last element.
;; Any object implement clojure.lang.IPersistentStack can use the functions peek, pop and conj.
;; conj also work with oterh Persistent type objects although they don't implement stack.

(peek [2 34 5])

;; See conj and cons work differently in vector and list, conj and cons are the same in lists.
(conj '(1 3 4) 8)
(cons 8 '(1 3 4))

(conj [1 3 4] 8)
(cons 3 [2 4 5]) ;; This doesn't look efficient to me.

(defn test [arg] "test")
;; How to unload a function in clojure repl??
;; To view source of a clojure core function, run
(source map)

;; Subvectors
;; Sub vector hangs onto the original vector, they don't create anything new, but do some offset
;; math to perform the operations 

(subvec (subvec a-to-j 3 6) 1 2)

;; Vectors are map entries
(first {:width 10 :height 20}) ;; NOTE: Map doesn't guarantee on the order of the elements
(class (first {:width 10 :height 20})) ;; The type here is MapEntry which essentially is a vector,
                                       ;; it has specialized functions key and val.

(key (first {:width 10 :height 20}))
(vector? (first {:width 10 :height 20}))
;;MapEntry being a vector is that we can perform all the vector functions on it.

(conj (first {:width 10 :height 20}) :breadth 10)

;; Destructuring map entries also become easy when we iterate over it
(doseq [[dimension amount] {:width 10, :height 20, :depth 15}]
  (println (str (name dimension) ":") amount "inches"))

;; What Vectors aren't
;;  aren't sparse - index based access, compare with map which doesn't follow sequential style access
;;  aren't queues - using vector to implement queue is a bad idea, the problem is that the rest and next function
;;                  for the pop implementation, returns a sequence and further conj to it won't be a queue conj.
;;                  It will add the value at front. Using subvec also isn't efficient since it keeps the original
;;                  vector, it's values are never going to be garbage collected.
;;  aren't set    - The following code returns false. As contains? work on checking whether the vector has the index
;;                  not the value. For vector keys are the index and values are the data. Contains? is for checking the
;;                  keys not the values.
                 
(contains? [1 3] 3)

;; LISTS
;; The correct way to add element (to the left) of a list is conj
(conj 3 '(1 2 4))
(conj '(1 2 4) 3)

;; A list built using conj is homogeneous, that is the result is guaranteed to be a list. This is also more efficient.
;; A sequence built with cons is always going to be a sequence
(cons 3 '(23 4 5))
(cons 3 [1 2 4])
;; So cons can be used to add to the front of any sequence.

;; All the sequence are printed with () but that doesn't mean that they are of the same type. Example many of the seq
;; don't know the size of their own and it would cost O(n) to calculate that.

;; We can use counted? to know whether the seq provide the definition of a count function for the sequence

(counted? [1 3 8])
(counted? '(1 3 4))
(counted? (range 10))
(counted? {:a 8 :b 8})
(counted? (hash-map :a 8 :b 9))
;;lazy sequences are not counted
(counted? (map inc [1  3 4 5]))

(counted? "dsfjsfldsj")

(count "dfjsdlkf") ;; Although string does have a count function but that is not a constant time function.

(count [1 3 8])
;; Don't use lists when you want to find the value by index and like vectors don't use if you want to search multiple
;; times about an element's presence in the list

;; Lists can't be used as queues as well as we can add elements to the left of the list but there is no removal from
;; the right of the list.

;; Contains? doesn't work with lists
(contains? '(1 1 2 4) 2)

;; BEWARE::::: 
(contains? '(1 2 4) 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistent queues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conj -> add to the rear
;; peek -> see the front
;; pop  -> remove from the front

;; NOTE: The Persistent queues are immutable queues and are not a workflow mechanism. Java has classes extending from
;; java.util.concurrent.BlockingQueue which are useful in clojure for the workflow mechanism.
;; Clojure persistent queues would give us new queue after the update making it unworkable with consumer producer (workflow
;; mechanism).

;; Poping from empty queue results in another empty queue, doesn't raise exception
;; Peeking empty queue results in nil
;; This is done so that queue can also be treated as sequence.

;; NOTE: functions like first, rest and next also work on queue but they return sequences not the queue.
;; So, if using a queue use the conj, peek and pop functions

;; Ex:
;; Following function is used to customize the behavior of printing, Its a multimethdo
;; There's no special symbol for queues in clojure, like we have (), [] etc
(defmethod print-method clojure.lang.PersistentQueue
[q, w]
(print-method '<- w)
(print-method (seq q) w)
(print-method '-< w))

(def schedule
(conj clojure.lang.PersistentQueue/EMPTY
:wake-up :shower :brush-teeth))

;; Internally queue is implemented as two DS, one queue and one vector, elements are added in vector and removed from list
;; If the list is exhausted, the vector is moved to the list and a new vector is created.
;; Again the functions like cons, rest etc would work here but they give us sequence and further conj on the sequence
;; doesn't guarantee the performance of the queue.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure Sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure sets are the function of their elements, returns the element if present or nil.

(#{1 3 4 5} 3)

(into #{[]} '[()]) ;; although types are different but these are equal as they lye in the same equality partition.

(into #{[1 3]} '[(1 3)])

(into #{[] #{} {}} [()]) ;; even if items are equal, the equality partition is also checked for the equality.

;; Sets are implemented as maps, where each element is the key and value as well.
(contains? #{1 2 3 4} 4) ;; This returns true because the key 4 is present in the keys.
(contains? #{"a" "b"} "b") ;; This returns true because the key "b" is present in the keys.
(contains? ["a" "b"] "b")

(clojure.set/difference #{1 2 4 5} #{1 2 4 7})

;; Why supplying seq with unique elements work but, if we repeat the element in the seq it doesn't seem to work.
(clojure.set/difference #{1 2 4 5} '(1 1 12 9 4))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure Maps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hash maps the default {}
(class {:a 3 :b 4})
{:a 3 :b 4 :c 1}

(class (hash-map :a 3 :b 4))
(hash-map :a 3 :b 4 :c 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 6 Functional Programming ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(concat [1 2 4] [23 5])


;; Example to show persistence structure
(defn xconj [t v]
  (cond 
    (nil? t) {:L nil :val v :R nil}
    (< v (:val t)) {:L (xconj (:L t) v) :val (:val t) :R (:R t)}
    :else          {:L (:L t) :val (:val t) :R (xconj (:R t) v)}))

(def tree1 (xconj nil 8))
(def tree1 (xconj tree1 6))
(def tree1 (xconj tree1 4))

;; preorder traversal
(defn xseq [t]
  (when t
    (concat (xseq (:L t)) [(:val t)] (xseq (:R t)))))

(xseq tree1)
;;
;;   8
;;  6
;; 4
;;

(def tree2 (xconj tree1 10))
(xseq tree2)

;; Conceptual picture
;;   8
;;  6 10
;; 4
;;

(= (:L tree1) (:L tree2))
;; How clojure does it
;;   8       8
;;  6 <----|  10
;; 4
;;
;; It DID NOT modify the left pointer of the node 8, but instead it created a new node (8 here) in the new tree and added the number
;; to the right of it (10) and afterwards it DID NOT copy the entire tree to the new tree but instead it made the new node's (8) left
;; to point to the the left of the older node (8), which is 6
;; Some observations:
;; 1. Every "change" creates a new root node at least and the other nodes as needed (all the nodes it traverse to the point where we find 
;;    the proper place of insertion).
;; 2. This is thread safe.

;; Fails the production quality because:
;; 1. It's a Binary Tree. #################### WHY ########################
;; 2. Stack overflow if the tree goes deep.
;; 3. It produces (via xseq) a non-lazy seq that will contain the entire copy of the seq.
;; 4. Can create unbalanced tree which have the worst case complexities.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;       Laziness           ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def res [1 [2 [3 [4 []]]]])

(defn steps [arr iter]
  (if (= (count arr) 1)
    (into [(first arr)] iter)
    (steps (rest arr) (into iter [first arr]))))

(defn recstep [[x & xs]]
  (if x
    [x (recstep xs)]
    []))

(recstep [1 2 3 4])
(recstep [1 2 3])
(recstep [1])
[1 [2 [3 [4 []]]]]

(recstep (range 2000))

(steps [1 2 3 4])

;; recstep with huge size

;; making the recstep lazy
(defn lz-recstep [x]
  (lazy-seq
   (if x
     [x (recstep (rest x))]
     [])))

;; This small number of 2000 actually returns nil as described in the book
;; but the bigger numbers are giving stackoverflow errors.
(dorun (lz-recstep (range 2000)))

;; recstep with recur
(defn recstep-recur [x]
  (lazy-seq
   (letfn [(rec-helper [s acc deep]
             (if (seq s)
               (recur (rest s)
                      (update-in acc (repeat deep 1) (fn [_] [(first s) []]))
                      ;;(conj acc (first s) acc)
                      (inc deep))
               acc))]
     (rec-helper (rest x) [(first x) []] 1))))

(dorun (recstep-recur (range 20000)))
(+ 2 4)

(cons 0 (apply vector (repeat 7 1)))
(cons )
(into [0] (repeat 8 7) [9])
(into )
(conj (repeat 7 1) 0)

(conj (into [0] (repeat 3 1)) 0)

(apply vector 0 (repeat 3 1) 8)

(apply vector 8 9 9 '(23 8) [88])
(vec 0 8)
(vec [0] [8])
(into [0] [4])

(vector 0 (repeat ))
(apply )

(vec 0 [1 4])
(lz-recstep-recur [1 2])

(vec  (update-in [1 2 [4 5]] [2 1] #(conj [] (inc %))))

(as-> [] $
  (into $ [(first s) []])
  (update-in acc [deep] $))

(update-in [1 2 [4 5]] [2 2] inc)
(assoc-in [1 2 [4 5]] [2 2] 9)

(def very-lazy (
                -> (iterate #(do (print \.) (inc %)) 1)
                rest rest rest))

(def some-lazy (
                -> (iterate #(do (print \.) (inc %)) 1)
                next next next))
;; WHY #################### 

;; Clojure compiler can't move expressions, i.e, it can't change the order of the expressions
;; because the purity of the functions is not guranteed. 

;; 

(let [r (range 1e9)]
  (first r)
  (last r))
;;=> 999999999 
(let [r (range 1e9)]
  (last r)
  (first r))

;; java.lang.OutOfMemoryError: GC overhead limit exceeded

;; Why the second one is showing out of memory error.
;; Ans: (last r) actually requires realizing the seq. In the first example, the head of the sequence
;; is not required 

;; Why lazy sequence?
;; Lazy sequences often foster more declaratvie solutions
;; 
;; Example: Triangle number of a number is a number of how many bowling pins need to be arranged to form a triangle.
;;          Triangle number of 4 is 10. (1 + 2 + 3 + 4) = 10

(defn triangle [n]
  (/ (* n (+ n 1)) 2))

(def all-triangle (map triangle (range)))

;; get-first
(first all-triangle)

;; get-first 10 even
(take 10 (filter #(even? %) all-triangle))

;; what Gauss found
(nth all-triangle 99) ;; Will it calculate the first 98 as well?

(take 2 (drop-while #(< % 10000) all-triangle))

;; map, reduce and filter are all lazy

;; delay and force are the macros where we can explicity state about the laziness of the computation

(defn defer-expensive [cheap expensive]
  (if-let [good-enough (force cheap)]
    good-enough
    (force expensive)))

;; To do the cheap thing
(defer-expensive (delay :cheap)
  (delay (do (Thread/sleep 3000)) :expensive))

;; If we must do the expensive thing
(defer-expensive (delay false)
  (delay (do (Thread/sleep 3000)) :expensive))

;; we could also do this with delayed fn 
;; we can also use this delay and force for the triangle number problem
;; instead of using map 

;; this realize only the head and the tail is always lazy
;; when we query the head, only then we will realize the element
(defn inf-triangles [n]
  {:head (triangle n)
   :tail (delay (inf-triangles (inc n)))})

(defn head [l] (:head l))
(defn tail [l] (force (:tail l)))

;; As an example, this is how to get the triangle number of 4
(head (inf-triangles 4))

;; Thinking of chaining head and tail
;; starts with 1 and every time we do a tail, it goes one step ahead.
;; so here we calculate the triangle number of 2
(head (tail (inf-triangles 1)))
;; triangle number of 3
(head (tail (tail (inf-triangles 1))))
;; triangle number of 4
(head (tail (tail (tail (inf-triangles 1)))))

;;
;; Instead of doing it this way, chaining tails with head and using this elementry
;; building blocks, we can create a high level construct that is easier to work with. 
;; create a triangle number list for first n elements
;; NOTE: The generalization only needs is that we have head and a tail function, working
;;       with the list.
;;
(defn taker [n]
  (loop [iter 1 res []]
    (if (> iter n)
      res
      ;; QUESTION: Won't (head (inf-traingles iter)) calculate triangle number from 
      ;;           start with each recursive call? Or is it cached somehow?
      (recur (inc iter) (conj res (head (inf-triangles iter)))))))

(defn taker-fast [n]
  (loop [iter 1 src (inf-triangles 1) res []]
    (if (> iter n)
      res
      ;; SEE: How we improved from calculating the head from 1 each recursive call
      ;;      to calculating it once in each recursive call
      (recur (inc iter) (tail src) (conj res (head src))))))

(defn nthr [l n]
  (if (zero? n)
    (head l)
    (recur (tail l) (dec n))))

(nthr (inf-triangles 10) 7)

;; Clojure inbuilt lazy-seq is good enough for lazy sequencing, we don't need to reinvent the
;; wheel with such elementry approach. Although clojure has given us these fundamental building
;; blocks (delay and force) but lazy-seq is sufficient and more succint.
;; The type of lazy-structure we used (with head and tail) is called head strict, where head is
;; computed and tail is lazy, wherease in lazy-seq both head and tail are lazy.


;; Quicksort implementation
;;
(defn rand-ints [n]
  (take n (repeatedly #(rand-int n))))

(defn sort-parts [work]
  (lazy-seq
   (loop [[part & parts] work]
     (if-let [[pivot & xs] (seq part)]
       (let [smaller? #(< % pivot)]
         (recur (list*
                 (filter smaller? xs)
                 pivot
                 (remove smaller? xs)
                 parts)))
       (when-let [[x & parts] parts]
         (cons x (sort-parts parts)))))))

(defn qsort [elems]
  (sort-parts (list elems)))

(qsort (rand-ints 30))

(list (filter even? [1 2 4]) 8 (remove even? [1 2 4]))

(let [[part & parts] (list [1 2 3 4])]
     part)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;                         ;;;;;;;
;;;;;;         Capter 7        ;;;;;;;
;;;    Functional Programming    ;;;;;
;;;;;;                         ;;;;;;;
;;;;;;                         ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Composing functions

;; We can use comp to create a function dynamically with other function composition
(def fifth (comp first rest rest rest rest))
(fifth [1 2 43 45 20 23])

;; Taking it one step ahead, to return the function from a function
(defn fnth [n]
  (apply comp
         (cons first (take (dec n) (repeat rest)))))

;; fnth build functions on the fly based on the argument it recieves
;; Creating new functions this way is a powerful technique and takes
;; some practice to think in this compositional way


(def sixth (fnth 6))
(sixth [1 4 8 3 23 9 2 10])

((comp inc #(* 2 %)) 8)

;; Create keys from a list of symbols
(map (comp
      keyword
      #(.toLowerCase %)
      name)
     '(a B C))

;; Also note from the above that 'a is a symbol
(class 'a)
;; To not evaluate the items in vector we can use ' like in list, this treat the item of vector as it is
;; and won't think that a is a expression in itself but treat it as a symbol
(map name '[a b c])

;; Partial functions 
((partial + 5) 100 200)
;; the above is eqv to
(#(apply + 5 %&) 100 200)

((complement even?) 2)
;; eqv
((comp not even?) 2)

;; The below two are producing the same thing
(:a {:a 4 :b 8})
({:a 4 :b 8} :a)

(sort-by [:plays ])

;; Function returning function
(def plays [{:band "Burial", :plays 979, :loved 9}
            {:band "Eno", :plays 2333, :loved 15}
            {:band "Bill Evans", :plays 979, :loved 9}
            {:band "Magma", :plays 2665, :loved 31}])

(defn columns [col-names]
  (fn [row] 
    (vec (map row col-names))))

;; what columns does
((columns [:plays :loved :band]) (first plays))

;; sorting through multiple keys
(sort-by (columns [:plays :loved :band]) plays)

;; could also have been done by anf 
(sort-by #(vec (map % [:plays :loved :band])) plays)

;; See how the sort-by just takes a function that create a vector of the map
;; and use this as a sorting function

;; notion of pure functions
;; A function is pure when:
;;  1. It doesn't depend on anything other than args
;;  2. It doesn't have any observable side-effects. 

;; optional parameters named

(defn slope
  [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]}}]
  (float (/ (- (p2 1) (p1 1))
            (- (p2 0) (p1 0)))))

;; see how we can call it in any order
(slope :p1 [8 10] :p2 [10 4])
(slope :p2 [8 10] :p1 [10 4])
;; and optionally
(slope :p2 [8 10])

;; In the above call although we are receiving it as a list
;; but we know that the items in it are map key and vals
;; and can be destructured in a map style
;; a simple example:
(let [{:keys [p1 p2]} '(:p1 8 :p2 10)] [p1 p2])


;; Pre and Post conditions
(defn slopepre [p1 p2] 
  {:pre [(not= p1 p2) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  (/ (- (p2 1) (p1 1))
     (- (p2 0) (p1 0))))


;; assertion error for p1 = p2
(slopepre [10 10] [10 10])
(slopepre [8.8 4] [4 8])

;; clojure also has asserts

;; Decoupling assertions from the function logic
;; In the previous example its pretty much okay to have the pre and post conditions
;; as part of the function logic because it's inherent to the slope finding formulae.
;; In some scenarios we want to handle the pre and post conditions not in the same place
;; as the function logic because those pre-conditions might depend on the context of the 
;; caller or we can say those are not part of the core logic of the function. 

;; In that case we have the freedom to use some other fuction as verfier
(defn put-things [m]
  (into m {:meat "beef" :veggie "broccoli"}))

;; adding constraints to put-things might not be a good idea and it should be left to the
;; caller

(defn vegan-contraints [f m]
  {:pre [:veggie m]
   :post [(:veggie %) (nil? (:meat %))]}
  (f m))

(vegan-contraints put-things {:veggie "carrot"})

;; closures
;; when a function access the locals of its outer scope, that function is said to
;; be closed over that local.
(def bearings [{:x 0, :y 1} ; north
               {:x 1, :y 0} ; east
               {:x 0, :y -1} ; south
               {:x -1, :y 0}]) ; west

(defn bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (bot (+ x (:x (bearings bearing-num)))
                        (+ y (:y (bearings bearing-num)))
                        bearing-num))
   :turn-right (fn [] (bot x y (mod (+ 1 bearing-num) 4)))
   :turn-left (fn [] (bot x y (mod (- 1 bearing-num) 4)))})


;; forward is closed over, bearing-num, NOTE: we are not abusing the purity of the closure
;; since we are just using a def and it can never change.
(:coords ((:forward (bot 5 5 0))))

;; If we attach a mutable state with the closure (the thing it closes over) we can easily 
;; mutate the closed state variable.
;; Will do this in the atom and mutation chapters

;; simple polymorphism can be acheived through closures, ex: creating a bot that does something else
(defn mirror-bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (mirror-bot (- x (:x (bearings bearing-num)))
                               (- y (:y (bearings bearing-num)))
                               bearing-num))
   :turn-right (fn [] (mirror-bot x y (mod (- 1 bearing-num) 4)))
   :turn-left (fn [] (mirror-bot x y (mod (+ 1 bearing-num) 4)))})

;; Now mirror-bot and bot are polymorphic substitutes.  
   ;; But this is true even if we don't have closures as well.

;; Recursion
;; Mundane recursion: Calling function by name, and not through recur or mutual recursion.
;; Classical power calculation
(defn pow [base exp]
  (if (zero? exp)
    1
    (* base (pow base (dec exp)))))

;; Here we are relying on the result of the previous iteration of the recursion, but we can do better
(defn powImp [base exp]
  (letfn [(powit [base exp acc]
            (if (zero? exp)
              acc
              (recur base (dec exp) (* base acc))))]
    (powit base exp 1)))

(powImp 8 8)

;; with lazy sequences we can use the mundane recursion for the good effect, for the functions
;; returning sequences.
;; QUESTION: Author keep saying that it avoids the stackoverflow error but I am not convinced
;; refer lz-recstep above and see the lazy-seq version is alsow giving the stackoverflow error

;; unit convertor
;; wise use of reduction with recursion, this si sick
(def simple-metric {:meter 1,
                    :km 1000,
                    :cm 1/100,
                    :mm [1/10 :cm]})

;; 3km 10m 80cm 10mm = ?m
(-> (* 3 (:km simple-metric))
    (+ (* 10 (:meter simple-metric)))
    (+ (* 80 (:cm simple-metric)))
    (+ (* (:cm simple-metric)
          (* 10 (first (:mm simple-metric)))))
    float)

;; a function which automates the above manual calculation:

;; synergy b/w recursive data and recursive function
;; this is amazing

(defn convert [context data]
  (reduce (fn [result [mag unit]]
            (+ result
               (let [val (get context unit)]
                 (if (vector? val)
                   (* mag (convert context val))
                   (* mag val)))))
          0
          (partition 2 data)))

(convert simple-metric [108 :meter 38 :km 32.4 :mm])
;; Beauty of the above function is that it can be used to convert anything
(convert {:bit 1 :byte 8 :nibble [1/2 :byte]} [32 :nibble])
;; see it works if the data is not recursive as well
;; NOTE: The return types are different, the above returns a big int and the 
;;       below on a long
(convert {:bit 1 :byte 8 :nibble 4} [32 :nibble])

;; Tail call optimization
(defn gcd [x y]
  (cond
    (> x y) (recur (- x y) y)
    (< x y) (recur x (- y x))
    :else x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;      IDIOMS      ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; 1. Finding items in a sequence using some function and set


(some #{1} [1 3 4 5])
(some #{2} [1 3 4 5])
(some #{1 :b} [:a 1 :b 2])

;; 2. Using seq to check if a sequence is empty or not
(seq [1 2 4])
(seq []) ;; this returns nil and which is falsy in clojure
