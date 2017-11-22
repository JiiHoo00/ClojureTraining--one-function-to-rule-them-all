(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (let [str-with-space (fn [text seq]
                         (str text " " seq))]
    (if (empty? a-seq)
      ""
      (reduce str-with-space a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [res a] (if (empty? res) [a]
                          (conj res x a))) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc a]
             (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [res a] (cons a res)) [] a-seq)))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [[mini maxi] a] [(min mini a) (max maxi a)])
            [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (if (< n (first sorted-seq))
      (cons n sorted-seq)
      (cons (first sorted-seq) (insert (rest sorted-seq) n)))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn toggle [a-set e]
  (if (contains? a-set e)
    (disj a-set e)
    (conj a-set e)))


(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (cons x (cons y more)))))

(defn pred-and
  ([] (fn [a] true))
  ([x] x)
  ([x y] (fn [a] (and (x a) (y a)) ))
  ([x y & more] (reduce pred-and (pred-and x y) more))
)


(defn my-map [f & more]
  "nope")
