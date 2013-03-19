(ns task01.core
  (:require [pl.danieljanus.tagsoup :refer :all])
  (:gen-class))

(defn sniffer [data]
  (and (coll? data)
       (> (count data) 2)
       (let [el1 (get data 0)
             el2 (get data 1)
             el3 (get data 2)]
         (and (vector? data)
              (= el1 :h3)
              (and (map? el2)
                   (contains? el2 :class) 
                   (= (:class el2) "r"))
              (= (el3 0) :a)))))

(defn tag-wanderer [sniffer data]
  (if (sniffer data)
    ((get (get data 2) 1) :href)
    (if (coll? data)
      (map #(tag-wanderer sniffer %) data))))

(defn get-links []
  ;;   " 1) Find all elements containing {:class \"r\"}.

  ;; Example:
  ;; \[:h3 {:class \"r\"} [:a {:shape \"rect\", :class \"l\",
  ;;                          :href \"https://github.com/clojure/clojure\",
  ;;                          :onmousedown \"return rwt(this,'','','','4','AFQjCNFlSngH8Q4cB8TMqb710dD6ZkDSJg','','0CFYQFjAD','','',event)\"}
  ;;                      [:em {} \"clojure\"] \"/\" [:em {} \"clojure\"] \" · GitHub\"]]

  ;; 2) Extract href from the element :a.

  ;; The link from the example above is 'https://github.com/clojure/clojure'.

  ;; 3) Return vector of all 10 links.

  ;; Example: ['https://github.com/clojure/clojure', 'http://clojure.com/', . . .]
  ;; "
  (let [data (parse "clojure_google.html")]
    (into [] (filter #(not (nil? %)) 
                     (flatten (map #(tag-wanderer sniffer %) data))))))

(defn -main []
  (println (str "Found " (count (get-links)) " links!")))


