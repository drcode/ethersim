(ns ethersim.core
    (:require [clojure.string :refer [upper-case]]
              [clojure.repl :refer [doc]]
              [clojure.pprint :refer [print-table]]
              [clll-examples.core :refer [key-value-publisher]]))

(def kvp key-value-publisher)

(defn out [& args]
      (print (apply str args))
      (flush))

(defn contract-tree [path]
      (out "Enter contract name (or enter to exit)\n")
      (out (apply str (interpose "/" (reverse path))) "/")
      (let [k (read-line)]
           (when (not= k "")
                 (let [contract (symbol k)]
                      (cons contract (contract-tree (cons contract path))))))) 

(defn pprint-storage [storage]
      (print-table (for [item storage]
                        (let [[k v] item]
                             {"Key"   k
                              "Value" v})))
      (flush))

(defn find-tree [item tree]
      (let [[cur & more] tree]
           (if (= cur item)
               tree
               (keep identity (map (partial item) more)))))

(defn interactive-transaction [tree contract-info blocknum caller]
      (let [contract                              (do (out "Contract name: ")
                                                      (symbol (read-line)))
            value                                 (do (out "Value to send (default 0): ")
                                                      (let [s (read-line)]
                                                           (if (= s "")
                                                               0
                                                               (Integer. s))))
            data                                  (do (out "Data (Hit enter when done)\n")
                                                      (vec (reverse (loop [acc []]
                                                                          (let [s (read-line)]
                                                                               (if (not= s "")
                                                                                   (recur (cons (try (Integer. s)
                                                                                                     (catch Exception e
                                                                                                            s))
                                                                                                acc))
                                                                                   acc))))))
            branch                                (find-tree contract tree)
            {:keys [fun storage]}                 (contract-info contract)
            {:keys [transactions storage result]} (fun storage 
                                                       {:caller caller
                                                        :value  value
                                                        :data   data}
                                                       {:number blocknum}
                                                       (into {}
                                                             (for [child (rest branch)]
                                                                  [(eval child) {:storage (:storage (contract-info child))}]))
                                                       0)]
           (print-table (for [transaction transactions]
                             (select-keys transaction [:to-address value])))
           (out "\n\n")
           (out "RESULT:")
           (prn result)
           (out "\n")
           (assoc-in contract-info [contract :storage] storage)))

(defn sim []
      (let [ct               (contract-tree nil)
            contract-names   (flatten ct)
            contract-authors (into {} 
                                   (for [contract contract-names]
                                        [contract (do (out "Author for " contract ": ") 
                                                      (read-line))]))
            contract-info    (into {}
                                   (for [contract contract-names]
                                        [contract (let [{:keys [storage result]} ((eval contract) {} {:caller (contract-authors contract)})]
                                                       {:storage storage 
                                                        :fun     (first result)})]))
            caller (do (out "starting caller: ")
                       (read-line))]
           (loop [contract-info contract-info
                  blocknum      0
                  caller        caller]
                 (doseq [contract contract-names] 
                        (out contract)
                        (pprint-storage (:storage (contract-info contract)))
                        (out "\n"))
                 (out "current block: " blocknum "\n\n")
                 (out "Execute [T]ransaction\nMine [B]lock\nChange [C]aller\n[Q]uit:\n")
                 (case (upper-case (read-line))
                       "T" (recur (interactive-transaction ct contract-info blocknum caller) blocknum caller)
                       "B" (recur contract-info (inc blocknum) caller)
                       "C" (recur contract-info blocknum (do (out "New caller: ")
                                                             (read-line)))
                       "Q" nil
                       (do (out "invalid command\n")
                           (recur contract-info blocknum caller))))))

