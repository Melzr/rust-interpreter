(ns rust-interpreter.core-test
  (:require [clojure.test :refer :all]
            [rust-interpreter.core :refer :all]))

(deftest palabra-reservada-while
  (testing "palabra-reservada? while"
    (is (= true (palabra-reservada? 'while)))
  )
)

(deftest palabra-reservada-until
  (testing "palabra-reservada? until"
    (is (= false (palabra-reservada? 'until)))
  )
)

(deftest palabra-reservada-13
  (testing "palabra-reservada? 13"
    (is (= false (palabra-reservada? 13)))
  )
)

(deftest identificador-boolean
  (testing "identificador? boolean"
    (is (= true (identificador? 'boolean)))
  )
)

(deftest identificador-bool
  (testing "identificador? bool"
    (is (= false (identificador? 'bool)))
  )
)

(deftest identificador-e120
  (testing "identificador? e120"
    (is (= true (identificador? 'e120)))
  )
)

(deftest identificador-12e0
  (testing "identificador? 12e0"
    (is (= false (identificador? '12e0)))
  )
)

(deftest identificador-a-b
  (testing "identificador? a-b"
    (is (= false (identificador? 'a-b)))
  )
)

(deftest identificador-a-b
  (testing "identificador? a_b"
    (is (= true (identificador? 'a_b)))
  )
)

(deftest dump-instrucciones-nil
  (testing "dump-instrucciones nil"
    (is (= nil (dump '[[POPREF 2] [PUSHFI 2] MUL [PUSHFI 1] ADD NEG])))
  )
)

(deftest dump-instrucciones-print
  (testing "dump-instrucciones print"
    (is (=
      "0 [POPREF 2]\n1 [PUSHFI 2]\n2 MUL\n3 [PUSHFI 1]\n4 ADD\n5 NEG\n"
      (with-out-str (dump '[[POPREF 2] [PUSHFI 2] MUL [PUSHFI 1] ADD NEG]))
    ))
  )
)

(deftest dump-instruccion-print
  (testing "dump-instruccion print"
    (is (=
      "0 HLT\n"
      (with-out-str (dump '[HLT]))
    ))
  )
)

(deftest dump-instruccion-nil
  (testing "dump-instruccion nil"
    (is (=
      "0 nil\n"
      (with-out-str (dump nil))
    ))
  )
)

