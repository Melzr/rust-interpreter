(ns rust-interpreter.core-test
  (:require [clojure.test :refer :all]
            [rust-interpreter.core :refer :all]))

; user=> (agregar-ptocoma (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'if 'x '< '0 (symbol "{") 'x '= '- 'x (symbol ";") (symbol "}") 'renglon '= 'x (symbol ";") 'if 'z '< '0 (symbol "{") 'z '= '- 'z (symbol ";") (symbol "}") (symbol "}") 'fn 'foo (symbol "(") (symbol ")") (symbol "{") 'if 'y '> '0 (symbol "{") 'y '= '- 'y (symbol ";") (symbol "}") 'else (symbol "{") 'x '= '- 'y (symbol ";") (symbol "}") (symbol "}")))
; (fn main ( ) { if x < 0 { x = - x ; } ; renglon = x ; if z < 0 { z = - z ; } } fn foo ( ) { if y > 0 { y = - y ; } else { x = - y ; } })
(deftest agregar-ptocoma-programa
  (testing "agregar-ptocoma programa"
    (is (= (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'if 'x '< '0 (symbol "{") 'x '= '- 'x 
                  (symbol ";") (symbol "}") (symbol ";") 'renglon '= 'x (symbol ";") 'if 'z '< '0 (symbol "{")
                  'z '= '- 'z (symbol ";") (symbol "}") (symbol "}") 'fn 'foo (symbol "(") (symbol ")")
                  (symbol "{") 'if 'y '> '0 (symbol "{") 'y '= '- 'y (symbol ";") (symbol "}") 'else (symbol "{")
                  'x '= '- 'y (symbol ";") (symbol "}") (symbol "}"))
            (agregar-ptocoma (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'if 'x '< '0 (symbol "{") 'x
                                    '= '- 'x (symbol ";") (symbol "}") 'renglon '= 'x (symbol ";") 'if 'z '< '0
                                    (symbol "{") 'z '= '- 'z (symbol ";") (symbol "}") (symbol "}") 'fn 'foo
                                    (symbol "(") (symbol ")") (symbol "{") 'if 'y '> '0 (symbol "{") 'y '= '- 'y
                                    (symbol ";") (symbol "}") 'else (symbol "{") 'x '= '- 'y (symbol ";")
                                    (symbol "}") (symbol "}")))
        ))
  )
)

; user=> (palabra-reservada? 'while)
; true
(deftest palabra-reservada-while
  (testing "palabra-reservada? while"
    (is (= true (palabra-reservada? 'while)))
  )
)

; user=> (palabra-reservada? 'until)
; false
(deftest palabra-reservada-until
  (testing "palabra-reservada? until"
    (is (= false (palabra-reservada? 'until)))
  )
)

; user=> (palabra-reservada? 13)
; false
(deftest palabra-reservada-13
  (testing "palabra-reservada? 13"
    (is (= false (palabra-reservada? 13)))
  )
)

; user=> (identificador? 'boolean)
; true
(deftest identificador-boolean
  (testing "identificador? boolean"
    (is (= true (identificador? 'boolean)))
  )
)

; user=> (identificador? 'bool)
; false
(deftest identificador-bool
  (testing "identificador? bool"
    (is (= false (identificador? 'bool)))
  )
)

; user=> (identificador? 'e120)
; true
(deftest identificador-e120
  (testing "identificador? e120"
    (is (= true (identificador? 'e120)))
  )
)

; user=> (identificador? '12e0)
; false
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

; user=> (dump '[[POPREF 2] [PUSHFI 2] MUL [PUSHFI 1] ADD NEG])
; 0 [POPREF 2]
; 1 [PUSHFI 2]
; 2 MUL
; 3 [PUSHFI 1]
; 4 ADD
; 5 NEG
; nil
(deftest dump-instrucciones-print
  (testing "dump-instrucciones print"
    (is (=
      "0 [POPREF 2]\n1 [PUSHFI 2]\n2 MUL\n3 [PUSHFI 1]\n4 ADD\n5 NEG\n"
      (with-out-str (dump '[[POPREF 2] [PUSHFI 2] MUL [PUSHFI 1] ADD NEG]))
    ))
  )
)

; user=> (dump '[HLT])
; 0 HLT
; nil
(deftest dump-instruccion-print
  (testing "dump-instruccion print"
    (is (=
      "0 HLT\n"
      (with-out-str (dump '[HLT]))
    ))
  )
)

; user=> (dump nil)
; 0 nil
; nil
(deftest dump-instruccion-nil
  (testing "dump-instruccion nil"
    (is (=
      "0 nil\n"
      (with-out-str (dump nil))
    ))
  )
)


; user=> (ya-declarado-localmente? 'Write [[0] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])
; true
(deftest declarado-localmente-0
  (testing "declarado-localmente 0"
    (is (=
      true
      (ya-declarado-localmente? 'Write [[0] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])
    ))
  )
)

; user=> (ya-declarado-localmente? 'Read [[0] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])
; false
(deftest no-declarado-localmente-0
  (testing "no declarado-localmente 0"
    (is (=
      false
      (ya-declarado-localmente? 'Read [[0] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])
    ))
  )
)

; user=> (ya-declarado-localmente? 'Write [[0 1] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])
; true
(deftest declarado-localmente-1
  (testing "declarado-localmente 1"
    (is (=
      true
      (ya-declarado-localmente? 'Write [[0 1] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])
    ))
  )
)

; user=> (ya-declarado-localmente? 'Write [[0 2] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])
; false
(deftest no-declarado-localmente-2
  (testing "no declarado-localmente 2"
    (is (=
      false
      (ya-declarado-localmente? 'Write [[0 2] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])
    ))
  )
)

(deftest declarado-localmente-vacio
  (testing "declarado-localmente vacio"
    (is (=
      false
      (ya-declarado-localmente? 'Write [[0] []])
    ))
  )
)

(deftest no-declarado-localmente-4
  (testing "no declarado-localmente 4"
    (is (=
      false
      (ya-declarado-localmente? 'Write [[0 4] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])
    ))
  )
)
