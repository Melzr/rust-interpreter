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

; user=> (cargar-const-en-tabla [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] 8 [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []])
; [; (fn main ( ) { println! ( "{}" , TRES ) }) [use std :: io ; const TRES : i64 = 3] 8 [[0] [[io [lib ()] 0]]] 0 [[CAL 0] HLT] []]
;                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^ ^^^^^^^^^^^^^^^^^^^^^^^ 
(deftest cargar-const-en-tabla-error
  (testing "cargar-const-en-tabla error"
    (is (=
      [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] 8 [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []]
      (cargar-const-en-tabla [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] 8 [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []])
    ))
  )
)

; user=> (cargar-const-en-tabla [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] :sin-errores [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []])
; [; (fn main ( ) { println! ( "{}" , TRES ) }) [use std :: io ; const TRES : i64 = 3] :sin-errores [[0] [[io [lib ()] 0] [TRES [const i64] 3]]] 0 [[CAL 0] HLT] []]
;                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
(deftest cargar-const-en-tabla-sin-errores
  (testing "cargar-const-en-tabla sin errores"
    (is (=
      [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] :sin-errores [[0] [['io ['lib '()] 0] ['TRES ['const 'i64] 3]]] 0 [['CAL 0] 'HLT] []]
      (cargar-const-en-tabla [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] :sin-errores [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []])
    ))
  )
)

; user=> (inicializar-contexto-local [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] 8 [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []])
; [{ (let x : i64 = 10 ; println! ( "{}" , x ) }) [fn main ( )] 8 [[0] [[main [fn [() ()]] 2]]] 0 [[CAL 2] HLT] []]
(deftest inicializar-contexto-local-con-errores
  (testing "inicializar-contexto-local con errores"
    (is (=
      [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] 8 [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []]
      (inicializar-contexto-local [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] 8 [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []])
    ))
  )
)


; user=> (inicializar-contexto-local [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] :sin-errores [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []])
; [{ (let x : i64 = 10 ; println! ( "{}" , x ) }) [fn main ( )] :sin-errores [[0 1] [[main [fn [() ()]] 2]]] 0 [[CAL 2] HLT] []]
(deftest inicializar-contexto-local-sin-errores
  (testing "inicializar-contexto-local sin errores"
    (is (=
      [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] :sin-errores [[0 1] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []]
      (inicializar-contexto-local [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] :sin-errores [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []])
    ))
  )
)

; user=> (restaurar-contexto-anterior ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] 8 [[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]])
; [EOF () [fn main ( ) { let x : i64 = 10 ; let y : i64 = 20 ; println! ( "{}" , x + y ) }] 8 [[0 1] [[main [fn [() ()]] 2] [x [var-inmut i64] 0] [y [var-inmut i64] 1]]] 2 [[CAL 2] HLT [PUSHFI 10] [POP 0] [PUSHFI 20] [POP 1] [PUSHFI "{}"] [PUSHFM 0] [PUSHFM 1] ADD [PUSHFI 2] OUT NL] [[2 [i64 nil] [i64 nil]]]]
(deftest restaurar-contexto-errores
  (testing "restaurar-contexto errores"
    (is (=
      ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] 8 [[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]]
      (restaurar-contexto-anterior ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] 8 [[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]])
    ))
  )
)

; user=> (restaurar-contexto-anterior ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] :sin-errores [[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]])
; [EOF () [fn main ( ) { let x : i64 = 10 ; let y : i64 = 20 ; println! ( "{}" , x + y ) }] :sin-errores [[0] [[main [fn [() ()]] 2]]] 2 [[CAL 2] HLT [PUSHFI 10] [POP 0] [PUSHFI 20] [POP 1] [PUSHFI "{}"] [PUSHFM 0] [PUSHFM 1] ADD [PUSHFI 2] OUT NL] [[2 [i64 nil] [i64 nil]]]]
(deftest restaurar-contexto
  (testing "restaurar-contexto"
    (is (=
      ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] :sin-errores [[0] [['main ['fn [() ()]] 2]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]]
      (restaurar-contexto-anterior ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] :sin-errores [[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]])
    ))
  )
)

; user=> (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 2)
; i64
(deftest buscar-tipo-de-retorno-i64
  (testing "buscar-tipo-de-retorno i64"
    (is (=
      'i64
      (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 2)
    ))
  )
)

; user=> (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 8)
; ()
(deftest buscar-tipo-de-retorno-void
  (testing "buscar-tipo-de-retorno void"
    (is (=
      '()
      (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 8)
    ))
  )
)

; user=> (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 1)
; nil
(deftest buscar-tipo-de-retorno-nil
  (testing "buscar-tipo-de-retorno nil"
    (is (=
      nil
      (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 1)
    ))
  )
)

; user=> (generar-ref [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] 8 [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]])
; [) (; println! ( "{}" , v ) ; }) [fn inc ( v : & mut i64 ) { * v += 1 ; } fn main ( ) { let mut v : i64 = 5 ; inc ( & mut v] 8 [[0 2] [[inc [fn [([v : & mut i64]) ()]] 2] [main [fn [() ()]] 6] [v [var-mut i64] 0]]] 1 [[CAL 6] HLT [POPARG 0] [PUSHFI 1] [POPADDREF 0] RETN [PUSHFI 5] [POP 0]] [[2 [i64 nil]] [6 [i64 nil]]]]
(deftest generar-ref-error
  (testing "generar-ref error"
    (is (=
      [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] 8 [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]]
      (generar-ref [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] 8 [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]])
    ))
  )
)

; user=> (generar-ref [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] :sin-errores [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]])
; [) (; println! ( "{}" , v ) ; }) [fn inc ( v : & mut i64 ) { * v += 1 ; } fn main ( ) { let mut v : i64 = 5 ; inc ( & mut v] :sin-errores [[0 2] [[inc [fn [([v : & mut i64]) ()]] 2] [main [fn [() ()]] 6] [v [var-mut i64] 0]]] 1 [[CAL 6] HLT [POPARG 0] [PUSHFI 1] [POPADDREF 0] RETN [PUSHFI 5] [POP 0] [PUSHADDR 0]] [[2 [i64 nil]] [6 [i64 nil]]]]
(deftest generar-ref-sin-errores
  (testing "generar-ref sin errores"
    (is (=
      [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] :sin-errores [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0] ['PUSHADDR 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]]
      (generar-ref [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] :sin-errores [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]])
    ))
  )
)

; user=> (fixup [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] 8 [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]] 4)
; [{ (x = 20 ; } ; println! ( "{}" , x ) }) [fn main ( ) { let x : i64 ; if false { x = 10 ; } else] 8 [[0 1 2] [[main [fn [() ()]] 2] [x [var-inmut i64] 0]]] 1 [[CAL 2] HLT [PUSHFI false] [JC 5] [JMP ?] [PUSHFI 10] [POP 0] [JMP ?]] [[2 [i64 nil]]]]
(deftest fixup-error
  (testing "fixup error"
    (is (=
      [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] 8 [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]]
      (fixup [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] 8 [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]] 4)
    ))
  )
)

; user=> (fixup [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] :sin-errores [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]] 4)
; [{ (x = 20 ; } ; println! ( "{}" , x ) }) [fn main ( ) { let x : i64 ; if false { x = 10 ; } else] :sin-errores [[0 1 2] [[main [fn [() ()]] 2] [x [var-inmut i64] 0]]] 1 [[CAL 2] HLT [PUSHFI false] [JC 5] [JMP 8] [PUSHFI 10] [POP 0] [JMP ?]] [[2 [i64 nil]]]]
(deftest fixup-sin-error
  (testing "fixup sin error"
    (is (=
      [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] :sin-errores [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP 8] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]]
      (fixup [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] :sin-errores [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]] 4)
    ))
  )
)

; user=> (convertir-formato-impresion '("Hola, mundo!"))
; ("Hola, mundo!")
(deftest convertir-formato-impresion-hola-mundo
  (testing "convertir-formato-impresion hola mundo"
    (is (=
      '("Hola, mundo!")
      (convertir-formato-impresion '("Hola, mundo!"))
    ))
  )
)

; user=> (convertir-formato-impresion '("- My name is {}, James {}.\n- Hello, {}{}{}!" "Bond" "Bond" 0 0 7))
; ("- My name is %s, James %s.\n- Hello, %d%d%d!" "Bond" "Bond" 0 0 7)
(deftest convertir-formato-impresion-james-bond
  (testing "convertir-formato-impresion james bond"
    (is (=
      '("- My name is %s, James %s.\n- Hello, %d%d%d!" "Bond" "Bond" 0 0 7)
      (convertir-formato-impresion '("- My name is {}, James {}.\n- Hello, {}{}{}!" "Bond" "Bond" 0 0 7))
    ))
  )
)

; user=> (convertir-formato-impresion '("{} elevado a la {} es\t{}" 2.0 2 4.0))
; ("%.0f elevado a la %d es\t%.0f" 2.0 2 4.0)
(deftest convertir-formato-impresion-floats
  (testing "convertir-formato-impresion floats"
    (is (=
      '("%.0f elevado a la %d es\t%.0f" 2.0 2 4.0)
      (convertir-formato-impresion '("{} elevado a la {} es\t{}" 2.0 2 4.0))
    ))
  )
)

; user=> (convertir-formato-impresion '("Las raices cuadradas de {} son +{:.8} y -{:.8}" 4.0 1.999999999985448 1.999999999985448))
; ("Las raices cuadradas de %.0f son +%.8f y -%.8f" 4.0 1.999999999985448 1.999999999985448)
(deftest convertir-formato-impresion-floats-digitos
  (testing "convertir-formato-impresion floats digitos"
    (is (=
      '("Las raices cuadradas de %.0f son +%.8f y -%.8f" 4.0 1.999999999985448 1.999999999985448)
      (convertir-formato-impresion '("Las raices cuadradas de {} son +{:.8} y -{:.8}" 4.0 1.999999999985448 1.999999999985448))
    ))
  )
)

; user=> (dividir 12 3)
; 4
(deftest dividir-12-3
  (testing "dividir 12 3"
    (is (= 4 (dividir 12 3)))
  )
)

; user=> (dividir 12.0 3)
; 4.0
(deftest dividir-120-3
  (testing "dividir 12.0 3"
    (is (= 4.0 (dividir 12.0 3)))
  )
)

; user=> (dividir 12 3.0)
; 4.0
(deftest dividir-12-30
  (testing "dividir 12 3.0"
    (is (= 4.0 (dividir 12 3.0)))
  )
)

; user=> (dividir 12.0 3.0)
; 4.0
(deftest dividir-120-30
  (testing "dividir 12.0 3.0"
    (is (= 4.0 (dividir 12.0 3.0)))
  )
)

; user=> (dividir 1 2)
; 0
(deftest division-entera
  (testing "division entera"
    (is (= 0 (dividir 1 2)))
  )
)

; user=> (dividir 1 2.0)
; 0.5
(deftest division-float
  (testing "division float"
    (is (= 0.5 (dividir 1 2.0)))
  )
)

; user=> (compatibles? 'i64 5)
; true
(deftest compatibles-i64-5
  (testing "compatibles i64 5"
    (is (= true (compatibles? 'i64 5)))
  )
)

; user=> (compatibles? 'i64 5.0)
; false
(deftest compatibles-i64-5.0
  (testing "compatibles i64 5.0"
    (is (= false (compatibles? 'i64 5.0)))
  )
)

; user=> (compatibles? 'i64 [5.0])
; true
(deftest compatibles-i64-5.0-vector
  (testing "compatibles i64 5.0 vector"
    (is (= true (compatibles? 'i64 [5.0])))
  )
)

; user=> (compatibles? 'f64 5.0)
; true
(deftest compatibles-f64-5.0
  (testing "compatibles f64 5.0"
    (is (= true (compatibles? 'f64 5.0)))
  )
)

; user=> (compatibles? 'String "Hola")
; true
(deftest compatibles-string-hola
  (testing "compatibles string hola"
    (is (= true (compatibles? 'String "Hola")))
  )
)

; user=> (compatibles? 'bool true)
; true
(deftest compatibles-bool-true
  (testing "compatibles bool true"
    (is (= true (compatibles? 'bool true)))
  )
)

; user=> (compatibles? 'bool 1)
; false
(deftest compatibles-bool-1
  (testing "compatibles bool 1"
    (is (= false (compatibles? 'bool 1)))
  )
)

; user=> (compatibles? 'usize 1)
; true
(deftest compatibles-usize-1
  (testing "compatibles usize 1"
    (is (= true (compatibles? 'usize 1)))
  )
)

; user=> (compatibles? 'char \a)
; true
(deftest compatibles-char-a
  (testing "compatibles char a"
    (is (= true (compatibles? 'char \a)))
  )
)

; user=> (compatibles? 'char 'a)
; false
(deftest compatibles-char-a-simbolo
  (testing "compatibles char a simbolo"
    (is (= false (compatibles? 'char 'a)))
  )
)

; user=> (compatibles? 'char ['a])
; true
(deftest compatibles-char-a-vector
  (testing "compatibles char a vector"
    (is (= true (compatibles? 'char ['a])))
  )
)

(deftest compatibles-negative-usize
  (testing "compatibles negative usize"
    (is (= false (compatibles? 'usize -1)))
  )
)

; user=> (pasar-a-int "10")
; 10
(deftest parsear-a-int-string
  (testing "parsear a int string"
    (is (= 10 (pasar-a-int "10")))
  )
)

; user=> (pasar-a-int 10.0)
; 10
(deftest parsear-a-int-float
  (testing "parsear a int float"
    (is (= 10 (pasar-a-int 10.0)))
  )
)

; user=> (pasar-a-int 10)
; 10
(deftest parsear-a-int-int
  (testing "parsear a int int"
    (is (= 10 (pasar-a-int 10)))
  )
)

; user=> (pasar-a-int 'a)
; a
(deftest parsear-a-int-simbolo
  (testing "parsear a int simbolo"
    (is (= 'a (pasar-a-int 'a)))
  )
)

; user=> (pasar-a-int [10.0])
; [10.0]
(deftest parsear-a-int-vector
  (testing "parsear a int vector"
    (is (= [10.0] (pasar-a-int [10.0])))
  )
)

; user=> (pasar-a-float "10")
; 10.0
(deftest parsear-a-float-string
  (testing "parsear a float string"
    (is (= 10.0 (pasar-a-float "10")))
  )
)

; user=> (pasar-a-float 10)
; 10.0
(deftest parsear-a-float-int
  (testing "parsear a float int"
    (is (= 10.0 (pasar-a-float 10)))
  )
)

; user=> (pasar-a-float 10.0)
; 10.0
(deftest parsear-a-float-float
  (testing "parsear a float float"
    (is (= 10.0 (pasar-a-float 10.0)))
  )
)

; user=> (pasar-a-float 'a)
; a
(deftest parsear-a-float-simbolo
  (testing "parsear a float simbolo"
    (is (= 'a (pasar-a-float 'a)))
  )
)

; user=> (pasar-a-float [10])
; [10]
(deftest parsear-a-float-vector
  (testing "parsear a float vector"
    (is (= [10] (pasar-a-float [10])))
  )
)
