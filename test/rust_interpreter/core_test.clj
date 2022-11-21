(ns rust-interpreter.core-test
  (:require [clojure.test :refer :all]
            [rust-interpreter.core :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TESTS UNITARIOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; user=> (listar (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "Hola, mundo!" (symbol ")") (symbol "}")))
; fn main ( )
; {
;   println! ( "Hola, mundo!" )
; }
; 
; nil

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

(deftest convertir-formato-impresion-float-no-redondo
  (testing "convertir-formato-impresion float no redondo"
    (is (=
      '("%f" 4.1)
      (convertir-formato-impresion '("{}" 4.1))
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

; user=> (cargar-en-ult-reg [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['i64 nil] ['i64 nil]]] 1 'i64 0)
; [[[String "2"] [i64 6] [i64 2] [i64 3] [i64 0]] [[i64 nil] [i64 0]]]
(deftest cargar-en-ult-reg-direccion-1
  (testing "cargar-en-ult-reg direccion 1"
    (is (=
      [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['i64 nil] ['i64 0]]]
      (cargar-en-ult-reg [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['i64 nil] ['i64 nil]]] 1 'i64 0)
    ))
  )
)

; user=> (cargar-en-ult-reg [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['i64 nil] ['i64 0]]] 0 'f64 3)
; [[[String "2"] [i64 6] [i64 2] [i64 3] [i64 0]] [[f64 3] [i64 0]]]
(deftest cargar-en-ult-reg-direccion-0
  (testing "cargar-en-ult-reg direccion 0"
    (is (=
      [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['f64 3] ['i64 0]]]
      (cargar-en-ult-reg [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['i64 nil] ['i64 0]]] 0 'f64 3)
    ))
  )
)

; user=> (cargar-en-reg-dest [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 2]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] [0 4] 'i64 0)
; [[[String "2"] [i64 6] [i64 2] [i64 2] [i64 0]] [[i64 6] [i64 2] [i64 [0 3]] [i64 [0 4]] [i64 2] [i64 2]]]
(deftest cargar-en-reg-dest-direccion-0-4
  (testing "cargar-en-reg-dest direccion 0 4"
    (is (=
      [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]]
      (cargar-en-reg-dest [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 2]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] [0 4] 'i64 0)
    ))
  )
)

; user=> (cargar-en-reg-dest [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] [0 3] 'f64 3)
; [[[String "2"] [i64 6] [i64 2] [f64 3] [i64 0]] [[i64 6] [i64 2] [i64 [0 3]] [i64 [0 4]] [i64 2] [i64 2]]]
(deftest cargar-en-reg-dest-direccion-0-3
  (testing "cargar-en-reg-dest direccion 0 3"
    (is (=
      [[['String "2"] ['i64 6] ['i64 2] ['f64 3] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]]
      (cargar-en-reg-dest [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] [0 3] 'f64 3)
    ))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TESTS DE INTEGRACION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; escan main01.rs
;; fn main ( )
;; {
;;  println! ( "- Hola, mundo!" ) ;
;;  print! ( "- My name is {}, James {}.\n- Hello, {}{}{}!" , "Bond" , "Bond" , - 2 + 2 , 0 , 3 + 2 * 2 ) ;
;;  println! ( ) ;
;;  println! ( "- Hasta la vista, Baby!\t\tI'll be back..." ) ;
;;  println! ( "{}" , if true
;;  {
;;  "- Lo dudo!\t\t\tBye!"
;;  }
;;  else
;;  {
;;  "- Obviamente!"
;;  }
;;  )
;; }

; virtu main01.rs
;; 0 [CAL 2]
;; 1 HLT
;; 2 [PUSHFI "- Hola, mundo!"]
;; 3 [PUSHFI 1]
;; 4 OUT
;; 5 NL
;; 6 [PUSHFI "- My name is {}, James {}.\n- Hello, {}{}{}!"]
;; 7 [PUSHFI "Bond"]
;; 8 [PUSHFI "Bond"]
;; 9 [PUSHFI 2]
;; 10 NEG
;; 11 [PUSHFI 2]
;; 12 ADD
;; 13 [PUSHFI 0]
;; 14 [PUSHFI 3]
;; 15 [PUSHFI 2]
;; 16 [PUSHFI 2]
;; 17 MUL
;; 18 ADD
;; 19 [PUSHFI 6]
;; 20 OUT
;; 21 [PUSHFI 0]
;; 22 OUT
;; 23 NL
;; 24 [PUSHFI "- Hasta la vista, Baby!\t\tI'll be back..."]
;; 25 [PUSHFI 1]
;; 26 OUT
;; 27 NL
;; 28 [PUSHFI "{}"]
;; 29 [PUSHFI true]
;; 30 [JC 32]
;; 31 [JMP 34]
;; 32 [PUSHFI "- Lo dudo!\t\t\tBye!"]
;; 33 [JMP 35]
;; 34 [PUSHFI "- Obviamente!"]
;; 35 [PUSHFI 2]
;; 36 OUT
;; 37 NL
;; 38 RETN

;inter main01.rs
;; - Hola, mundo!
;; - My name is Bond, James Bond.
;; - Hello, 007!
;; - Hasta la vista, Baby! I'll be back...
;; - Lo dudo! Bye!

; escan main02
;; use std :: io ;
;; use std :: io :: Write ;
;; fn main ( )
;; {
;;  println! ( "************************************************************" ) ;
;;  println! ( "Se ingresan dos valores enteros, se muestra su producto." ) ;
;;  println! ( "Se utiliza el algoritmo de 'multiplicacion por duplicacion'." ) ;
;;  println! ( "(Metodo campesino ruso de multiplicacion)" ) ;
;;  println! ( "************************************************************" ) ;
;;  print! ( "x: " ) ;
;;  io :: stdout ( ) . flush ( ) . expect ( "Error de escritura!" ) ;
;;  let mut renglon : String = String :: new ( ) ;
;;  io :: stdin ( ) . read_line ( & mut renglon ) . expect ( "Error de lectura!" ) ;
;;  let mut x : i64 = renglon . trim ( ) . parse :: < i64 > ( ) . expect ( "Se esperaba un numero entero!" ) ;
;;  let mut x_cambio : bool = false ;
;;  if x < 0
;;  {
;;  x = - x ;
;;  x_cambio = true ;
;;  }
;;  print! ( "y: " ) ;
;;  io :: stdout ( ) . flush ( ) . expect ( "Error de escritura!" ) ;
;;  renglon = String :: new ( ) ;
;;  io :: stdin ( ) . read_line ( & mut renglon ) . expect ( "Error de lectura!" ) ;
;;  let mut y : i64 = renglon . trim ( ) . parse :: < i64 > ( ) . expect ( "Se esperaba un numero entero!" ) ;
;;  let mut y_cambio : bool = false ;
;;  if y < 0
;;  {
;;  y = - y ;
;;  y_cambio = true ;
;;  }
;;  let mut prod : i64 = 0 ;
;;  while y > 0
;;  {
;;  if y % 2 != 0
;;  {
;;  prod += x ;
;;  }
;;  x *= 2 ;
;;  y /= 2 ;
;;  }
;;  if x_cambio
;;  {
;;  prod = - prod ;
;;  }
;;  if y_cambio
;;  {
;;  prod = - prod ;
;;  }
;;  println! ( "x*y={}" , prod ) ;
;; }

; virtu main01
;; 0 [CAL 2]
;; 1 HLT
;; 2 [PUSHFI "************************************************************"]
;; 3 [PUSHFI 1]
;; 4 OUT
;; 5 NL
;; 6 [PUSHFI "Se ingresan dos valores enteros, se muestra su producto."]
;; 7 [PUSHFI 1]
;; 8 OUT
;; 9 NL
;; 10 [PUSHFI "Se utiliza el algoritmo de 'multiplicacion por duplicacion'."]
;; 11 [PUSHFI 1]
;; 12 OUT
;; 13 NL
;; 14 [PUSHFI "(Metodo campesino ruso de multiplicacion)"]
;; 15 [PUSHFI 1]
;; 16 OUT
;; 17 NL
;; 18 [PUSHFI "************************************************************"]
;; 19 [PUSHFI 1]
;; 20 OUT
;; 21 NL
;; 22 [PUSHFI "x: "]
;; 23 [PUSHFI 1]
;; 24 OUT
;; 25 FLUSH
;; 26 [PUSHFI ""]
;; 27 [POP 0]
;; 28 [IN 0]
;; 29 [PUSHFM 0]
;; 30 TOI
;; 31 [POP 1]
;; 32 [PUSHFI false]
;; 33 [POP 2]
;; 34 [PUSHFM 1]
;; 35 [PUSHFI 0]
;; 36 LT
;; 37 [JC 39]
;; 38 [JMP 44]
;; 39 [PUSHFM 1]
;; 40 NEG
;; 41 [POP 1]
;; 42 [PUSHFI true]
;; 43 [POP 2]
;; 44 [PUSHFI "y: "]
;; 45 [PUSHFI 1]
;; 46 OUT
;; 47 FLUSH
;; 48 [PUSHFI ""]
;; 49 [POP 0]
;; 50 [IN 0]
;; 51 [PUSHFM 0]
;; 52 TOI
;; 53 [POP 3]
;; 54 [PUSHFI false]
;; 55 [POP 4]
;; 56 [PUSHFM 3]
;; 57 [PUSHFI 0]
;; 58 LT
;; 59 [JC 61]
;; 60 [JMP 66]
;; 61 [PUSHFM 3]
;; 62 NEG
;; 63 [POP 3]
;; 64 [PUSHFI true]
;; 65 [POP 4]
;; 66 [PUSHFI 0]
;; 67 [POP 5]
;; 68 [PUSHFM 3]
;; 69 [PUSHFI 0]
;; 70 GT
;; 71 [JC 73]
;; 72 [JMP 87]
;; 73 [PUSHFM 3]
;; 74 [PUSHFI 2]
;; 75 MOD
;; 76 [PUSHFI 0]
;; 77 NEQ
;; 78 [JC 80]
;; 79 [JMP 82]
;; 80 [PUSHFM 1]
;; 81 [POPADD 5]
;; 82 [PUSHFI 2]
;; 83 [POPMUL 1]
;; 84 [PUSHFI 2]
;; 85 [POPDIV 3]
;; 86 [JMP 68]
;; 87 [PUSHFM 2]
;; 88 [JC 90]
;; 89 [JMP 93]
;; 90 [PUSHFM 5]
;; 91 NEG
;; 92 [POP 5]
;; 93 [PUSHFM 4]
;; 94 [JC 96]
;; 95 [JMP 99]
;; 96 [PUSHFM 5]
;; 97 NEG
;; 98 [POP 5]
;; 99 [PUSHFI "x*y={}"]
;; 100 [PUSHFM 5]
;; 101 [PUSHFI 2]
;; 102 OUT
;; 103 NL
;; 104 RETN

; inter main02
;; ************************************************************
;; Se ingresan dos valores enteros, se muestra su producto.
;; Se utiliza el algoritmo de 'multiplicacion por duplicacion'.
;; (Metodo campesino ruso de multiplicacion)
;; ************************************************************
;; x: 12
;; y: 4
;; x*y=48

;escan main03
;; use std :: io ;
;; use std :: io :: Write ;
;; use std :: process ;
;; fn dividir ( x : i64 , y : i64 , q : & mut i64 , r : & mut i64 )
;; {
;;  if y == 0
;;  {
;;  println! ( "ERROR: Division por cero!" ) ;
;;  process :: exit ( 1 ) ;
;;  }
;;  * q = 0 ;
;;  * r = x ;
;;  if * r < 0
;;  {
;;  * r = - * r ;
;;  }
;;  let v : i64 ;
;;  let mut w : i64 ;
;;  if y >= 0
;;  {
;;  v = y ;
;;  w = y ;
;;  }
;;  else
;;  {
;;  v = - y ;
;;  w = - y ;
;;  }
;; while w <= * r
;;  {
;;  w *= 2 ;
;;  }
;;  while w > v
;;  {
;;  * q *= 2 ;
;;  w /= 2 ;
;;  if w <= * r
;;  {
;;  * r -= w ;
;;  * q += 1 ;
;;  }
;;  }
;;  if x < 0
;;  {
;;  * r = - * r ;
;;  * q = - * q ;
;;  }
;;  if y < 0
;;  {
;;  * q = - * q ;
;;  }
;; }
;; fn mostrar_salida ( cociente : i64 , resto : i64 )
;; {
;;  println! ( "Cociente: {}" , cociente ) ;
;;  println! ( "Resto: {}" , resto ) ;
;; }
;; fn main ( )
;; {
;;  println! ( "**************************************************************" ) ;
;;  println! ( "Se ingresan dos valores enteros, se muestra su cociente." ) ;
;;  println! ( "Se utiliza el algoritmo 'desplazar y restar' (shift-subtract)." ) ;
;;  println! ( "**************************************************************" ) ;
;;  print! ( "x: " ) ;
;;  io :: stdout ( ) . flush ( ) . expect ( "Error de escritura!" ) ;
;;  let mut renglon : String = String :: new ( ) ;
;;  io :: stdin ( ) . read_line ( & mut renglon ) . expect ( "Error de lectura!" ) ;
;;  let x : i64 = renglon . trim ( ) . parse :: < i64 > ( ) . expect ( "Se esperaba un numero entero!" ) ;
;;  print! ( "y: " ) ;
;;  io :: stdout ( ) . flush ( ) . expect ( "Error de escritura!" ) ;
;;  renglon = String :: new ( ) ;
;;  io :: stdin ( ) . read_line ( & mut renglon ) . expect ( "Error de lectura!" ) ;
;;  let y : i64 = renglon . trim ( ) . parse :: < i64 > ( ) . expect ( "Se esperaba un numero entero!" ) ;
;;  let mut q : i64 = 0 ;
;;  let mut r : i64 = 0 ;
;;  dividir ( x , y , & mut q , & mut r ) ;
;;  mostrar_salida ( q , r ) ;
;; }

; virtu main03
;; 0 [CAL 105]
;; 1 HLT
;; 2 [POPARG 3]
;; 3 [POPARG 2]
;; 4 [POPARG 1]
;; 5 [POPARG 0]
;; 6 [PUSHFM 1]
;; 7 [PUSHFI 0]
;; 8 EQ
;; 9 [JC 11]
;; 10 [JMP 17]
;; 11 [PUSHFI "ERROR: Division por cero!"]
;; 12 [PUSHFI 1]
;; 13 OUT
;; 14 NL
;; 15 [PUSHFI 1]
;; 16 HLT
;; 17 [PUSHFI 0]
;; 18 [POPREF 2]
;; 19 [PUSHFM 0]
;; 20 [POPREF 3]
;; 21 [PUSHREF 3]
;; 22 [PUSHFI 0]
;; 23 LT
;; 24 [JC 26]
;; 25 [JMP 29]
;; 26 [PUSHREF 3]
;; 27 NEG
;; 28 [POPREF 3]
;; 29 [PUSHFM 1]
;; 30 [PUSHFI 0]
;; 31 GTE
;; 32 [JC 34]
;; 33 [JMP 39]
;; 34 [PUSHFM 1]
;; 35 [POP 4]
;; 36 [PUSHFM 1]
;; 37 [POP 5]
;; 38 [JMP 45]
;; 39 [PUSHFM 1]
;; 40 NEG
;; 41 [POP 4]
;; 42 [PUSHFM 1]
;; 43 NEG
;; 44 [POP 5]
;; 45 [PUSHFM 5]
;; 46 [PUSHREF 3]
;; 47 LTE
;; 48 [JC 50]
;; 49 [JMP 53]
;; 50 [PUSHFI 2]
;; 51 [POPMUL 5]
;; 52 [JMP 45]
;; 53 [PUSHFM 5]
;; 54 [PUSHFM 4]
;; 55 GT
;; 56 [JC 58]
;; 57 [JMP 72]
;; 58 [PUSHFI 2]
;; 59 [POPMULREF 2]
;; 60 [PUSHFI 2]
;; 61 [POPDIV 5]
;; 62 [PUSHFM 5]
;; 63 [PUSHREF 3]
;; 64 LTE
;; 65 [JC 67]
;; 66 [JMP 71]
;; 67 [PUSHFM 5]
;; 68 [POPSUBREF 3]
;; 69 [PUSHFI 1]
;; 70 [POPADDREF 2]
;; 71 [JMP 53]
;; 72 [PUSHFM 0]
;; 73 [PUSHFI 0]
;; 74 LT
;; 75 [JC 77]
;; 76 [JMP 83]
;; 77 [PUSHREF 3]
;; 78 NEG
;; 79 [POPREF 3]
;; 80 [PUSHREF 2]
;; 81 NEG
;; 82 [POPREF 2]
;; 83 [PUSHFM 1]
;; 84 [PUSHFI 0]
;; 85 LT
;; 86 [JC 88]
;; 87 [JMP 91]
;; 88 [PUSHREF 2]
;; 89 NEG
;; 90 [POPREF 2]
;; 91 RETN
;; 92 [POPARG 1]
;; 93 [POPARG 0]
;; 94 [PUSHFI "Cociente: {}"]
;; 95 [PUSHFM 0]
;; 96 [PUSHFI 2]
;; 97 OUT
;; 98 NL
;; 99 [PUSHFI "Resto: {}"]
;; 100 [PUSHFM 1]
;; 101 [PUSHFI 2]
;; 102 OUT
;; 103 NL
;; 104 RETN
;; 105 [PUSHFI "**************************************************************"]
;; 106 [PUSHFI 1]
;; 107 OUT
;; 108 NL
;; 109 [PUSHFI "Se ingresan dos valores enteros, se muestra su cociente."]
;; 110 [PUSHFI 1]
;; 111 OUT
;; 112 NL
;; 113 [PUSHFI "Se utiliza el algoritmo 'desplazar y restar' (shift-subtract)."]
;; 114 [PUSHFI 1]
;; 115 OUT
;; 116 NL
;; 117 [PUSHFI "**************************************************************"]
;; 118 [PUSHFI 1]
;; 119 OUT
;; 120 NL
;; 121 [PUSHFI "x: "]
;; 122 [PUSHFI 1]
;; 123 OUT
;; 124 FLUSH
;; 125 [PUSHFI ""]
;; 126 [POP 0]
;; 127 [IN 0]
;; 128 [PUSHFM 0]
;; 129 TOI
;; 130 [POP 1]
;; 131 [PUSHFI "y: "]
;; 132 [PUSHFI 1]
;; 133 OUT
;; 134 FLUSH
;; 135 [PUSHFI ""]
;; 136 [POP 0]
;; 137 [IN 0]
;; 138 [PUSHFM 0]
;; 139 TOI
;; 140 [POP 2]
;; 141 [PUSHFI 0]
;; 142 [POP 3]
;; 143 [PUSHFI 0]
;; 144 [POP 4]
;; 145 [PUSHFM 1]
;; 146 [PUSHFM 2]
;; 147 [PUSHADDR 3]
;; 148 [PUSHADDR 4]
;; 149 [CAL 2]
;; 150 [PUSHFM 3]
;; 151 [PUSHFM 4]
;; 152 [CAL 92]
;; 153 RETN

; inter main03
;; **************************************************************
;; Se ingresan dos valores enteros, se muestra su cociente.
;; Se utiliza el algoritmo 'desplazar y restar' (shift-subtract).
;; **************************************************************
;; x: 23
;; y: 5
;; Cociente: 4
;; Resto: 3

; escan main04
;; use std :: io ;
;; use std :: io :: Write ;
;; use std :: process ;
;; fn mcd ( mut x : i64 , mut y : i64 ) -> i64
;; {
;;  if x <= 0 || y <= 0
;;  {
;;  println! ( "ERROR: El algoritmo requiere dos numeros enteros positivos!" ) ;
;;  process :: exit ( 1 ) ;
;;  }
;;  while x != y
;;  {
;;  if x < y
;;  {
;;  y -= x ;
;;  }
;;  if y < x
;;  {
;;  x -= y ;
;;  }
;;  }
;;  x
;; }
;; fn main ( )
;; {
;;  println! ( "******************************************************************************" ) ;
;;  println! ( "Se ingresan dos valores enteros positivos, se muestra su maximo comun divisor." ) ;
;;  println! ( "Se utiliza el algoritmo de Euclides." ) ;
;;  println! ( "******************************************************************************" ) ;
;;  print! ( "x: " ) ;
;;  io :: stdout ( ) . flush ( ) . expect ( "Error de escritura!" ) ;
;;  let mut renglon : String = String :: new ( ) ;
;;  io :: stdin ( ) . read_line ( & mut renglon ) . expect ( "Error de lectura!" ) ;
;;  let x : i64 = renglon . trim ( ) . parse :: < i64 > ( ) . expect ( "Se esperaba un numero entero!" ) ;
;;  print! ( "y: " ) ;
;;  io :: stdout ( ) . flush ( ) . expect ( "Error de escritura!" ) ;
;;  renglon = String :: new ( ) ;
;;  io :: stdin ( ) . read_line ( & mut renglon ) . expect ( "Error de lectura!" ) ;
;;  let y : i64 = renglon . trim ( ) . parse :: < i64 > ( ) . expect ( "Se esperaba un numero entero!" ) ;
;;  print! ( "{} es el MCD entre " , mcd ( x , y ) ) ;
;;  println! ( "{} y {}" , x , y ) ;
;; }

;virtu main04

;inter main04

;escan main05

; virtu main05

; inter main05

;escan main06

; virtu main06

; inter main06

;escan main07

; virtu main07

; inter main07

;escan main08

; virtu main08

; inter main08

;escan main09

; virtu main09

; inter main09

;escan main10

; virtu main10

; inter main10