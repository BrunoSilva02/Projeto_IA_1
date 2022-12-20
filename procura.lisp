;; Algoritmos de Procura
;; 
;; Aurélio Miranda - 202000572
;; Bruno Silva - 202002258


;;*********************************** variaveis de teste e operadores ********************************************************************

(defun no-teste-a ()
"Define um no teste do problema"
 (cria-no (tabuleiro-problema-a) 0 0 nil 3))

 (defun no-teste-b ()
"Define um no teste do problema"
 (cria-no (tabuleiro-problema-b) 0 0 nil 7))

 (defun no-teste-c ()
"Define um no teste do problema"
 (cria-no (tabuleiro-problema-c) 0 0 nil 10))

 (defun no-teste-d ()
"Define um no teste do problema"
 (cria-no (tabuleiro-problema-d) 0 0 nil 10))

(defun operadores ()
 "Cria uma lista com todos os operadores do problema."
 (list 'arco-vertical 'arco-horizontal))

;;; Construtor
(defun cria-no (tabuleiro &optional (g 0) (h 99) (pai nil) (o 5))
  "Cria um no representante do estado do problema"
  (list tabuleiro g h pai o)
)

;;; Metodos seletores
;; no-estado
;; teste: (no-estado (no-teste))
;; resultado: (((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 1) (1 0 1) (0 1 1)))
(defun no-estado (no)
  "Retorna o estado"
  (car no)
)


;; no-profundidade
;; teste: (no-profundidade (no-teste))
;; resultado: 0
(defun no-profundidade (no)
  (cadr no)
)

;; no-heuristica
;; teste: (no-heuristica (no-teste))
;; resultado: 999
(defun no-heuristica (no)
  (caddr no)
)

;; no-pai
;; teste: (no-pai (no-teste))
;; resultado: NIL
(defun no-pai (no)
  (cadddr no)
)

;; no-objetivo
;; teste: (no-objetivo (no-teste))
;; resultado: NIL
(defun no-objetivo (no)
  (car(cddddr no))
)

;;; Funcoes auxiliares do no
;;; predicado no-solucaop que verifica se um estado e final
;; teste: (no-solucaop (no-teste))
;; resultado: NIL
(defun no-solucaop (no)
  (cond ((eq (contar-caixas-fechadas (no-estado no)) (no-objetivo no)) T)
   (t NIL))
)

;;; função que calcula a heuristica de um nó
;; teste: (heuristica (no-teste))
;; resultado: 9
(defun heuristica (no)
  (- (no-objetivo no) (contar-caixas-fechadas (no-estado no)))
)


;;; Funcao auxiliar da geração de nos
;;; gera um novo nó
;; teste: (novo-sucessor (no-teste) 1 1 (car (operadores)))
;; resultado: ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((1 0 0) (0 1 1) (1 0 1) (0 1 1))) 1 9
;;            ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 1) (1 0 1) (0 1 1))) 0 99 NIL 10) 10)
(defun novo-sucessor(no l i func)
  (cond
    ((null (funcall func l i (no-estado no))) NIL)
    (t (cria-no (funcall func l i (no-estado no)) (+ (no-profundidade no) 1) 
        (heuristica no) no (no-objetivo no))
    )
  )
)

;;; Funcao geradora de nos
;;; gera todos os nos filho
;; teste: (sucessores-bfs (no-teste-a) (operadores))
;; resultado: (((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((1 0 0) (0 1 0) (0 0 1) (0 1 1))) 1 2
;;  ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1))) 0 0
;;   NIL 3)
;;  3)
;; ((((0 0 0) ...
(defun sucessores-bfs (no funcs)
  (remove nil 
    (append-list 
      (mapcar (lambda (funcao) 
        (mapcar (lambda (l-i) 
          (novo-sucessor no (first l-i)(second l-i) funcao)
          ) (encontrar-l-i (no-estado no))
        )
      )funcs)
    )
  )
)

;;; Funcao geradora de nos
;;; gera todos os nos filho
;; teste: (sucessores-dfs (no-teste) (operadores) 5)
;; resultado: ??
(defun sucessores-dfs (no funcs prof)
  (cond
    ((>= (no-profundidade no) maxProf) NIL)
    (T (remove nil 
        (append-list 
          (mapcar (lambda (funcao) 
            (mapcar (lambda (l-i) 
              (novo-sucessor no (first l-i)(second l-i) funcao)
              ) (encontrar-l-i (no-estado no))
            )
          )funcs)
        )
      ))
   )
)

;; Ordenação para Breadth-First Search
(defun abertos-bfs (abertos sucessores)
  "Ordenação para Breadth-First Search"
  (cond
     ((null abertos)sucessores)
     (T (append abertos sucessores))
   )
)

;; Ordenação para Depth-First Search
(defun abertos-dfs (abertos sucessores)
 "Ordenação para Depth-First Search"
   (cond
     ((null abertos)sucessores)
     (T (append sucessores abertos))
   )
)

;; Verificar Existência de um Nó
;; teste: (desenvolver teste)
;; resultado: -
(defun no-existep(no lista &optional alg)
    "Verifica a existência de um Nó na lista"
    (cond
         ((null lista) NIL)
         ((equal (no-estado no) (no-estado (car lista))) T)
         (T (no-existep no (cdr lista)))
     )
)

;; Devolve uma lista sem sucessores presentes nos nos fechados
;; teste: (desenvolver teste)
;; resultado: -
(defun no-diff (sucessores fechados)
   "Devolve apenas nos que sejam diferentes aos nos em fechados"
   (cond
       ((null sucessores) NIL)
       ((null fechados) sucessores)
       ((no-existep (car sucessores) fechados) (no-diff (cdr sucessores) fechados))
       (T (cons (car sucessores) (no-diff (cdr sucessores) fechados)))
   )
)

;; Devolve um no da lista caso seja o no objetivo
;; teste: (desenvolver teste)
;; resultado: -
(defun no-obj (list solucao)
  "Devolve NIL ou o no objetivo"
  (cond
      ((null list) NIL)
      ((funcall solucao (car list)) (car list))
      (T (no-obj (cdr list)solucao))
   )
)



;; -------------------------------------------------------------- Funções auxiliares ------------------------------------------------------


;; (trace encontrar-l-i)
;; teste - (encontrar-l-i (tabuleiro-problema-a))
;; resultado - ((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3) (4 1) (4 2) (4 3))
;; teste - (encontrar-l-i (tabuleiro-problema-b))
;; resultado - ((1 1) (1 2) (1 3) (1 4) (2 1) (2 2) (2 3) (2 4) (3 1) (3 2) (3 3) (3 4) (4 1) (4 2) (4 3) (4 4) (5 1) (5 2) (5 3) (5 4))
(defun encontrar-l-i (tabuleiro &optional (l 1) (i 1))
  "Encontra dos os l e i válidos para um dado tabuleiro"
  (cond
    ((< (length (get-arcos-verticais tabuleiro)) l) NIL)
    ((< (length (get-arcos-horizontais tabuleiro)) (+ i 1)) 
        (encontrar-l-i tabuleiro (+ l 1) 1))
    (T (cons (list l i) (encontrar-l-i tabuleiro l (+ i 1))))
  )
)


;; Junta o primeiro e o segundo elemento de uma lista num só elemento
(defun append-list (list)
  "Junta os 2 primeiros elementos de uma lista num só."
  (append (first list) (second list))
)



;; --------------------------------------------------------- Algoritmos ---------------------------------------------------------------------
;; Procura em largura
;; (trace bfs)
;; teste: (bfs (no-teste-a) 'no-solucaop 'sucessores-bfs (operadores) nil nil)
;; resultado: ((((0 0 0) (0 1 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 1 1) (0 1 1))) 2 1
;; ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 1 1) (0 1 1))) 1 2
;;  ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1))) 0 0
;;   NIL 3) 3) 3)
; sucessores-bfs (no funcs)
(defun bfs(no solucao sucessores operadores &optional abertos fechados)
  "Define a função bfs que irá efetuar a procura em largura-primeiro"
   (cond
       ((and (null abertos)(null fechados)) 
          (bfs no solucao sucessores operadores (list no) fechados))
       ((null abertos) nil)
       ((funcall solucao (car abertos)) 
          (car abertos))
       (T (bfs no solucao sucessores operadores 
            (abertos-bfs (cdr abertos) (no-diff (funcall sucessores (car abertos) operadores) fechados)) 
            (append fechados (list (car abertos))))
        )
    )
)

;; procura na profundidade
;; teste: (dfs (no-teste-a) 'no-solucaop 'sucessores-bfs 3 (operadores) nil nil)
;; resultado: ! acabar sucessores primeiro ! (incompleto)
(defun dfs(no solucao sucessores operadores profundidade &optional abertos fechados)
    "Defina a função dfs que irá efetuar a procura em profundidade-primeiro"
   (cond
      ((eq (no-profundidade no) profundidade) nil)
       ((and(null abertos)(null fechados)) 
        (dfs no solucao sucessores operadores profundidade (list no) fechados))
       ((funcall solucao no) no)
       ((null abertos)NIL)
       (T (dfs no solucao sucessores operadores profundidade 
            (abertos-bfs (cdr abertos) (no-diff (funcall sucessores (car abertos) operadores) fechados)) 
            (append fechados (list (car abertos))))
      )
    )
)



;; -------------------------------------------------------- Funções de leitura ---------------------------------------------------------------

;; Define funções de leitura para interação com utilizador
;; ler-profundidade
(defun ler-profundidade()
"Permite fazer a leitura da profundidade limite (dfs)."
    (progn
    (format t "Qual a profundidade limite? ~%")
    (read)
    ))


;; ler-algoritmo
(defun ler-algoritmo ()
"Permite fazer a leitura do algoritmo a utilizar."
  (progn
    (format t "Algoritmo a utilizar? ~%")
    (format t "1- Breadth-First Search ~%")
    (format t "2- Depth-First Search ~%")
    (let ((resposta (read)))
      (cond ((= resposta 1) 'bfs)
            (T 'dfs)))
    )
  )


  ;; BF

;; 1. NÓ INICIAL -> LISTA ABERTOS
;; 2. LENGTH LISTA ABERTOS = 0 -> NIL
;; 3. N = PRIMEIRO NÓ DE LISTA ABERTOS (REMOVENDO)
;; 4. N -> ADICIONAR EM LISTA FECHADOS
;; 5. EXPANDIR N -> SUCESSORES PARA LISTA ABERTOS
;; 6. SE SUCESSOR = OBJETIO, ACABAR SE NÃO, IR PARA 2.

(defun bf-sucessores-objetivo (lista-sucessores numero-caixas-fechadas) 
    "Verifica na lista de sucessores se existe um com o número de caixas fechadas necessárias"

    (cond
        (
             (= (list-length lista-sucessores) 0)
             nil
        ) 
        (
            (= (contar-caixas-fechadas (caar lista-sucessores)) numero-caixas-fechadas) 
            T
        )
        (
            T 
            (bf-sucessores-objetivo (cdr lista-sucessores) numero-caixas-fechadas)
        )
    )
)

;; (algoritmo-bf (no-teste) 3)
(defun algoritmo-bf (estado-inicial numero-caixas-fechadas &optional (lista-abertos ()) (lista-fechados ()))

    (cond 
        ((= (list-length lista-abertos) 0)
            
           (let ((abertos (cons estado-inicial ())))
         
                (cond 
                    ((= (list-length estado-inicial) 0) nil)
            
                    (T 

                        (cond 

                            ((= (list-length lista-fechados) 0)

                                (let ((fechados ()))
                            
                                    ;; adicionar aos fechados (first abertos)
                                    (append fechados (first abertos))

                                    ;; expandir primeiro nó dos abertos
                                    ;; verificar se está na lista de sucessores "estado-objetivo" (numero de caixas fechadas está good?)

                                    (cond 
                                        ( 
                                         (equal 
                                            (bf-sucessores-objetivo (sucessores-bfs (first abertos) (operadores)) numero-caixas-fechadas)
                                            T
                                            ) 
                                            ;; terminar ? (não sei o que acontece aqui)
                                            (list 'success)
                                        )  
                                        (
                                            T 
                                            (algoritmo-bf estado-inicial
                                                          numero-caixas-fechadas
                                                          (append abertos (sucessores-bfs (first abertos) (operadores)))
                                                          fechados
                                            )
                                        )
                                    )                        
                                )
                            )
                        
                            (T
                            
                                (let ((fechados lista-fechados))
                            
                                    ;; adicionar aos fechados (first abertos)
                                    (append fechados (first abertos))

                                    ;; expandir primeiro nó dos abertos
                                    ;; verificar se está na lista de sucessores "estado-objetivo" (numero de caixas fechadas está good?)

                                    (cond 
                                        ( 
                                         (equal 
                                            (bf-sucessores-objetivo (sucessores-bfs (first abertos) (operadores)) numero-caixas-fechadas)
                                            T
                                            ) 
                                            ;; terminar ? (não sei o que acontece aqui)
                                            (list 'success)
                                        )  
                                        (
                                            T 
                                            (algoritmo-bf estado-inicial
                                                          numero-caixas-fechadas
                                                          (append abertos (sucessores-bfs (first abertos) (operadores)))
                                                          fechados
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )           
        )

        (T

        (let ((abertos lista-abertos))
         
                (cond 
                    ((= (list-length estado-inicial) 0) nil)
            
                    (T 

                        (cond 

                            ((= (list-length lista-fechados) 0)

                                (let ((fechados (cons (first abertos) ())))
                            
                                    ;; adicionar aos fechados (first abertos)
                                    (append fechados (first abertos))

                                    ;; expandir primeiro nó dos abertos
                                    ;; verificar se está na lista de sucessores "estado-objetivo" (numero de caixas fechadas está good?)

                                    (cond 
                                        ( 
                                         (equal 
                                            (bf-sucessores-objetivo (sucessores-bfs (first abertos) (operadores)) numero-caixas-fechadas)
                                            T
                                            ) 
                                            ;; terminar ? (não sei o que acontece aqui)
                                            (list 'success)
                                        )  
                                        (
                                            T 
                                            (algoritmo-bf estado-inicial
                                                          numero-caixas-fechadas
                                                          (append abertos (sucessores-bfs (first abertos) (operadores)))
                                                          fechados
                                            )
                                        )
                                    )
                                                        
                                )
                            )
                        
                            (T                            
                                (let ((fechados lista-fechados))
                            
                                    ;; adicionar aos fechados (first abertos)
                                    (append fechados (first abertos))

                                    ;; expandir primeiro nó dos abertos
                                    ;; verificar se está na lista de sucessores "estado-objetivo" (numero de caixas fechadas está good?)

                                    (cond 
                                        ( 
                                         (equal 
                                            (bf-sucessores-objetivo (sucessores-bfs (first abertos) (operadores)) numero-caixas-fechadas)
                                            T
                                            ) 
                                            ;; terminar ? (não sei o que acontece aqui)
                                            (list 'success)
                                        )  
                                        (
                                            T 
                                            (algoritmo-bf estado-inicial
                                                          numero-caixas-fechadas
                                                          (append abertos (sucessores-bfs (first abertos) (operadores)))
                                                          fechados
                                            )
                                        )
                                    )                               
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)


