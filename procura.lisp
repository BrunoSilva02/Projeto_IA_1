;; Algoritmos de Procura
;; 
;; Aurélio Miranda - 202000572
;; Bruno Silva - 202002258


;;*********************************** variaveis de teste e operadores ********************************************************************

(defun no-teste ()
"Define um no teste do problema"
 (cria-no (tabuleiro-teste)))

(defun operadores ()
 "Cria uma lista com todos os operadores do problema."
 (list 'arco-vertical 'arco-horizontal))

;;; Construtor
(defun cria-no (tabuleiro &optional (g 0) (h 99) (pai nil) (o 10))
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
  (cond ((eq (no-heuristica no) (no-objetivo no)) T)
   (t NIL))
)

;;; função que calcula a heuristica de um nó
;; teste: (heuristica (no-teste))
;; resultado: 9
(defun heuristica (no)
  (cond ((and
    	(no-objetivo no) (contar-caixas-fechadas (no-estado no)))
      (- (no-objetivo no) (contar-caixas-fechadas (no-estado no))))
      (t (no-heuristica no))
  )
)

;;; Funcao auxiliar da geração de nos
;;; gera um novo nó
;; teste: (novo-sucessor (no-teste) 1 1 (car (operadores)))
;; resultado: ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((1 0 0) (0 1 1) (1 0 1) (0 1 1)) 1 9 --------------- nesta linha mostra "..(0 0 1)) (1 0 0).."
;;            ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 1) (1 0 1) (0 1 1))) 0 99 NIL 10) 1)--- na consola, ESPERO ser bug visual
(defun novo-sucessor(no l i func)
  (cond
    ((null (funcall func l i (no-estado no))) NIL)
    (t (cria-no (funcall func l i (no-estado no)) (+ (no-profundidade no) 1) 
        (- (no-objetivo no) (contar-caixas-fechadas (no-estado no))) no (contar-caixas-fechadas (no-estado no)))
    )
  )
)

;;; Funcao geradora de nos
;;; gera todos os nos filho
;; teste: (sucessores-bfs (no-teste) (operadores))
;; resultado: ??
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
;; teste: (sucessores-dfs (no-teste) (operadores))
;; resultado: ??
(defun sucessores-dfs (no funcs prof)
  (cond
    ((= (get-profundidade no) maxProf) NIL)
    (T (remove nil 
    (append-list 
      (mapcar (lambda (funcao)
        (mapcar (lambda (coordenada)
          (novo-sucessor no (first coordenada)(second coordenada) funcao))
          (coordenadas-posiveis (no-estado no) funcao)
        ))funcs)
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
(defun no-unicos (sucessores fechados)
   "Devolve apenas nos que sejam diferentes aos nos em fechados"
   (cond
       ((null sucessores) NIL)
       ((null fechados) sucessores)
       ((no-existep (car sucessores) fechados) (no-unicos (cdr sucessores) fechados))
       (T (cons (car sucessores) (no-unicos (cdr sucessores) fechados)))
   )
)

;; Devolve um no da lista caso seja o no objetivo
;; teste: (desenvolver teste)
;; resultado: -
(defun no-obj (list funObj)
  "Devolve NIL ou o no objetivo"
  (cond
      ((null list) NIL)
      ((funcall funObj (car list)) (car list))
      (T (no-obj (cdr list)funObj))
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
;; procura na largura
;; (trace bfs)
;; teste: (bfs (no-teste) 'no-solucaop 'sucessores-bfs (operadores) nil nil)
;; resultado: ! acabar sucessores primeiro ! (para testar)
(defun bfs(no funObj funSuss operadores &optional abertos fechados)
  "Define a função bfs que irá efetuar a procura em largura-primeiro"
   (cond
       ((and(null abertos)(null fechados)) (bfs no funObj funSuss operadores (list no) fechados))
       ((null abertos) nil)
       ((funcall funObj (car abertos)) (car abertos))
       (T 
           (let ((next-nos (no-unicos (funcall funSuss (car abertos) operadores) fechados)))
             (bfs no funObj funSuss operadores (abertos-bfs (cdr abertos) next-nos) (append fechados (list (car abertos))))
           )
        )
    )
)


;; procura na profundidade
;; teste: (dfs (no-teste) 'no-solucaop 'sucessores (operadores) 10)
;; resultado: ! acabar sucessores primeiro ! (incompleto)
(defun dfs(no funObj funSuss operadores maxProf &optional abertos fechados)
    "Defina a função dfs que irá efetuar a procura em profundidade-primeiro"
   (cond
       ((and(null abertos)(null fechados)) (bfs no funObj funSuss operadores (list no) fechados))
       ((funcall funObj no) no)
       ((null abertos)NIL)
       (T
          (let ((next-nos  (no-unicos (funcall funSuss (car abertos) operadores 'dfs maxProf) fechados)))
              (cond 
                   ((no-obj next-nos funObj) (no-obj next-nos funObj))
                   (T (dfs no funObj funSuss operadores maxProf (abertos-dfs (cdr abertos) next-nos) (append fechados (list (car abertos)))))
               )
          )
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