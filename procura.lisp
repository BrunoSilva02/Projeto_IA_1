;; Algoritmos de Procura
;; 
;; Aurélio Miranda - 202000572
;; Bruno Silva - 202002258


;;; Algoritmos
;; procura na largura
;; teste: (bfs (no-teste) 'no-solucaop 'sucessores (operadores) nil nil)
;; resultado: ((3 1) 1 ((2 2) 0 NIL))
(defun bfs(no funObj funSuss operadores &optional abertos fechados)
   (cond
       ((and(null abertos)(null fechados)) (bfs no funObj funSuss operadores (list no) fechados))
       ((funcall funObj no) no)
       ((null abertos)NIL)
       (T
          (let ((next-nos  (no-unicos (funcall funSuss (car abertos) operadores 'bfs) fechados)))
               (cond 
                   ((no-obj next-nos funObj) (no-obj next-nos funObj))
                   (T (bfs no funObj funSuss operadores (abertos-bfs (cdr abertos) next-nos) (append fechados (list (car abertos)))))
               )
          )
        )
    )
)

;; procura na profundidade
;; teste: (dfs (no-teste) 'no-solucaop 'sucessores (operadores) 10)
;; resultado: ((3 1) 1 ((2 2) 0 NIL))
(defun dfs(no funObj funSuss operadores maxProf &optional abertos fechados)
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

(defun novo-sucessor(no func)
  (cond
       ((equal (funcall func (no-estado no)) (no-estado no))   NIL)
       (T (list (funcall func (no-estado no)) (+ 1 (no-profundidade no))  no))
   )
)

(defun sucessores(no funcs alg &optional maxProf)
  (cond
     ((and (equal alg 'dfs)(equal (no-profundidade no) maxProf)) NIL)
     (T 
      (remove nil (mapcar  (lambda (fun) (novo-sucessor no fun)) funcs))
      )
   )
)

(defun abertos-bfs (abertos sucessores)
 ;;Ordena abertos para o algoritmo bfs
  (cond
     ((null abertos)sucessores)
     (T (append abertos sucessores))
   )
)

(defun abertos-dfs (abertos sucessores)
 ;;Ordena abertos para o algoritmo dfs
   (cond
     ((null abertos)sucessores)
     (T (append sucessores abertos))
   )
)

(defun no-existep(no list)
;;se existe um no lista igual ao no recebido
    (cond
         ((null list) NIL)
         ((equal (no-estado no) (no-estado (car list))) T)
         (T (no-existep no (cdr list)))
     )
)

(defun no-unicos (sucessores fechados)
;;devolve apenas nos que sejam diferentes aos nos em fechados
   (cond
       ((null sucessores) NIL)
       ((null fechados) sucessores)
       ((no-existep (car sucessores) fechados) (no-unicos (cdr sucessores) fechados))
       (T (cons (car sucessores) (no-unicos (cdr sucessores) fechados)))
   )
)

(defun no-obj (list funObj)
;; devolve NIL ou o no objetivo
  (cond
      ((null list) NIL)
      ((funcall funObj (car list)) (car list))
      (T (no-obj (cdr list)funObj))
   )
)

;;*********************************** variaveis de teste e operadores ********************************************************************

(defun no-teste ()
"Define um no teste do problema"
 (cria-no (tabuleiro-teste)))

(defun operadores ()
 "Cria uma lista com todos os operadores do problema."
 (list 'arco-vertical 'arco-horizontal))

;;; Construtor
(defun cria-no (tabuleiro &optional (g 0) (h 99) (pai nil))
  (list tabuleiro g h pai)
)

;;; Metodos seletores
;; no-estado
;; teste: (no-estado (no-teste))
;; resultado: (((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 1) (1 0 1) (0 1 1)))
(defun no-estado (no)
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
  (cdddr no)
)

;;; Funcoes auxiliares da procura
;;; predicado no-solucaop que verifica se um estado e final
;; teste: (no-solucaop (no-teste))
;; resultado: NIL
(defun no-solucaop (no)
  (cond ((= (no-heuristica no) 0) T)
   (t NIL))
)

;;(contar-caixas-fechadas (tabuleiro-problema-a))

(defun contar-caixas-fechadas (tabuleiro &optional (l 1) (i 1))
    (cond 
      ((< (list-length (get-arcos-verticais tabuleiro)) l) 0)
      ((< (list-length (car (get-arcos-verticais tabuleiro))) i) 0)
      

      ((and
                
        (and
          (= (get-arco-na-posicao l i (get-arcos-verticais tabuleiro)) 1) 
          (= (get-arco-na-posicao (+ l 1) i (get-arcos-verticais tabuleiro)) 1)
        )

        (and 
          (cond 
            ((< (list-length (get-arcos-horizontais tabuleiro)) l) nil)
            ((< (list-length (car (get-arcos-horizontais tabuleiro))) i) nil)

            ((and
              (= (get-arco-na-posicao i l (get-arcos-horizontais tabuleiro)) 1)
              (= (get-arco-na-posicao (+ i 1) l (get-arcos-horizontais tabuleiro) 1))
            ) t)

            (T nil)
          )
        )

      ) (+ (contar-caixas-fechadas tabuleiro (+ l 1) i) (contar-caixas-fechadas tabuleiro l (+ i 1)) 1))
                
      (t (+ (contar-caixas-fechadas tabuleiro (+ l 1) i)(contar-caixas-fechadas tabuleiro l (+ i 1))0)))
)