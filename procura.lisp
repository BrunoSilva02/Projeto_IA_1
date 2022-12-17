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