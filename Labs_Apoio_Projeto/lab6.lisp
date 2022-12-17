;;;; laboratorio6.lisp
;;;; Ficha de Laboratório nº6 - O Problema das Vasilhas de Água
;;;; Autor: 

;;; Inicialização do programa
;; iniciar
(defun iniciar ()
"Permite iniciar o programa, fazendo a leitura do teclado do estado inicial e do algoritmo a utilizar para procurar a solução (neste caso a procura na profundidade ou na largura)"
  (let* ((no (cria-no (ler-vasilhas)))
         (algoritmo (ler-algoritmo))
         (profundidade (cond ((eql algoritmo 'dfs) (ler-profundidade)) (T 9999))) )
	(cond
		((equal algoritmo 'bfs) (escreve-no (funcall algoritmo no 'no-solucaop 'sucessores (operadores))))
		((equal algoritmo 'dfs) (escreve-no (funcall algoritmo no 'no-solucaop 'sucessores (operadores) profundidade)))
	)
  )
)

;;; Input - interface que permite ler os valores iniciais das vasilhas junto do utilizador.
(defun ler-no-inicial (&optional (f t))
  (read f))

(defun ler-vasilhas ()
"Permite ler do teclado o estado inicial do problema das vasilhas."
  (let ((vasilha-a (ler-vasilha "A")) (vasilha-b (ler-vasilha "B")))
    (list vasilha-a vasilha-b)
    )
)

(defun ler-vasilha (vasilha)
"Permite ler do teclado o valor inicial de uma vasilha.
A função verifica que os valores lidos pertencem ao intervale esperado para cada vasilha."
(progn
    (format t "Insere o valor da vasilha ~A ~%" vasilha)
    (let ((valor (read)))
      (cond
        ((AND (equal vasilha "A") (OR (> valor 3) (< valor 0))) (progn (format t "Valor invalido ~%") (ler-vasilha vasilha)))
        ((AND (equal vasilha "B") (OR (> valor 5) (< valor 0))) (progn (format t "Valor invalido ~%") (ler-vasilha vasilha)))
        (T valor)
      )
  )
))

;; ler-algoritmo
(defun ler-algoritmo ()
"Permite fazer a leitura do algoritmo a utilizar."
  (progn
    (format t "Que algoritmo quer usar para procurar? ~%")
    (format t "1- Procura na largura ~%")
    (format t "2- Procura na profundidade ~%")
    (let ((resposta (read)))
      (cond ((= resposta 1) 'bfs)
            (T 'dfs)))
    )
  )
;; ler-profundidade
(defun ler-profundidade()
"Permite fazer a leitura da profundidade limite para o algoritmo dfs."
    (progn
    (format t "Qual a profundidade limite? ~%")
    (read)
    ))


;;; Output - escrita do estado do problema
;;
(defun escrever-no (no &optional (g t))
"Permite escrever um no, por defeito no ecra."
  (format g "~A" no))

 
(defun escreve-no (no)
 "Permite escrever no ecra um no do problema."
  (progn
     (format t "| A: ~a | B: ~a | G: ~a |~%" (vasilha-a-conteudo no) (vasilha-b-conteudo no) (no-profundidade no))
     (format t "Pai: ~a ~%" (no-pai no))
  ))

(defun escreve-lista-nos (lista)
  "Permite escrever no ecra uma lista de nos do problema das vasilhas, e.g. um conjunto de sucessores, a lista de abertos etc."
  (cond
   ((null lista) nil)
   (T (progn (escreve-no (car lista)) (escreve-lista-nos (cdr lista))))))


;;; Problema das vasilhas
;;; variaveis de teste e operadores
(defun no-teste ()
"Define um no teste do problema da vasilhas em que A=2, B=2, profundidade=0 e pai=NIL"
 (list '(2 2) 0 nil))

(defun operadores ()
 "Cria uma lista com todos os operadores do problema das vasilhas."
 (list 'vazar-a 'vazar-b 'encher-a 'encher-b 'transferir-a-b 'transferir-b-a))

;;; Construtor
(defun cria-no (vasilhas &optional (g 0) (pai nil))
  (list vasilhas g pai)
)


;;; Metodos seletores
;; no-estado
;; teste: (no-estado (no-teste))
;; resultado: (2 2)
(defun no-estado (no)
  (car no)
)

;; vasilha-a-conteudo
;; teste: (vasilha-a-conteudo (no-teste))
;; resultado: 2
(defun vasilha-a-conteudo (no)
 (caar no))


;; vasilha-b-conteudo
;; teste: (vasilha-b-conteudo (no-teste))
;; resultado: 2
(defun vasilha-b-conteudo (no)
  (cadar no)
)

;; no-profundidade
;; teste: (no-profundidade (no-teste))
;; resultado: 0
(defun no-profundidade (no)
  (cadr no)
)

;; no-pai
;; teste: (no-pai (no-teste))
;; resultado: NIL
(defun no-pai (no)
  (cddr no)
)



;;; Operadores do problema
;; transferir: que permite vazar o conteudo de uma vasilha para outra ou para fora.
;; encher: para encher uma vasilha ate o topo

;; teste: (vazar-a (no-estado (no-teste)))
;; resultado: (0 2)
(defun vazar-a (no)
  (if (= (vasilha-a-conteudo no) 0)
   NIL (list 0 (vasilha-b-conteudo no)))
)


;; teste: (vazar-b (no-estado (no-teste)))
;; resultado: (2 0)
(defun vazar-b (no)
  (if (= (vasilha-b-conteudo no) 0)
   NIL (list (vasilha-a-conteudo no) 0)))

;; teste: (encher-a (no-estado (no-teste)))
;; resultado: (3 2)
(defun encher-a (no)
  (if (= (vasilha-a-conteudo no) 3)
   NIL (list 3 (vasilha-b-conteudo no)))
)

;; teste: (encher-b (no-estado (no-teste)))
;; resultado: (2 5)
(defun encher-b (no)
  (if (= (vasilha-b-conteudo no) 5)
   NIL (list (vasilha-a-conteudo no) 5)))

;; teste: (transferir-a-b (no-estado (no-teste)))
;; resultado: (0 4)
(defun transferir-a-b (no)
  (if (> (+ (vasilha-a-conteudo no) (vasilha-b-conteudo no)) 5)
   (list (- 5 (+ (vasilha-a-conteudo no) (vasilha-b-conteudo no))) 5)
    (list 0 (+ (vasilha-a-conteudo no) (vasilha-b-conteudo no)))))

;; teste: (transferir-b-a (no-estado (no-teste)))
;; resultado: (3 1)
(defun transferir-b-a (no)
  (if (> (+ (vasilha-a-conteudo no) (vasilha-b-conteudo no)) 3)
   (list 3 (- (+ (vasilha-a-conteudo no) (vasilha-b-conteudo no)) 3))
    (list (+ (vasilha-a-conteudo no) (vasilha-b-conteudo no)) 0)))

;;; Funcoes auxiliares da procura
;;; predicado no-solucaop que verifica se um estado e final
;; teste: (no-solucaop (no-teste))
;; resultado: NIL
(defun no-solucaop (no)
  (cond ((= (vasilha-a-conteudo no) 1) T)
        ((= (vasilha-b-conteudo no) 1) T)
   (t NIL))
)


;;; sucessores
;; teste: (novo-sucessor (no-teste) 'encher-a)
;; resultado: ((3 2) 1 ((2 2) 0 NIL))
;; teste: (novo-sucessor (no-teste) 'transferir-a-b)
;; resultado: ((0 4) 1 ((2 2) 0 NIL))
;; teste: (novo-sucessor (cria-no '(3 5)) 'encher-a)
;; resultado: NIL
(defun novo-sucessor(no func)
  (cond
       ((equal (funcall func (no-estado no)) (no-estado no))   NIL)
       (T (list (funcall func (no-estado no)) (+ 1 (no-profundidade no))  no))
   )
)


;; teste: (sucessores (no-teste) (operadores) 'bfs)
;; resultado: (((0 2) 1 ((2 2) 0 NIL)) ((2 0) 1 ((2 2) 0 NIL)) ((3 2) 1 ((2 2) 0 NIL)) ((2 5) 1 ((2 2) 0 NIL)) ((0 4) 1 ((2 2) 0 NIL)) ((3 1) 1 ((2 2) 0 NIL)))
;; teste: (sucessores (no-teste) (operadores) 'dfs 2)
;; resultado: (((0 2) 1 ((2 2) 0 NIL)) ((2 0) 1 ((2 2) 0 NIL)) ((3 2) 1 ((2 2) 0 NIL)) ((2 5) 1 ((2 2) 0 NIL)) ((0 4) 1 ((2 2) 0 NIL)) ((3 1) 1 ((2 2) 0 NIL)))
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