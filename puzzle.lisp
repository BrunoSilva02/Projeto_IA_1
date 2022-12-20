;; Código relacionado ao problema
;; 
;; Aurélio Miranda - 202000572
;; Bruno Silva - 202002258

;;; Tabuleiro

;; ------------------------------------------------- Tabuleiros problema -----------------------------------------------------------------

(defun tabuleiro-teste ()
  "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"
	'(
		((0 0 0) (0 0 1) (0 1 1) (0 0 1))
		((0 0 0) (0 1 1) (1 0 1) (0 1 1))
	)
)

(defun tabuleiro-problema-a ()
  "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"
	'(
		((0 0 0) (0 0 1) (0 1 1) (0 0 1))
		((0 0 0) (0 1 0) (0 0 1) (0 1 1))
	)
)

(defun tabuleiro-problema-a-resolvido ()
  "Retorna o tabuleiro a resolvido para fins de teste"
	'(((0 0 0) (0 1 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 1 1) (0 1 1)))
)

(defun tabuleiro-problema-b ()
  "Retorna um tabuleiro 4x4 (4 arcos na vertical por 4 arcos na horizontal)"
	'(
		((0 0 1 0) (1 1 1 1) (0 0 1 1) (0 0 1 1) (0 0 1 1))
		((0 0 1 1) (0 0 1 1) (1 1 1 1) (1 0 1 1) (0 1 1 1))
	)
)

(defun tabuleiro-problema-c ()
  "Retorna um tabuleiro 5x5 (5 arcos na vertical por 5 arcos na horizontal)"
	'(
		((0 0 1 0) (1 0 1 1) (0 0 1 1) (0 0 1 1) (0 0 1 1))
		((0 0 1 1) (0 0 1 1) (0 0 1 1) (1 0 1 1) (0 1 1 1))
	)
)

(defun tabuleiro-problema-d ()
  "Retorna um tabuleiro 5x5 (5 arcos na vertical por 5 arcos na horizontal)"
	'(
		((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))
		((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))
	)
)


(defun tabuleiro-problema-e ()
  "Retorna um tabuleiro 5x5 (5 arcos na vertical por 5 arcos na horizontal)"
	'(
		((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))
		((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))
	)
)


(defun tabuleiro-problema-f ()
  "Retorna um tabuleiro 5x5 (5 arcos na vertical por 5 arcos na horizontal)"
	'(
		((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))
		((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))
	)
)

;; -------------------------------------------------------- Funções de manipulação ----------------------------------------------------------

;; teste: (get-arcos-horizontais (tabuleiro-problema-a))
;; resultado: ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
(defun get-arcos-horizontais (tabuleiro)
	"Devolve a lista dos arcos horizontais do tabuleiro"
	(first tabuleiro)	
)

;; teste: (get-arcos-verticais (tabuleiro-problema-a))
;; resultado: ((0 0 0) (0 1 0) (0 0 1) (0 1 1))
(defun get-arcos-verticais (tabuleiro)
	"Devolve a lista dos arcos verticais do tabuleiro"
	(second tabuleiro)
)

;; teste: (get-arco-na-posicao 2 3 (tabuleiro-problema-a))
;; resultado: (0 0 1)
(defun get-arco-na-posicao (lista-arcos posicao lista)
	"Devolve o arco existente no tabuleiro na lista de arcos horizontais ou verticais"
		(nth (- posicao 1) (nth (- lista-arcos 1) lista))
)

;; teste: (substituir 1 (car (get-arcos-horizontais (tabuleiro-problema-a))))
;; resultado: (1 0 0)
(defun substituir (indice lista &optional (valor 1))
	(cond ((= indice 1) (cons valor (cdr lista)))
		(t (cons (car lista) (substituir (- indice 1) (cdr lista) valor))))
)

;; teste: (arco-na-posicao 2 2 (get-arcos-horizontais (tabuleiro-teste)))
;; resultado: ((0 0 0) (0 1 1) (0 1 1) (0 0 1))
(defun arco-na-posicao (lista-arcos posicao lista &optional (X 1))
	"Coloca o um valor X no arco escolhido"
	(substituir lista-arcos lista 
			(substituir posicao (nth (- lista-arcos 1) lista) X))
)

;; teste: (arco-horizontal 1 1 (no-estado (no-teste-a)))
;; resultado: (((1 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1)))
(defun arco-horizontal (lista-arcos posicao tabuleiro &optional (X 1))

	(if (or 
			(> lista-arcos (length (car tabuleiro))) 
			(> posicao (length (car tabuleiro)))
		)
		nil

		(let ((lista (arco-na-posicao lista-arcos posicao (get-arcos-horizontais tabuleiro) X)))

			(if (equal lista (get-arcos-horizontais tabuleiro))
				nil
				(cons 
					lista 
					(cons
					(get-arcos-verticais tabuleiro) ())
				)
			)
		)
	)

)

;; teste: (arco-vertical 1 1 (no-estado (no-teste-a)))
;; resultado: (((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((1 0 0) (0 1 0) (0 0 1) (0 1 1)))
(defun arco-vertical (lista-arcos posicao tabuleiro &optional (X 1))

	(if (or 
			(> lista-arcos (length (car tabuleiro))) 
			(> posicao (length (car tabuleiro)))
		)
		nil

		(let ((lista (arco-na-posicao lista-arcos posicao (get-arcos-verticais tabuleiro) X)))

			(if (equal lista (get-arcos-verticais tabuleiro))
				nil
				(cons 
					(get-arcos-horizontais tabuleiro)
					(cons lista ())
				)
			)
		)
	)
)


;;----------------------------------- variaveis de teste e operadores ------------------------------------------------------------------------

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

;;; Construtor (tabuleiro, profindidade, heuristica, pai e objetivo do tabuleiro)
;; Objetivo -> número de caixas a fechar no tabuleiro
(defun cria-no (tabuleiro &optional (g 0) (h 99) (pai nil) (o 3))
  "Cria um no representante do estado do problema"
  (list tabuleiro g h pai o)
)

;;; Metodos seletores
;; no-estado
;; teste: (no-estado (no-teste-a))
;; resultado: (((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 1) (1 0 1) (0 1 1)))
(defun no-estado (no)
  "Retorna o estado"
  (car no)
)


;; no-profundidade
;; teste: (no-profundidade (no-teste-a))
;; resultado: 0
(defun no-profundidade (no)
  "Retorna a profundidade de um no"
  (cadr no)
)

;; no-heuristica
;; teste: (no-heuristica (no-teste-a))
;; resultado: 99
(defun no-heuristica (no)
  "Retorna a heuristica de um no"
  (caddr no)
)

;; no-pai
;; teste: (no-pai (no-teste-a))
;; resultado: NIL
(defun no-pai (no)
  "Retorna o pai de um no"
  (cadddr no)
)

;; no-objetivo
;; teste: (no-objetivo (no-teste-a))
;; resultado: NIL
(defun no-objetivo (no)
  "Retorna o objetivo (numero de caixas fechadas) de um no"
  (car(cddddr no))
)


;; --------------------------------------------------------- Funções Auxiliares--------------------------------------------------------------


;; (trace contar-caixas-fechadas)
;; teste: (contar-caixas-fechadas (tabuleiro-problema-a)) 
;; resultado: 1
(defun contar-caixas-fechadas (tabuleiro &optional (l 1) (i 1))
	(cond
		((<= (length (get-arcos-horizontais tabuleiro)) l) 0)
		((<= (length (get-arcos-verticais tabuleiro)) i) (contar-caixas-fechadas tabuleiro (+ l 1) 1))
		((and (eq (get-arco-na-posicao l i (get-arcos-horizontais tabuleiro)) '1)
			(eq (get-arco-na-posicao (+ l 1) i (get-arcos-horizontais tabuleiro)) '1)
			(eq (get-arco-na-posicao i l (get-arcos-verticais tabuleiro)) '1)
			(eq (get-arco-na-posicao (+ i 1) l (get-arcos-verticais tabuleiro)) '1)
		) (+ (contar-caixas-fechadas tabuleiro l (+ i 1)) 1)
		)
		(t (contar-caixas-fechadas tabuleiro l (+ i 1)))
	)
)


;; (escreve-no (bfs (no-teste-a) 'no-solucaop 'sucessores-bfs (operadores) nil nil) 2)
;; (escreve-no (dfs (no-teste-a) 'no-solucaop 'sucessores-dfs (operadores) 5 nil nil) 2)
(defun escreve-no (no tempo)
 "Permite escrever no ecra um no do problema."
  (progn
     (format t "~%~%| Estado: ~a ~%" (no-estado (car no)))
     (format t "| Profundidade: ~a ~%" (no-profundidade (car no)))
     (format t "| Nós gerados: ~a ~%" (+ (cadr no) (caddr no)))
     (format t "| Nós expandidos: ~a ~%" (caddr no))
	 (format t "| Penetrância: ~6f ~%" (/ (length (caminho (car no))) (+ (cadr no) (caddr no))))
	 (format t "| Tempo demorado: ~as ~%" tempo)
     (format t "| Pai: ~a ~%" (no-pai (car no)))
	 (format t "| Caminho: ~a ~%" (caminho (car no)))
  )
  (escrever-ficheiro (list
  	"| Estado: " (no-estado (car no))
  	"| Profundidade: " (no-profundidade (car no))
	"| Nós gerados: " (+ (cadr no) (caddr no))
	"| Nós expandidos: " (caddr no)
	"| Penetrância: " (/ (length (caminho (car no))) (+ (cadr no) (caddr no)))
	"| Tempo demorado: " tempo
	"| Pai: " (no-pai (car no))
	"| Caminho: " (caminho (car no))
  	))
  (voltar '1)
)


;;(resolver 'bfs (no-teste-a))
(defun resolver (algoritmo tabuleiro)
	"Resolve um tabuleiro escolhido com o algoritmo designado"
	(let ((inicio (get-universal-time)))
    	(cond ((equal algoritmo 'dfs) 
				(let ((profundidade (ler-profundidade)))
					(cond ((dfs tabuleiro 'no-solucaop 'sucessores-dfs (operadores) profundidade)
						(escreve-no (dfs tabuleiro 'no-solucaop 'sucessores-dfs (operadores) profundidade)
							(- (get-universal-time) inicio))) 
						(t (voltar '0))
					)))
			((equal algoritmo 'bfs) 
				(escreve-no (bfs tabuleiro 'no-solucaop 'sucessores-bfs (operadores)) (- (get-universal-time) inicio)))
		)
	)
)
