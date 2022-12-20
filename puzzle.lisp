;; Código relacionado ao problema
;; 
;; Aurélio Miranda - 202000572
;; Bruno Silva - 202002258

;;; Tabuleiro

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

;;; Funções de manipulação

(defun get-arcos-horizontais (tabuleiro)
	"Devolve a lista dos arcos horizontais do tabuleiro"
	(first tabuleiro)	
)

(defun get-arcos-verticais (tabuleiro)
	"Devolve a lista dos arcos verticais do tabuleiro"
	(second tabuleiro)
)

(defun get-arco-na-posicao (lista-arcos posicao lista)
	"Devolve o arco existente no tabuleiro na lista de arcos horizontais ou verticais"
		(nth (- posicao 1) (nth (- lista-arcos 1) lista))
)

(defun substituir (indice lista &optional (valor 1))
	(cond ((= indice 1) (cons valor (cdr lista)))
		(t (cons (car lista) (substituir (- indice 1) (cdr lista) valor))))
)

(defun arco-na-posicao (lista-arcos posicao lista &optional (X 1))
	"Coloca o um valor X no arco escolhido"
	(substituir lista-arcos lista 
			(substituir posicao (nth (- lista-arcos 1) lista) X))
)

; (arco-horizontal 1 1 (no-estado (no-teste)))
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

; (arco-vertical 1 1 (no-estado (no-teste)))
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


;;(trace contar-caixas-fechadas)
;;(contar-caixas-fechadas (tabuleiro-problema-a)) 
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
  (voltar)
)


;;(resolver 'bfs (no-teste-a))
(defun resolver (algoritmo tabuleiro)
	"Resolve um tabuleiro escolhido com o algoritmo designado"
	(let ((inicio (get-universal-time)))
    (cond ((equal algoritmo 'dfs) (escreve-no (dfs tabuleiro 'no-solucaop 'sucessores-dfs (operadores) (ler-profundidade))
			 (- (get-universal-time) inicio))) 
		((equal algoritmo 'bfs) (escreve-no (bfs tabuleiro 'no-solucaop 'sucessores-bfs (operadores)) (- (get-universal-time) inicio))))
	 )
)


#|
(tabuleiro &optional (l 1) (i 1) melhor-pos val)

if (l i = 1) {aumenta valor} 

if (l+1 i = 1) {aumenta valor}

if (i l = 1) {aumenta valor}

if (i+1 l = 1) {aumenta valor}

if (valor > val) {(tabuleiro &optional (l 1) (i 1) tabuleiro val)}
|#