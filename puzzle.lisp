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

(defun tabuleiro-problema-b ()
  "Retorna um tabuleiro 4x4 (4 arcos na vertical por 4 arcos na horizontal)"
	'(
		((0 0 1 0) (1 1 1 1) (0 0 1 1) (0 0 1 1) (0 0 1 1))
		((0 0 1 1) (0 0 1 1) (1 1 1 1) (1 0 1 1) (0 1 1 1))
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
					(get-arcos-verticais tabuleiro)
				)
			)
		)
	)

)

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
					lista 
				)
			)
		)
	)
)
;;(trace contar-caixas-fechadas)
;;(contar-caixas-fechadas (tabuleiro-problema-a)) 
#|
(defun contar-caixas-fechadas (tabuleiro &optional (l 1) (i 1))
	(cond ((< (list-length (car (get-arcos-horizontais tabuleiro))) i) 0)
		((< (list-length (get-arcos-horizontais tabuleiro)) l) 0)
		((and
			(and
				(not (null (get-arco-na-posicao l i (get-arcos-verticais tabuleiro))))
				(not (null (get-arco-na-posicao (+ l 1) i (get-arcos-verticais tabuleiro))))
				(and 
					(eq (get-arco-na-posicao l i (get-arcos-verticais tabuleiro)) '1)
					(eq (get-arco-na-posicao (+ l 1) i (get-arcos-verticais tabuleiro)) '1)
				)
			)
			(cond ((< (list-length (car (get-arcos-horizontais tabuleiro))) l) 0)
				((< (list-length (get-arcos-horizontais tabuleiro)) i) 0)
				((and
					(not (null (get-arco-na-posicao l i (get-arcos-verticais tabuleiro))))
					(not (null (get-arco-na-posicao (+ l 1) i (get-arcos-verticais tabuleiro))))
					(and 
						(eq (get-arco-na-posicao i l (get-arcos-horizontais tabuleiro)) '1)
						(eq (get-arco-na-posicao (+ i 1) l (get-arcos-horizontais tabuleiro)) '1)
					)
				)
				)
			)
		)
		(+ (contar-caixas-fechadas tabuleiro (+ l 1) i)(contar-caixas-fechadas tabuleiro l (+ i 1)) 1))
		(t (+ (contar-caixas-fechadas tabuleiro (+ l 1) i)(contar-caixas-fechadas tabuleiro l (+ i 1)) 0))
	)
)
|#

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

;;cria-no (tabuleiro &optional (g 0) (h 0) (pai nil))
(defun iniciar ()
  (let* ((no (cria-no (tabuleiro-problema-a)))
         (algoritmo (ler-algoritmo))
         (profundidade (cond ((eql algoritmo 'dfs) (ler-profundidade)) (T 9999))) )
	(cond
		((equal algoritmo 'bfs) (escreve-no (funcall algoritmo no 'no-solucaop 'sucessores (operadores))))
		((equal algoritmo 'dfs) (escreve-no (funcall algoritmo no 'no-solucaop 'sucessores (operadores) profundidade)))
	)
  )
)


