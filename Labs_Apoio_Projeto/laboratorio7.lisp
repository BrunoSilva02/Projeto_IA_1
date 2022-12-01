;;;; laboratorio7.lisp
;;;; Ficha de Laborat�rio n�7 - Apoio ao 1� projeto
;;;; Autor: 


;;; Tabuleiro

(defun tabuleiro-teste ()
  "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"
	'(
		((0 0 0) (0 0 1) (0 1 1) (0 0 1))
		((0 0 0) (0 1 1) (1 0 1) (0 1 1))
	)
)

;;; Exercicios

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
		  ((= indice 2) (cons (car lista) (cons valor (cddr lista))))
		  ((= indice 3) (cons (car lista) (cons (second lista)  (cons valor (cdddr lista)))))
		  (T(cons (car lista) (cons (second lista) (cons (third lista) (cons valor ()))))))
)

(defun arco-na-posicao (lista-arcos posicao lista &optional (X 1))
	"Coloca o um valor X no arco escolhido"
	(substituir lista-arcos lista 
			(substituir posicao (nth (- lista-arcos 1) lista) X))
)