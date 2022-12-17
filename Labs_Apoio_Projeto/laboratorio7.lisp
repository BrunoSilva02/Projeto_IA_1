;;;; laboratorio7.lisp
;;;; Ficha de Laboratorio nº7 - Apoio ao 1º projeto
;;;; Autor: Aurélio Miranda (202000572 | SW-03) & Bruno Silva (202002258 | SW-03)


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
		(cond ((< posicao 0) nil)
			  ((> posicao (length lista)) nil))
			  (T (nth (- posicao 1) (nth (- lista-arcos 1) lista)))
)

(defun substituir (indice lista &optional (valor 1))
	"Substitui numa lista um valor, na posição indica pelo indice"
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

(defun arco-horizontal (lista-arcos posicao tabuleiro &optional (X 1))
	"Adiciona um arco horizontal na posição pretendida, caso já esteja lá um arco retorna nil"
	(if (or 
			(> lista-arcos (length (car tabuleiro))) 
			(> posicao (length (car tabuleiro)))
		)
		nil

		(let ((lista 
				(arco-na-posicao lista-arcos posicao (get-arcos-horizontais tabuleiro) X)
				))

			(if 
				(equal lista (get-arcos-horizontais tabuleiro))
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
	"Adiciona um arco vertical na posição pretendida, caso já esteja lá um arco retorna nil"
	(if (or 
			(> lista-arcos (length (car tabuleiro))) 
			(> posicao (length (car tabuleiro)))
		)
		nil

		(let ((lista 
				(arco-na-posicao lista-arcos posicao (get-arcos-verticais tabuleiro) X)
				))

			(if 
				(equal lista (get-arcos-verticais tabuleiro))
				nil

				(cons 
					(get-arcos-horizontais tabuleiro)
					lista 
				)
			)
		)
	)
)