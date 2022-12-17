;; Interação com o utilizador e outros ficheiros de código
;; Escrita e leitura de ficheiros
;; 
;; Aurélio Miranda - 202000572
;; Bruno Silva - 202002258


(defun start ()
  (progn
    (format t "************************** ~%")
    (format t "*     Dots and Boxes     * ~%")
    (format t "*                        * ~%")
    (format t "*  Selecionar Tabuleiro  * ~%")
    (format t "*  Opt 2                 * ~%")
    (format t "*  Opt 3                 * ~%")
    (format t "* Escolha: ~%")
    (let ((resposta (read)))
      (cond ((eq resposta 1) (fn1))
        ((eq resposta 2) (fn2))
        ((eq resposta 'e) (fn-exit))
        (t (fn-default))
      )
    )
  )
)
    

(defun fn-default ()
  (progn
    (format t "Por favor insira uma opção válida. ~%"))
  (start))

(defun fn1 ()
  (progn
    (format t "wlecome to function 1. ~%")))

(defun fn2 ()
  (progn
    (format t "wlecome to function 2. ~%")))

(defun fn-exit ()
  (progn
    (format t "Adeus!")))


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