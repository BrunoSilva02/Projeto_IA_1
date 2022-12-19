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


; (ler-ficheiro "problemas.dat")
(defun ler-ficheiro (ficheiro)
(let ((in (open ficheiro :if-does-not-exist nil)))
  (let (problemas 'PROB)

  (when in
    (loop for line = (read-line in nil)
         while line do 
          (cond ((string-equal line ",") (format t ""))
          (T (append line problemas))
          )
    )
    (list problemas)   
    (close in))
    )
  )
)

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line))
)

; (print-ficheiro "problemas.dat")
(defun print-ficheiro (ficheiro)
  (get-file ficheiro)
)

(defun current-time()
"Retorna o tempo actual com o formato (h m s)"
  ;;HORAS-MINUTOS-SEGUNDOS
  (multiple-value-bind (s m h) (get-decoded-time)
    (list h m s)
   )
)