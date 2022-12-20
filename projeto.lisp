;; Interação com o utilizador e outros ficheiros de código
;; Escrita e leitura de ficheiros
;; 
;; Aurélio Miranda - 202000572
;; Bruno Silva - 202002258


;; Este método começa o programa
;; (start)
(defun start ()
  (progn
    (format t "******************************* ~%")
    (format t "*                             * ~%")
    (format t "*     Dots and Boxes          * ~%")
    (format t "*                             * ~%")
    (format t "*  Selecionar Tabuleiro (1)   * ~%")
    (format t "*  Ajuda (2)                  * ~%")
    (format t "*                             * ~%")
    (format t "******************************* ~%")
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
    (format t "******************************* ~%")
    (format t "*     Dots and Boxes          * ~%")
    (format t "*                             * ~%")
    (format t "*  Selecione um Tabuleiro     * ~%")
    (format t "*                             * ~%")
    (format t "*  Opt 1: A (3x3)             * ~%")
    (format t "*  Opt 2: B (4x4)             * ~%")
    (format t "*  Opt 3: C (5x5)             * ~%")
    (format t "*  Opt 4: D (5x5)             * ~%")
    (format t "*  Opt 5: E (6x6)             * ~%")
    (format t "*  Opt 6: F (7x7)             * ~%")
    (format t "*  Voltar                     * ~%")
    (format t "*                             * ~%")
    (format t "******************************* ~%")
    (format t "* Escolha: ~%")
    (let ((resposta (read)))
      (cond ((eq resposta 1) (fn2 'a))
        ((eq resposta 2) (fn2 'b))
        ((eq resposta 3) (fn2 'c))
        ((eq resposta 4) (fn2 'd))
        ((eq resposta 5) (fn2 'e))
        ((eq resposta 6) (fn2 'f))
        (t (start))
      )
    )
  ))

(defun fn2 (escolha)
  (progn
    (format t "Bem vindo ao tabuleiro ~s. ~%" escolha)))

(defun fn-exit ()
  (progn
    (format t "Adeus!")))


;; ler-algoritmo
(defun ler-algoritmo (tabuleiro)
"Permite fazer a leitura do algoritmo a utilizar."
  (progn
    (format t "Que algoritmo quer usar para procurar? ~%")
    (format t "1- Procura na largura ~%")
    (format t "2- Procura na profundidade ~%")
    (let ((resposta (read)))
      (cond ((= resposta 1) (iniciar 'bfs algoritmo))
            (T (iniciar 'dfs))))
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

(defun tempo-total (inicial final)
"Retorna o tempo total com o formato (h m s)"
  (list (- (car final) (car inicial))
    (- (cadr final) (cadr inicial))
    (- (caddr final) (caddr inicial)))
)

(defun teste ()
  (let ((temp)) (current-time)
    (sleep 1)
    (tempo-total temp (current-time))
  )
)