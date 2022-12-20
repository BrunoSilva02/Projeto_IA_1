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
      (cond ((eq resposta 1) (selecionar-tabuleiro))
        ((eq resposta 2) (funcao-ajuda))
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

(defun selecionar-tabuleiro ()
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
      (cond ((eq resposta 1) (ler-algoritmo (no-teste-a)))
        ((eq resposta 2) (ler-algoritmo (no-teste-b)))
        ((eq resposta 3) (ler-algoritmo (no-teste-c)))
        ((eq resposta 4) (ler-algoritmo (no-teste-d)))
        ((eq resposta 5) (ler-algoritmo (no-teste-e)))
        ((eq resposta 6) (ler-algoritmo (no-teste-f)))
        (t (start))
      )
    )
  ))

(defun fn-exit ()
  (progn
    (format t "Adeus!"))
)


;; -------------------------------------------------------- Funções de leitura ---------------------------------------------------------------

;; Define funções de leitura para interação com utilizador
;; teste: (ler-profundidade) User: 1
;; resultado: 1
;; teste: (ler-profundidade) User: a
;; resultado: 99
(defun ler-profundidade()
"Permite fazer a leitura da profundidade limite (dfs)."
    (progn
    (format t "Qual a profundidade limite? ~%")
    (let ((num (read)))
      (cond ((numberp num) num)
        (t 99)
      )
    )
    )
)

;; ler-algoritmo
(defun ler-algoritmo (escolha)
"Permite fazer a leitura do algoritmo a utilizar."
  (progn
    (format t "Que algoritmo quer usar para procurar? ~%~%")
    (format t "1- Procura na largura ~%")
    (format t "2- Procura na profundidade ~%")
    (format t "3- Procura informada a* ~%")
    (format t "Outra opção- Voltar ~%")
    (let ((resposta (read)))
      (cond ((= resposta 1) (resolver 'bfs escolha))
            ((= resposta 2) (resolver 'dfs escolha))
            ((= resposta 3) (ainda-nao-implementado 'ler-algoritmo escolha))
            (T (selecionar-tabuleiro))))
    )
)

(defun funcao-ajuda ()
  (progn
    (format t "              Dots and Boxes ~%")
    (format t "   Esta aplicação foi desenvolvida com o intuito de ~%")
    (format t "resolver tabuleiros (lxl) do jogo Dots and Boxes ~%")
    (format t "   O jogo consiste em colocar arcos no tabuleiro de ~%")
    (format t "forma a criar um quadrado, cada problema tem ~%")
    (format t "um número pré-definido de caixas fechadas objetivo ~%")
    (format t "que o utilizador tem de atingir com o menor número ~%")
    (format t "de arcos possível. ~%")
    (format t "   Com o recurso aos algoritmos a resolução de alguns~%")
    (format t "problemas torna-se instantânea. ~%~%")
    (format t "Escolha qualquer caractere ~%e em seguida Enter para voltar ao menu inicial. ~%")
    (let ((resposta (read)))
      (cond (T (start))))
    )
)

(defun voltar ()
  (progn
    (format t "~%~%~%Deseja resolver mais algum tabuleiro? ~%~%")
    (format t "1- Sim ~%")
    (format t "Outra opção- Não ~%")
    (let ((resposta (read)))
      (cond ((eq resposta '1) (selecionar-tabuleiro))
            (T (funcao-exit))))
    )
)

(defun funcao-exit ()
  (progn
    (format t "Adeus! ~%")
    )
)

;; ----------------------------------------------------- Funções de leitura de ficheiros -----------------------------------------------------

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

(defun ainda-nao-implementado (func &optional adicional)
  (format t "Função ainda não implementada. ~%~%")
  (funcall func adicional)
)