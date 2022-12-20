;; Interação com o utilizador e outros ficheiros de código
;; Escrita e leitura de ficheiros
;; 
;; Aurélio Miranda - 202000572
;; Bruno Silva - 202002258


;; Este método começa o programa
;; (start)
(defun start ()
  (progn
    (format t "~%~%******************************** ~%")
    (format t "*                              * ~%")
    (format t "*     Dots and Boxes           * ~%")
    (format t "*                              * ~%")
    (format t "*  Opt 1: Selecionar Tabuleiro * ~%")
    (format t "*  Opt 2: Ajuda                * ~%")
    (format t "*  Opt e: Sair                 * ~%")
    (format t "*                              * ~%")
    (format t "******************************** ~%")
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
    (format t "~%~%******************************* ~%")
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
      (cond ((eq resposta 1) (ler-algoritmo (get-lista '1)))
        ((eq resposta 2) (ler-algoritmo (no-teste-b)))
        ((eq resposta 3) (ler-algoritmo (no-teste-c)))
        ((eq resposta 4) (ler-algoritmo (no-teste-d)))
        ((eq resposta 5) (ler-algoritmo (no-teste-e)))
        ((eq resposta 6) (ler-algoritmo (no-teste-f)))
        ((eq resposta 7) (ler-algoritmo (get-lista '7)))
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
      (cond ((eq resposta '1) (resolver 'bfs escolha))
            ((eq resposta '2) (resolver 'dfs escolha))
            ((eq resposta '3) (ainda-nao-implementado 'ler-algoritmo escolha))
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

;; (get-lista 1)
(defun get-lista (indice)
  "vai buscar a lista de valores de um tabuleiro ao ficheiro"
  (cria-no (append (string-to-list (nth indice (ler-ficheiro)))
     (string-to-list (nth (+ indice 1) (ler-ficheiro)))))
)

;; converte uma string em lista
(defun string-to-list (str)
  "converte uma string em lista"
   (do* ((stringstream (make-string-input-stream str))
         (result nil (cons next result))
         (next (read stringstream nil 'eos)
               (read stringstream nil 'eos)))
        ((equal next 'eos) (reverse result))))

; (escrever-ficheiro (ler-ficheiro))
; (ler-ficheiro)
(defun ler-ficheiro ()
  (with-open-file (stream "problemas.dat")
    (loop for line = (read-line stream nil)
          while line
          collect 
          (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)
            (substitute #\Space #\, line))
          )))


;; (escrever-ficheiro (escreve-no (bfs (no-teste-a) 'no-solucaop 'sucessores-bfs (operadores) nil nil) 2))
;; (escrever-ficheiro (ler-ficheiro "problemas.dat"))
(defun escrever-ficheiro (escrever)
  (with-open-file (str "resultados.dat"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
  (format str "~a~%~%~%" escrever))
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
    (format t "Função ainda não implementada. ~a~%" (get-universal-time))
    (sleep 1)
    (get-universal-time)
)

(defun ainda-nao-implementado (func &optional adicional)
  (format t "Função ainda não implementada. ~%~%")
  (funcall func adicional)
)