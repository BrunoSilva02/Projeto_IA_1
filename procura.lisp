;; Algoritmos de Procura
;; 
;; Aurélio Miranda - 202000572
;; Bruno Silva - 202002258

;;; Funcoes auxiliares do no
;;; predicado no-solucaop que verifica se um estado e final
;; teste: (no-solucaop (no-teste-a))
;; resultado: NIL
(defun no-solucaop (no)
  "Verifica se o no é um no solucao"
  (cond ((eq (contar-caixas-fechadas (no-estado no)) (no-objetivo no)) T)
   (t NIL))
)

;;; função que calcula a heuristica de um nó (dada no enunciado)
;; teste: (heuristica (no-teste-a))
;; resultado: 9
(defun heuristica (no)
  "Calcula a heuristica de um no"
  (- (no-objetivo no) (contar-caixas-fechadas (no-estado no)))
)

;;; função que calcula a heuristica de um nó (proposta pelos alunos)
;; teste: (heuristica-proposta (no-teste-a))
;; resultado: 9
(defun heuristica-proposta (no)
  "Calcula a heuristica proposta pelos alunos de um no"
  (- (no-objetivo no) (contar-caixas-fechadas (no-estado no)))
)


;;; Funcao auxiliar da geração de nos
;;; gera um novo nó
;; teste: (novo-sucessor (no-teste-a) 1 1 (car (operadores)))
;; resultado: ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((1 0 0) (0 1 1) (1 0 1) (0 1 1))) 1 9
;;            ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 1) (1 0 1) (0 1 1))) 0 99 NIL 10) 10)
(defun novo-sucessor(no l i func)
  "Gera um novo sucessor do no"
  (cond
    ((null (funcall func l i (no-estado no))) NIL)
    (t (cria-no (funcall func l i (no-estado no)) (+ (no-profundidade no) 1) 
        (heuristica no) no (no-objetivo no))
    )
  )
)

;;; Funcao geradora de nos
;;; gera todos os nos filho
;; teste: (sucessores-bfs (no-teste-a) (operadores))
;; resultado: (((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((1 0 0) (0 1 0) (0 0 1) (0 1 1))) 1 2
;;  ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1))) 0 0
;;   NIL 3)
;;  3)
;; ((((0 0 0) ...
(defun sucessores-bfs (no funcs)
  "Gera todos os sucessores do no, no contexto do algoritmo bfs"
  (remove nil 
    (append-list 
      (mapcar (lambda (funcao) 
        (mapcar (lambda (l-i) 
          (novo-sucessor no (first l-i)(second l-i) funcao)
          ) (encontrar-l-i (no-estado no))
        )
      )funcs)
    )
  )
)

;;; Funcao geradora de nos
;;; gera todos os nos filho
;; teste: (sucessores-dfs (no-teste-a) (operadores) 5)
;; resultado: (((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((1 0 0) (0 1 0) (0 0 1) (0 1 1))) 1 2
;;  ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1))) 0 0
;;   NIL 3)  3) ...
(defun sucessores-dfs (no funcs prof)
  "Gera todos os sucessores do no, no contexto do algoritmo dfs"
  (cond
    ((>= (no-profundidade no) prof) NIL)
    (T (remove nil 
        (append-list 
          (mapcar (lambda (funcao) 
            (mapcar (lambda (l-i) 
              (novo-sucessor no (first l-i)(second l-i) funcao)
              ) (encontrar-l-i (no-estado no))
            )
          )funcs)
        )
      ))
   )
)

;; Ordenação para Breadth-First Search
;; (trace abertos-bfs)
(defun abertos-bfs (abertos sucessores)
  "Ordenação para Breadth-First Search"
  (cond
     ((null abertos)sucessores)
     (T (append abertos sucessores))
   )
)

;; Ordenação para Depth-First Search
;; (trace abertos-dfs)
(defun abertos-dfs (abertos sucessores)
 "Ordenação para Depth-First Search"
   (cond
     ((null abertos)sucessores)
     (T (append sucessores abertos))
   )
)

;; Verificar Existência de um no numa lista
;; (trace no-existep)
(defun no-existep(no lista &optional alg)
    "Verifica a existência de um Nó na lista"
    (cond
         ((null lista) NIL)
         ((equal (no-estado no) (no-estado (car lista))) T)
         (T (no-existep no (cdr lista)))
     )
)

;; Devolve uma lista sem sucessores presentes nos nos fechados
;; (trace no-diff)
(defun no-diff (sucessores fechados)
   "Devolve apenas nos que sejam diferentes aos nos em fechados"
   (cond
       ((null sucessores) NIL)
       ((null fechados) sucessores)
       ((no-existep (car sucessores) fechados) (no-diff (cdr sucessores) fechados))
       (T (cons (car sucessores) (no-diff (cdr sucessores) fechados)))
   )
)


;; -------------------------------------------------------------- Funções auxiliares ------------------------------------------------------


;; (trace encontrar-l-i)
;; teste - (encontrar-l-i (tabuleiro-problema-a))
;; resultado - ((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3) (4 1) (4 2) (4 3))
;; teste - (encontrar-l-i (tabuleiro-problema-b))
;; resultado - ((1 1) (1 2) (1 3) (1 4) (2 1) (2 2) (2 3) (2 4) (3 1) (3 2) (3 3) (3 4) (4 1) (4 2) (4 3) (4 4) (5 1) (5 2) (5 3) (5 4))
(defun encontrar-l-i (tabuleiro &optional (l 1) (i 1))
  "Encontra dos os l e i válidos para um dado tabuleiro"
  (cond
    ((< (length (get-arcos-verticais tabuleiro)) l) NIL)
    ((< (length (get-arcos-horizontais tabuleiro)) (+ i 1)) 
        (encontrar-l-i tabuleiro (+ l 1) 1))
    (T (cons (list l i) (encontrar-l-i tabuleiro l (+ i 1))))
  )
)


;; Junta o primeiro e o segundo elemento de uma lista num só elemento
(defun append-list (list)
  "Junta os 2 primeiros elementos de uma lista num só."
  (append (first list) (second list))
)

;; teste: (caminho (car (bfs (no-teste-a) 'no-solucaop 'sucessores-bfs (operadores) nil nil)))
;; resultado: (((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1)))
;;  (((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 1 1) (0 1 1))))
;; (((0 0 0) (0 1 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 1 1) (0 1 1))))
(defun caminho (no)
  "Devolve o caminho do nó inicial até ao resultado"
  (cond ((null (no-pai no)) (no-estado no))
    (t (append (caminho (no-pai no)) (no-estado no)))
  )
)

;; pesquisa se um no na lista de sucessores possui o numero de caixas desejado
(defun bf-sucessores-objetivo (lista-sucessores numero-caixas-fechadas) 
    "Verifica na lista de sucessores se existe um com o número de caixas fechadas necessárias"

    (cond
        (
             (= (list-length lista-sucessores) 0)
             nil
        ) 
        (
            (= (contar-caixas-fechadas (caar lista-sucessores)) numero-caixas-fechadas) 
            (car lista-sucessores)
        )
        (
            T 
            (bf-sucessores-objetivo (cdr lista-sucessores) numero-caixas-fechadas)
        )
    )
)


;; --------------------------------------------------------- Algoritmos ---------------------------------------------------------------------
;; Procura em largura
;; (trace bfs)
;; teste: (bfs (no-teste-b) 'no-solucaop 'sucessores-bfs (operadores) nil nil)
;; resultado: (((((0 0 0) (0 1 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 1 1) (0 1 1))) 2 1
;; ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 1 1) (0 1 1))) 1 2
;;  ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1))) 0 0
;;   NIL 3) 3) 3) 67 5)
(defun bfs(no solucao sucessores operadores &optional abertos fechados)
  "Define a função bfs que irá efetuar a procura em largura-primeiro"
   (cond
       ((and (null abertos)(null fechados)) 
          (bfs no solucao sucessores operadores (list no) fechados)) ; 1a iteração.
       ((null abertos) nil) ; Se ABERTOS vazia falha.
       ((funcall solucao (car abertos)) 
          (list (car abertos) (length abertos) (length fechados))) ; Se algum dos sucessores é um nó objectivo sai, e dá a solução.
       (T (bfs no solucao sucessores operadores 
            (abertos-bfs (cdr abertos) (no-diff (funcall sucessores (car abertos) operadores) fechados)) 
            (append fechados (list (car abertos)))) ; Expande o nó n. Coloca os sucessores no fim de ABERTOS.
        )
    )
)

;; procura na profundidade
;; teste: (dfs (no-teste-a) 'no-solucaop 'sucessores-dfs (operadores) 5 nil nil)
;; resultado: (((((0 0 0) (0 1 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 1 1) (0 1 1))) 2 1
;; ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 1 1) (0 1 1))) 1 2
;;  ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1))) 0 0
;;   NIL 3)  3) 3) 100 7)
(defun dfs(no solucao sucessores operadores profundidade &optional abertos fechados)
    "Função dfs que irá efetuar a procura em profundidade-primeiro"
   (cond
       ((and(null abertos)(null fechados)) 
        (dfs no solucao sucessores operadores profundidade (list no) fechados)) ; 1a iteração
       ((null abertos) nil) ; Se ABERTOS vazia falha
       ;((> (no-profundidade no) profundidade) nil) ; Se a profundidade de no é maior que profundidade não gera os sucessores
       (T (cond ((bf-sucessores-objetivo (no-diff (funcall sucessores (car abertos) operadores profundidade) fechados) (no-objetivo no))
             (list (bf-sucessores-objetivo (no-diff (funcall sucessores (car abertos) operadores profundidade) fechados) 
                  (no-objetivo no)) (length abertos) (length fechados))) ; Se algum dos sucessores é um nó objectivo sai, e dá a solução.
            (t (dfs no solucao sucessores operadores profundidade                                              
            (abertos-bfs (cdr abertos) (no-diff (funcall sucessores (car abertos) operadores profundidade) fechados)) 
            (append fechados (list (car abertos))))) ; Expande o nó n. Coloca os sucessores no início de ABERTOS.
          )
      )
    )
)

;; Procura informada a* (INCOMPLETO)
;; teste: - (a* (no-teste-c) 'no-solucaop 'sucessores-a* 'heuristica (operadores) nil nil)
;; resultado: -
(defun a*(no solucao sucessores heuristica-a operadores &optional abertos fechados)
    "Função a* que irá efetuar a procura informada com um heuristica"
   (cond
       ((and(null abertos)(null fechados)) 
        (a* no solucao sucessores operadores heuristica-a (list no) fechados)) ; 1a iteração
       ((null abertos) nil) ; Se ABERTOS vazia falha
       (T (cond ((bf-sucessores-objetivo (no-diff (funcall sucessores (car abertos) operadores) fechados) (no-objetivo no))
             (bf-sucessores-objetivo (no-diff (funcall sucessores (car abertos) operadores) fechados) 
                  (no-objetivo no))) ; Se algum dos sucessores é um nó objectivo sai, e dá a solução.
            (t (a* no solucao sucessores operadores heuristica-a
            (abertos-a* (cdr abertos) (no-diff (funcall sucessores (car abertos) operadores heuristica-a) fechados)) 
            (append fechados (list (car abertos))))) ; Expande o nó n. Coloca os sucessores no início de ABERTOS.
          )
      )
    )
)


;; --------------------------------------------------------------- Misc & Tentativas ---------------------------------------------------------
;; Tentativas da realização de algumas funções

  ;; BF

;; 1. NÓ INICIAL -> LISTA ABERTOS
;; 2. LENGTH LISTA ABERTOS = 0 -> NIL
;; 3. N = PRIMEIRO NÓ DE LISTA ABERTOS (REMOVENDO)
;; 4. N -> ADICIONAR EM LISTA FECHADOS
;; 5. EXPANDIR N -> SUCESSORES PARA LISTA ABERTOS
;; 6. SE SUCESSOR = OBJETIO, ACABAR SE NÃO, IR PARA 2.

;; (algoritmo-bf (no-teste-a) 3)
(defun algoritmo-bf (estado-inicial numero-caixas-fechadas &optional (lista-abertos ()) (lista-fechados ()))

    (cond 
        ((= (list-length lista-abertos) 0)
            
           (let ((abertos (cons estado-inicial ())))
         
                (cond 
                    ((= (list-length estado-inicial) 0) nil)
            
                    (T 

                        (cond 

                            ((= (list-length lista-fechados) 0)

                                (let ((fechados ()))
                            
                                    ;; adicionar aos fechados (first abertos)
                                    (append fechados (first abertos))

                                    ;; expandir primeiro nó dos abertos
                                    ;; verificar se está na lista de sucessores "estado-objetivo" (numero de caixas fechadas está good?)

                                    (cond 
                                        ( 
                                         (equal 
                                            (bf-sucessores-objetivo (sucessores-bfs (first abertos) (operadores)) numero-caixas-fechadas)
                                            T
                                            ) 
                                            ;; terminar ? (não sei o que acontece aqui)
                                            (list 'success)
                                        )  
                                        (
                                            T 
                                            (algoritmo-bf estado-inicial
                                                          numero-caixas-fechadas
                                                          (append abertos (sucessores-bfs (first abertos) (operadores)))
                                                          fechados
                                            )
                                        )
                                    )                        
                                )
                            )
                        
                            (T
                            
                                (let ((fechados lista-fechados))
                            
                                    ;; adicionar aos fechados (first abertos)
                                    (append fechados (first abertos))

                                    ;; expandir primeiro nó dos abertos
                                    ;; verificar se está na lista de sucessores "estado-objetivo" (numero de caixas fechadas está good?)

                                    (cond 
                                        ( 
                                         (equal 
                                            (bf-sucessores-objetivo (sucessores-bfs (first abertos) (operadores)) numero-caixas-fechadas)
                                            T
                                            ) 
                                            ;; terminar ? (não sei o que acontece aqui)
                                            (list 'success)
                                        )  
                                        (
                                            T 
                                            (algoritmo-bf estado-inicial
                                                          numero-caixas-fechadas
                                                          (append abertos (sucessores-bfs (first abertos) (operadores)))
                                                          fechados
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )           
        )

        (T

        (let ((abertos lista-abertos))
         
                (cond 
                    ((= (list-length estado-inicial) 0) nil)
            
                    (T 

                        (cond 

                            ((= (list-length lista-fechados) 0)

                                (let ((fechados (cons (first abertos) ())))
                            
                                    ;; adicionar aos fechados (first abertos)
                                    (append fechados (first abertos))

                                    ;; expandir primeiro nó dos abertos
                                    ;; verificar se está na lista de sucessores "estado-objetivo" (numero de caixas fechadas está good?)

                                    (cond 
                                        ( 
                                         (equal 
                                            (bf-sucessores-objetivo (sucessores-bfs (first abertos) (operadores)) numero-caixas-fechadas)
                                            T
                                            ) 
                                            ;; terminar ? (não sei o que acontece aqui)
                                            (list 'success)
                                        )  
                                        (
                                            T 
                                            (algoritmo-bf estado-inicial
                                                          numero-caixas-fechadas
                                                          (append abertos (sucessores-bfs (first abertos) (operadores)))
                                                          fechados
                                            )
                                        )
                                    )
                                                        
                                )
                            )
                        
                            (T                            
                                (let ((fechados lista-fechados))
                            
                                    ;; adicionar aos fechados (first abertos)
                                    (append fechados (first abertos))

                                    ;; expandir primeiro nó dos abertos
                                    ;; verificar se está na lista de sucessores "estado-objetivo" (numero de caixas fechadas está good?)

                                    (cond 
                                        ( 
                                         (equal 
                                            (bf-sucessores-objetivo (sucessores-bfs (first abertos) (operadores)) numero-caixas-fechadas)
                                            T
                                            ) 
                                            ;; terminar ? (não sei o que acontece aqui)
                                            (list 'success)
                                        )  
                                        (
                                            T 
                                            (algoritmo-bf estado-inicial
                                                          numero-caixas-fechadas
                                                          (append abertos (sucessores-bfs (first abertos) (operadores)))
                                                          fechados
                                            )
                                        )
                                    )                               
                                )
                            )
                        )
                    )
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


#|
(tabuleiro &optional (l 1) (i 1) melhor-pos val)

if (l i = 1) {aumenta valor} 

if (l+1 i = 1) {aumenta valor}

if (i l = 1) {aumenta valor}

if (i+1 l = 1) {aumenta valor}

if (valor > val) {(tabuleiro &optional (l 1) (i 1) tabuleiro val)}
|#

