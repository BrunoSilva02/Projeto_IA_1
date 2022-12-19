;; BF

;; 1. NÓ INICIAL -> LISTA ABERTOS
;; 2. LENGTH LISTA ABERTOS = 0 -> NIL
;; 3. N = PRIMEIRO NÓ DE LISTA ABERTOS (REMOVENDO)
;; 4. N -> ADICIONAR EM LISTA FECHADOS
;; 5. EXPANDIR N -> SUCESSORES PARA LISTA ABERTOS
;; 6. SE SUCESSOR = OBJETIO, ACABAR SE NÃO, IR PARA 2.

(defun bf-sucessores-objetivo (lista-sucessores numero-caixas-fechadas) 
    "Verifica na lista de sucessores se existe um com o número de caixas fechadas necessárias"

    (cond
        (
             (= (list-length lista-sucessores) 0)
             nil
        ) 
        (
            (= (contar-caixas-fechadas (caar lista-sucessores)) numero-caixas-fechadas) 
            T
        )
        (
            T 
            (bf-sucessores-objetivo (cdr lista-sucessores) numero-caixas-fechadas)
        )
    )
)


(defun algoritmo-bf (estado-inicial numero-caixas-fechadas &optional (lista-abertos ()) (lista-fechados ()))

    (cond 
        ((= (list-lenght lista-abertos) 0)
            
           (let ((abertos (cons estado-inicial ())))
         
                (cond 
                    ((= (list-length estado-inicial) 0) nil)
            
                    (T 

                        (cond 

                            (((= (list-lenght lista-fechados) 0))

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

                            (((= (list-lenght lista-fechados) 0))

                                (let ((fechados (cons (first abertos))))
                            
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


