; ficheiro de teste, testar funcionalidades e cenas assim

; podemos também correr o ficheiro bem rápido é só fazer clisp hello.lsp
; vai fazer o print neste caso e não corre a função de baixo
(print "Palavras fixes")

; para correr função -> make run e depois (test [numero])
(defun test (a) 
"função de teste"
    (list a)
)