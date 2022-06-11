 (eval-exp 
    (call 
        (closure '() 
                 (fun #f "x" (add (var "x") (int 7)))) 
        (int 1)))