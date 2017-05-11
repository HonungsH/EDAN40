{- Test for Program -}
module TestProgram where

import Program

p, p1, p2, p3 :: Program.T
p = fromString  ("\
\read k;\
\read n;\
\m := 1;\
\while n-m do\
\  begin\
\    if m - m/k*k then\
\      skip;\
\    else\
\      write m;\
\    m := m + 1;\
\  end")

p1 = fromString  ("\
\read n;\
\read b;\
\m := 1;\
\s := 0;\
\p := 1;\
\while n do\
\  begin\
\    q := n/b;\
\    r := n - q*b;\
\    write r;\
\    s := p*r+s;\                    
\    p := p*10;\
\    n :=q;\
\  end\
\write s;")

sp = putStr (toString p)

sp1 = putStr (toString p1)

p2 = fromString (toString p)

p3 = fromString (toString p1)

rp = Program.exec p [3,16]

rp1 = Program.exec p1 [1024, 2]

p4 = fromString  ("\
\read a;\
\read b;\
\-- a comment\
\s := 3;\
\while a do\
\  begin\
\    c := a^s;\
\    d := 2^a;\
\    write c;\
\    write d;\                    
\    a := a-1;\
\  end\
\write a;")

rp4 = Program.exec p4 [4,4]
