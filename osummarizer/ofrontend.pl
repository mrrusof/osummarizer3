id(I) --> [I], { atom(I) }.
cst(C) --> [C], { number(C) }.

expr(E) --> id(E).
expr(E) --> cst(E).
expr(E) --> ['('], expr(E), [')'].
expr(E) --> ['begin'], expr(E), ['end'].
% ∣ ( expr :  typexpr )
% ∣ expr  {, expr}+
% ∣ constr  expr
% ∣ `tag-name  expr
% ∣ expr ::  expr
% ∣ [ expr  { ; expr }  [;] ]
% ∣ [| expr  { ; expr }  [;] |]
% ∣ { field =  expr  { ; field =  expr }  [;] }
% ∣ { expr with  field =  expr  { ; field =  expr }  [;] }
% ∣ expr  { argument }+
% ∣ prefix-symbol  expr
% ∣ - expr
% ∣ -. expr
% ∣ expr  infix-op  expr
% ∣ expr .  field
% ∣ expr .  field <-  expr
% ∣ expr .(  expr )
% ∣ expr .(  expr ) <-  expr
% ∣ expr .[  expr ]
% ∣ expr .[  expr ] <-  expr
% expr(ite(E0, E1, unit@l0)) --> [if], expr(E0), [then], expr(E).
expr(ite(E0, E1, E2)) --> [if], expr(E0), [then], expr(E1), [else], expr(E2).
% ∣ while expr do  expr done
% ∣ for value-name =  expr  ( to ∣  downto ) expr do  expr done
% ∣ expr ;  expr
% ∣ match expr with  pattern-matching
% ∣ function pattern-matching
% ∣ fun multiple-matching
% ∣ try expr with  pattern-matching
% ∣ let [rec] let-binding  { and let-binding } in  expr
% ∣ new class-path
% ∣ object class-body end
% ∣ expr #  method-name
% ∣ inst-var-name
% ∣ inst-var-name <-  expr
% ∣ ( expr :>  typexpr )
% ∣ ( expr :  typexpr :>  typexpr )
% ∣ {< [ inst-var-name =  expr  { ; inst-var-name =  expr }  [;] ] >}


user:runtime_entry(start) :-
        read(user, Source),
        expr(E, Source, []),
        portray_clause(E).