# Lambda Calculus

## Grammar

```bnf
e ::= x               ; Variable
    | |x : t| e       ; Funtion
    | e e             ; Application
    | decimal         ; Int Literal
    | let x = e in e  ; Let binding

p ::= forall x̅. m     ; Generic type

m ::= x               ; Type name
    | m -> m          ; Function Type
```

```

                 (x : t) ∈ Γ
            --------------------  (Var)
                  Γ ⊢ x : t


          Γ, x : a ⊢ e : b      t = a
  -------------------------------------- (Abs)
           Γ ⊢ (|x : t| e) : (a -> b)


       Γ ⊢ f : u -> t        Γ ⊢ a : u
  ---------------------------------------- (App)
                Γ ⊢ (f a) : t


          ------------------------- (Const)
               Γ ⊢ decimal : int

```