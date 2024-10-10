;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TODO : Check edge cases for swaps, e.g. swapping negative amounts, swapping same token pairs... and more!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(set-option :produce-proofs true)

( declare-datatype Token ( ( t0 ) ( t1 ) ( t2 ) ))

( declare-datatype TokenAmount ( 
    ( amount ( t Token ) (v Real) )
))

( declare-datatype Amm ( 
    ( amm (r0 TokenAmount) (r1 TokenAmount) )
))

( declare-datatypes (( Maybe 1 )) (
( par ( X ) ( ( nothing ) ( just ( val X ))))))

( declare-datatype State ( 
    (pair (amms  (Array Token (Array Token (Maybe Amm))))
          (users (Array String (Array Token Real)))
    )
))

( declare-datatype Txn (( tx ( user String ) ( from TokenAmount ) ( to TokenAmount))))

( declare-datatype Conf (( triple ( g State) ( s State) ( q (List Txn)))))

( define-fun swap ((state State)
                   (swp   Txn))
                   State 
(
    ; TODO: rethink if the below approach is good/safe
    ; Identify AMM corresponding to token pair, and swap r0 r1, if (t from) != (t r0)
    let ((foundAmm (select (select (amms state) (t (from swp))) (t (to swp)))))
        ( match foundAmm ((nothing state) ((just foundAmmX)
        (let ((swappingAmm (
            ite (= (t (r0 foundAmmX)) (t (from swp)))
                   foundAmmX
                   (amm (r1 foundAmmX) (r0 foundAmmX)))))
        ; Calculate payout
        (let ((payout (/ (* (v (from swp)) (v (r1 swappingAmm)))
                         (+ (v (from swp)) (v (r0 swappingAmm))))))
              ; If swap withing x-rate, then execute, otherwise leave state unchanged
             (ite (and (<= 0      (v (to swp)))
                       (<= (v (to swp)) payout))
                  (let ((oldBal (select (users state) (user swp))))
                    (
                    let ((newBal 
                            (store 
                                (store oldBal 
                                       (t (to swp)) 
                                       (+ (select oldBal (t (to swp))) payout)
                                )
                                (t (from swp)) 
                                (- (select oldBal (t (from swp)))
                                   (v (from swp)))))
                         (newAmm (amm
                                  (amount (t (from swp)) (+ (v (r0 swappingAmm)) (v (from swp))))
                                  (amount (t (to swp)  ) (- (v (r1 swappingAmm)) payout))
                                  ))
                         )
                    ; return new state
                    (pair
                        (let ((oldTFromAmms (select (amms state) (t (from swp))))
                              (oldTToAmms   (select (amms state) (t (to swp)  ))))
                              ; update lookup corresponding to selecting t0 -> t1
                             (let ((tmpamms (store (amms state ) (t (from swp))
                                (store oldTFromAmms (t (to swp)) (just newAmm)))))
                              (store tmpamms (t (to swp)) (store oldTToAmms (t (from swp)) (just newAmm)))))
                        (store (users state) (user swp) newBal)
                        )))
                  state
            )
    )
))))))

( define-fun getBal ((state State)
                     (name String)
                     (tau  Token))
                      Real
(
    select (select (users state) name) tau
))

( declare-const txn1 Txn)

( declare-const state1 State )
( declare-const state2 State )

( declare-const conf1 Conf)
( declare-const conf2 Conf)

(assert (= state2 (swap state1 txn1)))
(assert (= conf1 (triple state1 state1 nil)))


; First selecting t, then t' is the same as selecting t' then t:
(assert (forall ((tau1 Token) (tau2 Token)) 
    (=> (distinct tau1 tau2) 
        (= (select (select (amms state1) tau1) tau2) 
           (select (select (amms state1) tau2) tau1)))))

; No amm can hold reserves of (t, t)
(assert (forall ((tau1 Token) (tau2 Token))
    (match (select (select (amms state1) tau1) tau2) ((nothing true) 
    ((just a) (distinct (t (r0 a)) (t (r1 a))))))))

; No AMM can hold negative reserves
(assert (forall ((tau1 Token) (tau2 Token))
    (match (select (select (amms state1) tau1) tau2) ((nothing true) 
    ((just a) (and (< 0 (v (r0 a))) (< 0 (v (r1 a)))))))))

(assert (forall ((tau1 Token) (tau2 Token))
    (match (select (select (amms state1) tau1) tau2) ((nothing true)  ; TODO: investigate if (nothing true) ok
    ((just a) 
        (xor (and (= (t (r0 a)) tau1) (= (t (r1 a)) tau2)) 
             (and (= (t (r1 a)) tau1) (= (t (r0 a)) tau2))))))))

; initial state of users
(assert (= (users state1)
    (store ((as const (Array String (Array Token Real)))
         ((as const (Array Token Real)) 0.0))
       "Sander"
       (store ((as const (Array Token Real)) 0.0) t1 6))))

(define-fun lempt () (Array Token (Maybe Amm))
    ((as const (Array Token (Maybe Amm))) nothing))

(define-fun hempt () (Array Token (Array Token (Maybe Amm)))
    ((as const (Array Token (Array Token (Maybe Amm)))) lempt))

(define-fun t1amm () (Array Token (Maybe Amm))
    (store lempt t1 (just (amm (amount t0 12.0) (amount t1 12.0)))))

(define-fun t0amm () (Array Token (Maybe Amm))
    (store lempt t0 (just (amm (amount t0 12.0) (amount t1 12.0)))))

; initial state of AMMs
(assert (= (amms state1)
(store (store hempt t0 t1amm) t1 t0amm)))


(assert (and
         (= 0 (select (select (users state1) "Sander") t0))
         (= 4 (select (select (users state2) "Sander") t0))
         (forall ((tau1 Token) (tau2 Token)) 
            (and
            (match (select (select (amms state2) tau1) tau2) ((nothing true) 
            ((just a) (and (< 0 (v (r0 a))) (< 0 (v (r1 a)))))))
              (= conf2 
                 (ite (forall ((uname String) (tau Token))
                       (>= (getBal state2 uname tau) 0)) 
                      (triple 
                          state2 ; green
                          state2 ; simulated
                           nil) 
                      (triple 
                           state1 ; green
                           state2 ; simulated
                          (insert txn1 (q conf1)))))
              (= (select (select (amms state2) tau1) tau2) 
                 (select (select (amms state2) tau2) tau1))
              (match (select (select (amms state2) tau1) tau2) ((nothing true) 
              ((just a) (distinct (t (r0 a)) (t (r1 a))))))))))

; Green state assertion
(assert (forall ((uname String) (tau Token))
    (>= (getBal state1 uname tau) 0)))


(check-sat)
(get-value (conf2))
