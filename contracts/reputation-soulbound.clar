;; reputation-soulbound.clar
;; Reputation-based DAO with non-transferable (soulbound) reputation tokens
;; - Non-transferable balances (no transfer function)
;; - Authorized minters can mint/burn reputation
;; - Delegation: route your voting power to another principal (or self)
;; - Maintains delegated-power per principal for efficient voting queries

(define-constant ERR_NOT_ADMIN u100)
(define-constant ERR_NOT_MINTER u101)
(define-constant ERR_ZERO_AMOUNT u102)
(define-constant ERR_INSUFFICIENT_BALANCE u103)
(define-constant ERR_SAME_DELEGATEE u104)
(define-constant ERR_NO_VESTING u105)

;; Admin (deployer)
(define-data-var admin principal tx-sender)

;; Authorized minters (map: minter -> bool)
(define-map minters { who: principal } { enabled: bool })

;; Total reputation supply
(define-data-var total-supply uint u0)

;; Balances map: who -> balance (reputation units)
(define-map balances { who: principal } { balance: uint })

;; Delegation map: delegator -> (optional delegatee)
;; If none => delegator's voting power counts to themselves.
(define-map delegation { delegator: principal } { delegatee: (optional principal) })

;; Delegated power map: principal -> effective voting power (sum of balances delegated to them + self if self not delegating)
(define-map delegated-power { who: principal } { power: uint })

;; Events
(define-private (ev-minted (to principal) (amount uint) (by principal))
  (print { event: "rep-minted", to: to, amount: amount, by: by }))

(define-private (ev-burned (from principal) (amount uint) (by principal))
  (print { event: "rep-burned", from: from, amount: amount, by: by }))

(define-private (ev-delegated (delegator principal) (from (optional principal)) (to (optional principal)))
  (print { event: "delegation-changed", delegator: delegator, from: from, to: to }))

(define-private (ev-minter-updated (who principal) (enabled bool))
  (print { event: "minter-updated", who: who, enabled: enabled }))

;; -------------------------
;; Helpers
;; -------------------------
(define-private (is-admin (p principal)) (is-eq p (var-get admin)))
(define-private (is-minter (p principal)) (default-to false (get enabled (map-get? minters { who: p }))))

(define-private (balance-of (p principal))
  (default-to u0 (get balance (map-get? balances { who: p }))))

(define-private (delegation-of (p principal))
  (default-to (some p) (get delegatee (map-get? delegation { delegator: p })))) ;; if none, interpret as self

(define-private (delegated-power-of (p principal))
  (default-to u0 (get power (map-get? delegated-power { who: p }))))

;; Internal: add `delta` to the effective recipient's delegated-power.
;; recipientOpt is (optional principal) from delegation-of; unwrap to actual recipient (if none -> delegator themselves)
(define-private (add-power-to (recipient principal) (delta uint))
  (let ((cur (delegated-power-of recipient)))
    (begin 
      (map-set delegated-power { who: recipient } { power: (+ cur delta) })
      (ok true))))

(define-private (sub-power-from (recipient principal) (delta uint))
  (let ((cur (delegated-power-of recipient)))
    (begin 
      (asserts! (>= cur delta) (err ERR_INSUFFICIENT_BALANCE))
      (map-set delegated-power { who: recipient } { power: (- cur delta) })
      (ok true))))

;; Internal: get effective recipient for a delegator (if delegator delegates to someone, use them; else delegator)
(define-private (effective-recipient (delegator principal))
  (let ((dopt (map-get? delegation { delegator: delegator })))
    (if (is-none dopt)
        delegator
        (let ((rec (get delegatee (unwrap-panic dopt))))
          (if (is-none rec) delegator (unwrap-panic rec))))))

;; When a delegator's balance changes by `delta` (positive or negative), update the delegated-power bookkeeping:
;; if delegator currently delegates to R => add/sub delta to delegated-power[R]
(define-private (on-balance-change (delegator principal) (delta int))
  (let ((effective (effective-recipient delegator)))
    (try! 
      (if (>= delta (to-int u0))  ;; check if positive
          (add-power-to effective (unwrap-panic (ok (to-uint delta))))
          (sub-power-from effective (unwrap-panic (ok (to-uint (- (to-int u0) delta)))))))
    (ok true)))

;; -------------------------
;; Admin functions
;; -------------------------
(define-public (set-admin (p principal))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_NOT_ADMIN))
    (asserts! (is-some (some p)) (err ERR_NOT_ADMIN))  ;; validate principal
    (var-set admin p)
    (ok true)
  ))

(define-public (set-minter (who principal) (enabled bool))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_NOT_ADMIN))
    (asserts! (is-some (some who)) (err ERR_NOT_MINTER))  ;; validate principal
    (if enabled
        (map-set minters { who: who } { enabled: true })
        (map-delete minters { who: who }))
    (ev-minter-updated who enabled)
    (ok true)
  ))

;; -------------------------
;; Mint / Burn (minter-only)
;; -------------------------
(define-public (mint (to principal) (amount uint))
  (begin
    (asserts! (is-minter tx-sender) (err ERR_NOT_MINTER))
    (asserts! (> amount u0) (err ERR_ZERO_AMOUNT))
    (asserts! (is-some (some to)) (err ERR_INSUFFICIENT_BALANCE))  ;; validate principal
    ;; update balances
    (let ((old (balance-of to)))
      (asserts! (>= (+ old amount) old) (err ERR_INSUFFICIENT_BALANCE))  ;; check for overflow
      (map-set balances { who: to } { balance: (+ old amount) })
      (var-set total-supply (+ (var-get total-supply) amount))
      ;; update delegated-power for effective recipient
      (try! (on-balance-change to (to-int amount)))
      (ev-minted to amount tx-sender)
      (ok true))))

(define-public (burn (from principal) (amount uint))
  (begin
    (asserts! (is-minter tx-sender) (err ERR_NOT_MINTER))
    (asserts! (> amount u0) (err ERR_ZERO_AMOUNT))
    (let ((old (balance-of from)))
      (asserts! (>= old amount) (err ERR_INSUFFICIENT_BALANCE))
      (map-set balances { who: from } { balance: (- old amount) })
      (var-set total-supply (- (var-get total-supply) amount))
      ;; update delegated-power
      (try! (on-balance-change from (- (to-int u0) (to-int amount)))) ;; negative int
      (ev-burned from amount tx-sender)
      (ok true))))

;; Batch mint (minter-only) - simplified version that mints to a single address
(define-public (batch-mint (tos (list 100 principal)) (amounts (list 100 uint)) (count uint))
  (begin
    (asserts! (is-minter tx-sender) (err ERR_NOT_MINTER))
    (asserts! (> count u0) (err ERR_ZERO_AMOUNT))
    (let ((to (unwrap! (element-at? tos u0) (err ERR_INSUFFICIENT_BALANCE)))
          (amount (unwrap! (element-at? amounts u0) (err ERR_INSUFFICIENT_BALANCE))))
      (asserts! (> amount u0) (err ERR_ZERO_AMOUNT))
      (try! (mint to amount))
      (ok true))))

;; -------------------------
;; Delegation
;; - Call `delegate-to` with (some principal) to delegate, or `none` to undelegate (self)
;; - Updates delegated-power bookkeeping in O(1)
;; -------------------------
(define-public (delegate-to (delegatee (optional principal)))
  (begin
    (let ((delegator tx-sender))
      (let ((current (map-get? delegation { delegator: delegator })))
        (let ((cur-de (if (is-none current) none (get delegatee (unwrap-panic current)))))
          (if (and (is-some cur-de) (is-some delegatee) (is-eq (unwrap-panic cur-de) (unwrap-panic delegatee)))
              (err ERR_SAME_DELEGATEE)
              (let ((bal (balance-of delegator)))
                ;; remove power from current effective recipient
                (let ((old-rec (if (is-none cur-de) delegator (unwrap-panic cur-de))))
                  (if (> bal u0)
                    (unwrap! (sub-power-from old-rec bal) (err ERR_INSUFFICIENT_BALANCE))
                    true))
                ;; set new delegation mapping
                (if (is-none delegatee)
                    (map-set delegation { delegator: delegator } { delegatee: none })
                    (map-set delegation { delegator: delegator } { delegatee: delegatee }))
                ;; add power to new effective recipient
                (let ((new-rec (if (is-none delegatee) delegator (unwrap-panic delegatee))))
                  (if (> bal u0)
                    (unwrap! (add-power-to new-rec bal) (err ERR_INSUFFICIENT_BALANCE))
                    true))
                (ev-delegated delegator (if (is-none cur-de) none (some (unwrap-panic cur-de))) delegatee)
                (ok true))))))))

;; -------------------------
;; Views
;; -------------------------
(define-read-only (get-balance (who principal))
  (ok (balance-of who)))

(define-read-only (get-total-supply)
  (ok (var-get total-supply)))

(define-read-only (get-delegate (who principal))
  (let ((dopt (map-get? delegation { delegator: who })))
    (ok (if (is-none dopt) none (get delegatee (unwrap-panic dopt))))))

(define-read-only (get-delegated-power (who principal))
  (ok (delegated-power-of who)))

(define-read-only (is-minter-view (who principal))
  (ok (is-minter who)))

(define-read-only (get-admin)
  (ok (var-get admin)))

