;; Title: EcoCarbonRouter
;; Carbon Offset Token Router - Production Ready v1.0

;; Error Codes
(define-constant ERR-NOT-ADMIN (err u100))
(define-constant ERR-PROJECT-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u102))
(define-constant ERR-CAP-EXCEEDED (err u103))
(define-constant ERR-MINT-FAIL (err u104))
(define-constant ERR-NOT-OWNER (err u105))
(define-constant ERR-ALREADY-RETIRED (err u106))
(define-constant ERR-INVALID-ARG (err u107))
(define-constant ERR-TRANSFER-FAIL (err u108))
(define-constant ERR-NOT-FOUND (err u109))

;; NFT Definition
(define-non-fungible-token carbon-offset uint)

;; Data Variables
(define-data-var admin principal tx-sender)
(define-data-var treasury principal tx-sender)
(define-data-var project-counter uint u0)
(define-data-var token-counter uint u0)

;; Maps
(define-map projects uint
  {owner: principal,
   metadata: (buff 64),
   price: uint,
   cap: uint,
   minted: uint,
   verified: bool})

(define-map tokens uint
  {project-id: uint,
   uri: (buff 128),
   retired: bool})

;; Private Functions
(define-private (mint-nft (recipient principal))
  (let ((id (+ u1 (var-get token-counter))))
    (var-set token-counter id)
    (try! (nft-mint? carbon-offset id recipient))
    (ok id)))

(define-private (get-project-safe (id uint))
  (let ((project (unwrap! (map-get? projects id) ERR-PROJECT-NOT-FOUND)))
    (ok {
      owner: (get owner project),
      metadata: (get metadata project),
      price: (get price project),
      cap: (get cap project),
      minted: (get minted project),
      verified: (get verified project)
    })))

;; Public Functions
(define-public (set-treasury (new-treasury principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-ADMIN)
    (asserts! (not (is-none (some new-treasury))) ERR-INVALID-ARG)
    (var-set treasury new-treasury)
    (ok true)))

(define-public (register-project (metadata (buff 64)) (price uint) (cap uint) (verified bool))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-ADMIN)
    (asserts! (and (> price u0) (> cap u0)) ERR-INVALID-ARG)
    (asserts! (not (is-none (some metadata))) ERR-INVALID-ARG)
    (let ((id (+ u1 (var-get project-counter))))
      (var-set project-counter id)
      (map-set projects id
        {owner: tx-sender,
         metadata: metadata,
         price: price,
         cap: cap,
         minted: u0,
         verified: verified})
      (ok id))))

(define-public (update-project (id uint) (metadata (buff 64)) (price uint) (cap uint) (verified bool))
  (let ((project (try! (get-project-safe id))))
    (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-ADMIN)
    (asserts! (and (> price u0) (> cap u0)) ERR-INVALID-ARG)
    (asserts! (not (is-none (some metadata))) ERR-INVALID-ARG)
    (map-set projects id
      {owner: (get owner project),
       metadata: metadata,
       price: price,
       cap: cap,
       minted: (get minted project),
       verified: verified})
    (ok true)))

(define-public (buy-offsets (project-id uint) (uri-prefix (buff 128)))
  (let ((project (try! (get-project-safe project-id)))
        (validated-project-id (unwrap! (if (is-eq project-id u0) none (some project-id)) ERR-INVALID-ARG)))
    (match (map-get? projects validated-project-id)
      project-data (let ((available (- (get cap project-data) (get minted project-data))))
        (asserts! (> available u0) ERR-CAP-EXCEEDED)
        (asserts! (not (is-none (some uri-prefix))) ERR-INVALID-ARG)
        (try! (stx-transfer? (get price project-data) tx-sender (as-contract tx-sender)))
        (let ((token-id (try! (mint-nft tx-sender))))
          (map-set projects validated-project-id
            {owner: (get owner project-data),
             metadata: (get metadata project-data),
             price: (get price project-data),
             cap: (get cap project-data),
             minted: (+ u1 (get minted project-data)),
             verified: (get verified project-data)})
          (map-set tokens token-id
            {project-id: validated-project-id,
             uri: uri-prefix,
             retired: false})
          (ok token-id)))
      ERR-PROJECT-NOT-FOUND)))

(define-public (retire-offset (token-id uint))
  (let ((owner (unwrap! (nft-get-owner? carbon-offset token-id) ERR-NOT-OWNER))
        (token (unwrap! (map-get? tokens token-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender owner) ERR-NOT-OWNER)
    (asserts! (not (get retired token)) ERR-ALREADY-RETIRED)
    (map-set tokens token-id
      {project-id: (get project-id token),
       uri: (get uri token),
       retired: true})
    (ok true)))

(define-public (withdraw (to principal) (amount uint))
  (let ((valid-amount (unwrap! (if (> amount u0) (some amount) none) ERR-INVALID-ARG))
        (valid-recipient (unwrap! (if (not (is-eq to tx-sender)) (some to) none) ERR-INVALID-ARG)))
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-ADMIN)
    (try! (as-contract (stx-transfer? valid-amount tx-sender valid-recipient)))
    (ok true)))

(define-public (withdraw-all-to-treasury)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-ADMIN)
    (let ((balance (stx-get-balance (as-contract tx-sender))))
      (try! (as-contract (stx-transfer? balance tx-sender (var-get treasury))))
      (ok balance))))

;; View Functions
(define-read-only (get-project (id uint))
  (map-get? projects id))

(define-read-only (get-token (id uint))
  (map-get? tokens id))

(define-read-only (get-treasury)
  (var-get treasury))

(define-read-only (get-admin)
  (var-get admin))