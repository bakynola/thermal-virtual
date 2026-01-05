;; Thermal Virtual - Climate-Aware Land NFT Smart Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-already-minted (err u102))
(define-constant err-not-found (err u103))
(define-constant err-invalid-coordinates (err u104))
(define-constant err-unauthorized-oracle (err u105))

;; Data Variables
(define-data-var last-token-id uint u0)
(define-data-var thermal-token-supply uint u0)

;; NFT Definition
(define-non-fungible-token thermal-land uint)

;; Data Maps
(define-map land-properties
  { token-id: uint }
  {
    latitude: int,
    longitude: int,
    temperature: int,
    humidity: uint,
    vegetation-index: uint,
    last-updated: uint,
    owner: principal
  }
)

(define-map thermal-token-balance
  { owner: principal }
  { balance: uint }
)

(define-map authorized-oracles
  { oracle: principal }
  { authorized: bool }
)

(define-map climate-achievements
  { token-id: uint }
  {
    trees-planted: uint,
    renewable-energy: uint,
    carbon-offset: uint,
    sustainability-score: uint
  }
)

;; Authorization Functions
(define-public (authorize-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set authorized-oracles { oracle: oracle } { authorized: true }))
  )
)

(define-private (is-authorized-oracle (oracle principal))
  (default-to false (get authorized (map-get? authorized-oracles { oracle: oracle })))
)

;; Mint Land NFT
(define-public (mint-land (latitude int) (longitude int))
  (let
    (
      (token-id (+ (var-get last-token-id) u1))
    )
    (asserts! (and (>= latitude -90000) (<= latitude 90000)) err-invalid-coordinates)
    (asserts! (and (>= longitude -180000) (<= longitude 180000)) err-invalid-coordinates)
    
    (try! (nft-mint? thermal-land token-id tx-sender))
    
    (map-set land-properties
      { token-id: token-id }
      {
        latitude: latitude,
        longitude: longitude,
        temperature: 0,
        humidity: u50,
        vegetation-index: u50,
        last-updated: block-height,
        owner: tx-sender
      }
    )
    
    (map-set climate-achievements
      { token-id: token-id }
      {
        trees-planted: u0,
        renewable-energy: u0,
        carbon-offset: u0,
        sustainability-score: u0
      }
    )
    
    (var-set last-token-id token-id)
    (ok token-id)
  )
)

;; Update Climate Data (Oracle Function)
(define-public (update-climate-data 
  (token-id uint) 
  (temperature int) 
  (humidity uint) 
  (vegetation-index uint))
  (begin
    (asserts! (is-authorized-oracle tx-sender) err-unauthorized-oracle)
    (asserts! (is-some (map-get? land-properties { token-id: token-id })) err-not-found)
    
    (let
      (
        (current-props (unwrap! (map-get? land-properties { token-id: token-id }) err-not-found))
      )
      (ok (map-set land-properties
        { token-id: token-id }
        (merge current-props {
          temperature: temperature,
          humidity: humidity,
          vegetation-index: vegetation-index,
          last-updated: block-height
        })
      ))
    )
  )
)

;; Sustainable Actions - Plant Trees
(define-public (plant-trees (token-id uint) (tree-count uint))
  (let
    (
      (land-owner (unwrap! (nft-get-owner? thermal-land token-id) err-not-found))
      (current-achievements (unwrap! (map-get? climate-achievements { token-id: token-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender land-owner) err-not-token-owner)
    
    (map-set climate-achievements
      { token-id: token-id }
      (merge current-achievements {
        trees-planted: (+ (get trees-planted current-achievements) tree-count),
        sustainability-score: (+ (get sustainability-score current-achievements) (* tree-count u10))
      })
    )
    
    ;; Reward THERMAL tokens
    (mint-thermal-tokens tx-sender (* tree-count u100))
    (ok true)
  )
)

;; Install Renewable Energy
(define-public (install-renewable-energy (token-id uint) (capacity uint))
  (let
    (
      (land-owner (unwrap! (nft-get-owner? thermal-land token-id) err-not-found))
      (current-achievements (unwrap! (map-get? climate-achievements { token-id: token-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender land-owner) err-not-token-owner)
    
    (map-set climate-achievements
      { token-id: token-id }
      (merge current-achievements {
        renewable-energy: (+ (get renewable-energy current-achievements) capacity),
        sustainability-score: (+ (get sustainability-score current-achievements) (* capacity u20))
      })
    )
    
    (mint-thermal-tokens tx-sender (* capacity u150))
    (ok true)
  )
)

;; Purchase Carbon Offsets
(define-public (purchase-carbon-offset (token-id uint) (offset-amount uint))
  (let
    (
      (land-owner (unwrap! (nft-get-owner? thermal-land token-id) err-not-found))
      (current-achievements (unwrap! (map-get? climate-achievements { token-id: token-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender land-owner) err-not-token-owner)
    
    (map-set climate-achievements
      { token-id: token-id }
      (merge current-achievements {
        carbon-offset: (+ (get carbon-offset current-achievements) offset-amount),
        sustainability-score: (+ (get sustainability-score current-achievements) (* offset-amount u5))
      })
    )
    
    (mint-thermal-tokens tx-sender (* offset-amount u50))
    (ok true)
  )
)

;; THERMAL Token Management
(define-private (mint-thermal-tokens (recipient principal) (amount uint))
  (let
    (
      (current-balance (default-to { balance: u0 } (map-get? thermal-token-balance { owner: recipient })))
    )
    (map-set thermal-token-balance
      { owner: recipient }
      { balance: (+ (get balance current-balance) amount) }
    )
    (var-set thermal-token-supply (+ (var-get thermal-token-supply) amount))
    true
  )
)

;; Transfer Land NFT
(define-public (transfer-land (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) err-not-token-owner)
    (let
      (
        (current-props (unwrap! (map-get? land-properties { token-id: token-id }) err-not-found))
      )
      (try! (nft-transfer? thermal-land token-id sender recipient))
      (ok (map-set land-properties
        { token-id: token-id }
        (merge current-props { owner: recipient })
      ))
    )
  )
)

;; Read-Only Functions
(define-read-only (get-land-properties (token-id uint))
  (map-get? land-properties { token-id: token-id })
)

(define-read-only (get-climate-achievements (token-id uint))
  (map-get? climate-achievements { token-id: token-id })
)

(define-read-only (get-thermal-balance (owner principal))
  (default-to { balance: u0 } (map-get? thermal-token-balance { owner: owner }))
)

(define-read-only (get-last-token-id)
  (ok (var-get last-token-id))
)

(define-read-only (get-total-thermal-supply)
  (ok (var-get thermal-token-supply))
)

(define-read-only (get-land-owner (token-id uint))
  (ok (nft-get-owner? thermal-land token-id))
)