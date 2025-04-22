
;; title: AgriSupply



(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-state (err u104))
(define-constant err-invalid-input (err u105))

(define-data-var next-product-id uint u1)

(define-map products
  { product-id: uint }
  {
    name: (string-ascii 50),
    farm-id: uint,
    farmer: principal,
    planting-date: uint,
    harvest-date: uint,
    product-type: (string-ascii 20),
    organic: bool,
    active: bool
  }
)

(define-map farms
  { farm-id: uint }
  {
    name: (string-ascii 50),
    location: (string-ascii 100),
    owner: principal,
    certification: (string-ascii 30),
    registration-date: uint,
    active: bool
  }
)

(define-map supply-chain-events
  { product-id: uint, event-id: uint }
  {
    timestamp: uint,
    event-type: (string-ascii 20),
    handler: principal,
    location: (string-ascii 100),
    temperature: int,
    humidity: int,
    quality-score: uint,
    notes: (string-ascii 200)
  }
)

(define-map product-event-count
  { product-id: uint }
  { count: uint }
)

(define-map authorized-inspectors
  { inspector: principal }
  { active: bool }
)

(define-map farm-count
  { owner: principal }
  { count: uint }
)

(define-data-var next-farm-id uint u1)

(define-read-only (get-product (product-id uint))
  (map-get? products { product-id: product-id })
)

(define-read-only (get-farm (farm-id uint))
  (map-get? farms { farm-id: farm-id })
)

(define-read-only (get-supply-chain-event (product-id uint) (event-id uint))
  (map-get? supply-chain-events { product-id: product-id, event-id: event-id })
)

(define-read-only (get-product-event-count (product-id uint))
  (default-to { count: u0 } (map-get? product-event-count { product-id: product-id }))
)

(define-read-only (is-authorized-inspector (inspector principal))
  (default-to { active: false } (map-get? authorized-inspectors { inspector: inspector }))
)

(define-public (register-farm (name (string-ascii 50)) (location (string-ascii 100)) (certification (string-ascii 30)))
  (let
    (
      (farm-id (var-get next-farm-id))
      (owner-farm-count (default-to { count: u0 } (map-get? farm-count { owner: tx-sender })))
    )
    (map-set farms
      { farm-id: farm-id }
      {
        name: name,
        location: location,
        owner: tx-sender,
        certification: certification,
        registration-date: stacks-block-height,
        active: true
      }
    )
    (map-set farm-count
      { owner: tx-sender }
      { count: (+ u1 (get count owner-farm-count)) }
    )
    (var-set next-farm-id (+ farm-id u1))
    (ok farm-id)
  )
)

(define-public (register-product 
    (name (string-ascii 50)) 
    (farm-id uint) 
    (planting-date uint) 
    (product-type (string-ascii 20)) 
    (organic bool))
  (let
    (
      (product-id (var-get next-product-id))
      (farm (map-get? farms { farm-id: farm-id }))
    )
    ;; (asserts! farm err-not-found)
    ;; (asserts! (is-eq (get owner farm) tx-sender) err-unauthorized)
    (map-set products
      { product-id: product-id }
      {
        name: name,
        farm-id: farm-id,
        farmer: tx-sender,
        planting-date: planting-date,
        harvest-date: u0,
        product-type: product-type,
        organic: organic,
        active: true
      }
    )
    (var-set next-product-id (+ product-id u1))
    (ok product-id)
  )
)

(define-public (record-harvest (product-id uint) (harvest-date uint))
  (let
    (
      (product (unwrap! (map-get? products { product-id: product-id }) err-not-found))
    )
    (asserts! (is-eq (get farmer product) tx-sender) err-unauthorized)
    (asserts! (> harvest-date (get planting-date product)) err-invalid-input)
    
    (map-set products
      { product-id: product-id }
      (merge product { harvest-date: harvest-date })
    )
    
    (add-supply-chain-event product-id "HARVESTED" "" 0 0 u0 "Product harvested")
  )
)

(define-public (add-supply-chain-event 
    (product-id uint) 
    (event-type (string-ascii 20)) 
    (location (string-ascii 100)) 
    (temperature int) 
    (humidity int) 
    (quality-score uint) 
    (notes (string-ascii 200)))
  (let
    (
      (product (map-get? products { product-id: product-id }))
      (event-count (get-product-event-count product-id))
      (next-event-id (get count event-count))
    )
    ;; (asserts! product err-not-found)
    (asserts! 
      (or 
        ;; (is-eq tx-sender (get farmer product))
        (get active (is-authorized-inspector tx-sender))
      )
      err-unauthorized
    )
    
    (map-set supply-chain-events
      { product-id: product-id, event-id: next-event-id }
      {
        timestamp: stacks-block-height,
        event-type: event-type,
        handler: tx-sender,
        location: location,
        temperature: temperature,
        humidity: humidity,
        quality-score: quality-score,
        notes: notes
      }
    )
    
    (map-set product-event-count
      { product-id: product-id }
      { count: (+ u1 next-event-id) }
    )
    
    (ok next-event-id)
  )
)

(define-public (add-inspector (inspector principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-inspectors
      { inspector: inspector }
      { active: true }
    )
    (ok true)
  )
)

(define-public (remove-inspector (inspector principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-inspectors
      { inspector: inspector }
      { active: false }
    )
    (ok true)
  )
)

(define-public (deactivate-product (product-id uint))
  (let
    (
      (product (unwrap! (map-get? products { product-id: product-id }) err-not-found))
    )
    (asserts! (is-eq (get farmer product) tx-sender) err-unauthorized)
    
    (map-set products
      { product-id: product-id }
      (merge product { active: false })
    )
    
    (ok true)
  )
)

(define-public (deactivate-farm (farm-id uint))
  (let
    (

      (farm (unwrap! (map-get? farms { farm-id: farm-id }) err-not-found))
    )

    (asserts! (is-eq (get owner farm) tx-sender) err-unauthorized)
    
    (map-set farms
      { farm-id: farm-id }
      (merge farm { active: false })
    )
    
    (ok true)
  )
)