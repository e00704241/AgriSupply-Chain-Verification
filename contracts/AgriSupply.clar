
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



(define-map product-certifications
  { product-id: uint, certification-type: (string-ascii 30) }
  {
    certifier: principal,
    certification-date: uint,
    expiry-date: uint,
    status: (string-ascii 20),
    certificate-id: (string-ascii 50)
  }
)

(define-map authorized-certifiers 
  { certifier: principal }
  { 
    active: bool,
    organization: (string-ascii 50)
  }
)

(define-public (register-certifier (certifier principal) (organization (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-certifiers
      { certifier: certifier }
      { active: true, organization: organization }
    )
    (ok true)
  )
)

(define-public (add-certification 
    (product-id uint)
    (certification-type (string-ascii 30))
    (expiry-date uint)
    (certificate-id (string-ascii 50)))
  (let
    ((certifier-info (unwrap! (map-get? authorized-certifiers { certifier: tx-sender }) err-unauthorized)))
    (asserts! (get active certifier-info) err-unauthorized)
    (map-set product-certifications
      { product-id: product-id, certification-type: certification-type }
      {
        certifier: tx-sender,
        certification-date: stacks-block-height,
        expiry-date: expiry-date,
        status: "ACTIVE",
        certificate-id: certificate-id
      }
    )
    (ok true)
  )
)



(define-map product-ratings
  { product-id: uint, reviewer: principal }
  {
    rating: uint,
    review: (string-ascii 200),
    timestamp: uint,
    reviewer-type: (string-ascii 20)
  }
)

(define-map product-rating-stats
  { product-id: uint }
  {
    total-ratings: uint,
    average-rating: uint,
    last-updated: uint
  }
)

(define-public (add-product-rating 
    (product-id uint)
    (rating uint)
    (review (string-ascii 200))
    (reviewer-type (string-ascii 20)))
  (let
    (
      (product (unwrap! (map-get? products { product-id: product-id }) err-not-found))
      (current-stats (default-to { total-ratings: u0, average-rating: u0, last-updated: u0 }
        (map-get? product-rating-stats { product-id: product-id })))
    )
    (asserts! (<= rating u5) err-invalid-input)
    (map-set product-ratings
      { product-id: product-id, reviewer: tx-sender }
      {
        rating: rating,
        review: review,
        timestamp: stacks-block-height,
        reviewer-type: reviewer-type
      }
    )
    (map-set product-rating-stats
      { product-id: product-id }
      {
        total-ratings: (+ (get total-ratings current-stats) u1),
        average-rating: (/ (+ (* (get average-rating current-stats) (get total-ratings current-stats)) rating)
                          (+ (get total-ratings current-stats) u1)),
        last-updated: stacks-block-height
      }
    )
    (ok true)
  )
)




(define-public (batch-add-supply-chain-events (events-data (list 50 {
  product-id: uint,
  event-type: (string-ascii 20),
  location: (string-ascii 100),
  temperature: int,
  humidity: int,
  quality-score: uint,
  notes: (string-ascii 200)
})))
  (let
    ((results (map batch-add-single-event events-data)))
    (ok results)
  )
)

(define-private (batch-add-single-event (event-data {
  product-id: uint,
  event-type: (string-ascii 20),
  location: (string-ascii 100),
  temperature: int,
  humidity: int,
  quality-score: uint,
  notes: (string-ascii 200)
}))
  (let
    ((result (add-supply-chain-event
      (get product-id event-data)
      (get event-type event-data)
      (get location event-data)
      (get temperature event-data)
      (get humidity event-data)
      (get quality-score event-data)
      (get notes event-data))))
    (match result
      success { success: true, event-id: success, product-id: (get product-id event-data), error: u0 }
      error { success: false, event-id: u0, product-id: (get product-id event-data), error: error }
    )
  )
)

(define-public (batch-add-certifications (certifications-data (list 20 {
  product-id: uint,
  certification-type: (string-ascii 30),
  expiry-date: uint,
  certificate-id: (string-ascii 50)
})))
  (let
    ((results (map batch-add-single-certification certifications-data)))
    (ok results)
  )
)

(define-private (batch-add-single-certification (cert-data {
  product-id: uint,
  certification-type: (string-ascii 30),
  expiry-date: uint,
  certificate-id: (string-ascii 50)
}))
  (let
    ((result (add-certification
      (get product-id cert-data)
      (get certification-type cert-data)
      (get expiry-date cert-data)
      (get certificate-id cert-data))))
    (match result
      success { success: true, product-id: (get product-id cert-data), error: u0 }
      error { success: false, product-id: (get product-id cert-data), error: error }
    )
  )
)

(define-public (batch-record-harvests (harvest-data (list 30 {
  product-id: uint,
  harvest-date: uint
})))
  (let
    ((results (map batch-record-single-harvest harvest-data)))
    (ok results)
  )
)

(define-private (batch-record-single-harvest (harvest-info {
  product-id: uint,
  harvest-date: uint
}))
  (let
    ((result (record-harvest
      (get product-id harvest-info)
      (get harvest-date harvest-info))))
    (match result
      success { success: true, product-id: (get product-id harvest-info), error: u0 }
      error { success: false, product-id: (get product-id harvest-info), error: error }
    )
  )
)

(define-public (batch-deactivate-products (product-ids (list 50 uint)))
  (let
    ((results (map batch-deactivate-single-product product-ids)))
    (ok results)
  )
)

(define-private (batch-deactivate-single-product (product-id uint))
  (let
    ((result (deactivate-product product-id)))
    (match result
      success { success: true, product-id: product-id, error: u0 }
      error { success: false, product-id: product-id, error: error }
    )
  )
)

(define-read-only (batch-get-products (product-ids (list 50 uint)))
  (map get-product product-ids)
)

(define-read-only (batch-get-farms (farm-ids (list 50 uint)))
  (map get-farm farm-ids)
)

(define-read-only (batch-get-product-ratings (product-ids (list 30 uint)))
  (map get-product-rating-stats product-ids)
)

(define-read-only (get-product-rating-stats (product-id uint))
  (map-get? product-rating-stats { product-id: product-id })
)

(define-map product-ownership
  { product-id: uint }
  {
    current-owner: principal,
    ownership-type: (string-ascii 20),
    last-transfer-date: uint
  }
)

(define-map ownership-transfers
  { product-id: uint, transfer-id: uint }
  {
    from-owner: principal,
    to-owner: principal,
    transfer-date: uint,
    transfer-type: (string-ascii 20),
    price: uint,
    notes: (string-ascii 200),
    status: (string-ascii 20)
  }
)

(define-map product-transfer-count
  { product-id: uint }
  { count: uint }
)

(define-public (initiate-product-transfer
    (product-id uint)
    (to-owner principal)
    (transfer-type (string-ascii 20))
    (price uint)
    (notes (string-ascii 200)))
  (let
    (
      (product (unwrap! (map-get? products { product-id: product-id }) err-not-found))
      (current-ownership (map-get? product-ownership { product-id: product-id }))
      (transfer-count (default-to { count: u0 } (map-get? product-transfer-count { product-id: product-id })))
      (next-transfer-id (get count transfer-count))
    )
    (asserts! 
      (or 
        (is-eq tx-sender (get farmer product))
        (match current-ownership
          some-ownership (is-eq tx-sender (get current-owner some-ownership))
          false
        )
      )
      err-unauthorized
    )
    (asserts! (not (is-eq tx-sender to-owner)) err-invalid-input)
    
    (map-set ownership-transfers
      { product-id: product-id, transfer-id: next-transfer-id }
      {
        from-owner: tx-sender,
        to-owner: to-owner,
        transfer-date: stacks-block-height,
        transfer-type: transfer-type,
        price: price,
        notes: notes,
        status: "PENDING"
      }
    )
    
    (map-set product-transfer-count
      { product-id: product-id }
      { count: (+ next-transfer-id u1) }
    )
    
    (ok next-transfer-id)
  )
)

(define-public (complete-product-transfer
    (product-id uint)
    (transfer-id uint))
  (let
    (
      (transfer (unwrap! (map-get? ownership-transfers { product-id: product-id, transfer-id: transfer-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get to-owner transfer)) err-unauthorized)
    (asserts! (is-eq (get status transfer) "PENDING") err-invalid-state)
    
    (map-set ownership-transfers
      { product-id: product-id, transfer-id: transfer-id }
      (merge transfer { status: "COMPLETED" })
    )
    
    (map-set product-ownership
      { product-id: product-id }
      {
        current-owner: (get to-owner transfer),
        ownership-type: (get transfer-type transfer),
        last-transfer-date: stacks-block-height
      }
    )
    
    (ok true)
  )
)

(define-public (cancel-product-transfer
    (product-id uint)
    (transfer-id uint))
  (let
    (
      (transfer (unwrap! (map-get? ownership-transfers { product-id: product-id, transfer-id: transfer-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get from-owner transfer)) err-unauthorized)
    (asserts! (is-eq (get status transfer) "PENDING") err-invalid-state)
    
    (map-set ownership-transfers
      { product-id: product-id, transfer-id: transfer-id }
      (merge transfer { status: "CANCELLED" })
    )
    
    (ok true)
  )
)

(define-read-only (get-product-ownership (product-id uint))
  (map-get? product-ownership { product-id: product-id })
)

(define-read-only (get-ownership-transfer (product-id uint) (transfer-id uint))
  (map-get? ownership-transfers { product-id: product-id, transfer-id: transfer-id })
)

(define-read-only (get-product-transfer-count (product-id uint))
  (default-to { count: u0 } (map-get? product-transfer-count { product-id: product-id }))
)

(define-read-only (get-current-product-owner (product-id uint))
  (let
    (
      (product (map-get? products { product-id: product-id }))
      (ownership (map-get? product-ownership { product-id: product-id }))
    )
    (match ownership
      some-ownership (some (get current-owner some-ownership))
      (match product
        some-product (some (get farmer some-product))
        none
      )
    )
  )
)

;; Quality Assurance Laboratory Testing System
;; Data structures for laboratory certification and test management

(define-map certified-laboratories
  { lab-id: uint }
  {
    name: (string-ascii 100),
    location: (string-ascii 150),
    operator: principal,
    accreditation: (string-ascii 50),
    registration-date: uint,
    active: bool
  }
)

(define-map test-type-definitions
  { test-type-id: uint }
  {
    name: (string-ascii 80),
    category: (string-ascii 30),
    unit: (string-ascii 20),
    min-value: int,
    max-value: int,
    lab-id: uint
  }
)

(define-map product-test-results
  { product-id: uint, test-id: uint }
  {
    lab-id: uint,
    test-type-id: uint,
    result-value: int,
    result-status: (string-ascii 20),
    test-date: uint,
    technician: principal,
    sample-batch: (string-ascii 50),
    notes: (string-ascii 300)
  }
)

(define-map product-test-count
  { product-id: uint }
  { count: uint }
)

(define-map lab-authorization
  { operator: principal }
  { 
    authorized: bool,
    lab-id: uint
  }
)

(define-data-var next-lab-id uint u1)
(define-data-var next-test-type-id uint u1)

;; Laboratory registration and management functions

(define-public (register-laboratory 
    (name (string-ascii 100))
    (location (string-ascii 150))
    (accreditation (string-ascii 50)))
  (let
    (
      (lab-id (var-get next-lab-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set certified-laboratories
      { lab-id: lab-id }
      {
        name: name,
        location: location,
        operator: tx-sender,
        accreditation: accreditation,
        registration-date: stacks-block-height,
        active: true
      }
    )
    
    (map-set lab-authorization
      { operator: tx-sender }
      { authorized: true, lab-id: lab-id }
    )
    
    (var-set next-lab-id (+ lab-id u1))
    (ok lab-id)
  )
)

(define-public (authorize-lab-operator 
    (operator principal)
    (lab-id uint))
  (let
    (
      (lab (unwrap! (map-get? certified-laboratories { lab-id: lab-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (get active lab) err-invalid-state)
    
    (map-set lab-authorization
      { operator: operator }
      { authorized: true, lab-id: lab-id }
    )
    
    (ok true)
  )
)

(define-public (define-test-type
    (name (string-ascii 80))
    (category (string-ascii 30))
    (unit (string-ascii 20))
    (min-value int)
    (max-value int))
  (let
    (
      (test-type-id (var-get next-test-type-id))
      (auth (unwrap! (map-get? lab-authorization { operator: tx-sender }) err-unauthorized))
    )
    (asserts! (get authorized auth) err-unauthorized)
    (asserts! (< min-value max-value) err-invalid-input)
    
    (map-set test-type-definitions
      { test-type-id: test-type-id }
      {
        name: name,
        category: category,
        unit: unit,
        min-value: min-value,
        max-value: max-value,
        lab-id: (get lab-id auth)
      }
    )
    
    (var-set next-test-type-id (+ test-type-id u1))
    (ok test-type-id)
  )
)

;; Test result recording and validation

(define-public (record-test-result
    (product-id uint)
    (test-type-id uint)
    (result-value int)
    (sample-batch (string-ascii 50))
    (notes (string-ascii 300)))
  (let
    (
      (product (unwrap! (map-get? products { product-id: product-id }) err-not-found))
      (test-type (unwrap! (map-get? test-type-definitions { test-type-id: test-type-id }) err-not-found))
      (auth (unwrap! (map-get? lab-authorization { operator: tx-sender }) err-unauthorized))
      (test-count (default-to { count: u0 } (map-get? product-test-count { product-id: product-id })))
      (next-test-id (get count test-count))
      (result-status (if (and (>= result-value (get min-value test-type)) 
                             (<= result-value (get max-value test-type)))
                        "PASS"
                        "FAIL"))
    )
    (asserts! (get authorized auth) err-unauthorized)
    (asserts! (is-eq (get lab-id auth) (get lab-id test-type)) err-unauthorized)
    
    (map-set product-test-results
      { product-id: product-id, test-id: next-test-id }
      {
        lab-id: (get lab-id auth),
        test-type-id: test-type-id,
        result-value: result-value,
        result-status: result-status,
        test-date: stacks-block-height,
        technician: tx-sender,
        sample-batch: sample-batch,
        notes: notes
      }
    )
    
    (map-set product-test-count
      { product-id: product-id }
      { count: (+ next-test-id u1) }
    )
    
    (ok next-test-id)
  )
)

(define-public (update-test-result-status
    (product-id uint)
    (test-id uint)
    (new-status (string-ascii 20)))
  (let
    (
      (test-result (unwrap! (map-get? product-test-results { product-id: product-id, test-id: test-id }) err-not-found))
      (auth (unwrap! (map-get? lab-authorization { operator: tx-sender }) err-unauthorized))
    )
    (asserts! (get authorized auth) err-unauthorized)
    (asserts! (is-eq (get lab-id auth) (get lab-id test-result)) err-unauthorized)
    
    (map-set product-test-results
      { product-id: product-id, test-id: test-id }
      (merge test-result { result-status: new-status })
    )
    
    (ok true)
  )
)

;; Read-only functions for querying test data

(define-read-only (get-laboratory (lab-id uint))
  (map-get? certified-laboratories { lab-id: lab-id })
)

(define-read-only (get-test-type-definition (test-type-id uint))
  (map-get? test-type-definitions { test-type-id: test-type-id })
)

(define-read-only (get-test-result (product-id uint) (test-id uint))
  (map-get? product-test-results { product-id: product-id, test-id: test-id })
)

(define-read-only (get-product-test-count (product-id uint))
  (default-to { count: u0 } (map-get? product-test-count { product-id: product-id }))
)

(define-read-only (is-lab-operator-authorized (operator principal))
  (default-to { authorized: false, lab-id: u0 } (map-get? lab-authorization { operator: operator }))
)

(define-read-only (get-product-test-summary (product-id uint))
  (let
    (
      (test-count (get-product-test-count product-id))
    )
    { 
      total-tests: (get count test-count),
      product-exists: (is-some (map-get? products { product-id: product-id }))
    }
  )
)



