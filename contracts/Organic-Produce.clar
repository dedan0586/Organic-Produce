;; title: Organic-Produce
;; version: 1.0
;; summary: A decentralized marketplace for organic produce
;; description: Allows farmers to list organic products with authenticity guarantees and buyers to verify product origin on-chain

;; ----- Constants -----
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PRODUCT-EXISTS (err u101))
(define-constant ERR-PRODUCT-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-ALREADY-CERTIFIED (err u104))
(define-constant ERR-NOT-CERTIFIED (err u105))
(define-constant ERR-INVALID-RATING (err u106))
(define-constant ERR-ALREADY-PURCHASED (err u107))
(define-constant ERR-NOT-SELLER (err u108))
(define-constant ERR-NOT-BUYER (err u109))
(define-constant ERR-DISPUTE-EXISTS (err u110))

;; ----- Data Variables -----
(define-data-var next-product-id uint u1)
(define-data-var next-certificate-id uint u1)
(define-data-var platform-fee-percent uint u2) ;; 2% platform fee

;; ----- Data Maps -----
;; Product information
(define-map products
  { product-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    price: uint,
    quantity: uint,
    seller: principal,
    is-certified: bool,
    certificate-id: (optional uint),
    is-available: bool,
    harvest-date: uint,
    farm-location: (string-ascii 100)
  }
)

;; Authenticity certificates (NFTs)
(define-map certificates
  { certificate-id: uint }
  {
    product-id: uint,
    issuer: principal,
    issue-date: uint,
    expiry-date: uint,
    certification-standard: (string-ascii 50),
    verification-details: (string-ascii 200)
  }
)

;; Seller reputation
(define-map seller-reputation
  { seller: principal }
  {
    total-ratings: uint,
    rating-sum: uint,
    products-sold: uint
  }
)

;; Purchases
(define-map purchases
  { product-id: uint, buyer: principal }
  {
    purchase-date: uint,
    status: (string-ascii 20), ;; "pending", "completed", "disputed", "refunded"
    rating: (optional uint),
    review: (optional (string-ascii 200))
  }
)

;; Disputes
(define-map disputes
  { product-id: uint, buyer: principal }
  {
    reason: (string-ascii 200),
    status: (string-ascii 20), ;; "open", "resolved-buyer", "resolved-seller"
    created-at: uint
  }
)

;; ----- Public Functions -----

;; List a new organic product
(define-public (list-product 
    (name (string-ascii 50)) 
    (description (string-ascii 200)) 
    (price uint) 
    (quantity uint)
    (harvest-date uint)
    (farm-location (string-ascii 100)))
  (let ((product-id (var-get next-product-id)))
    (map-insert products
      { product-id: product-id }
      {
        name: name,
        description: description,
        price: price,
        quantity: quantity,
        seller: tx-sender,
        is-certified: false,
        certificate-id: none,
        is-available: true,
        harvest-date: harvest-date,
        farm-location: farm-location
      }
    )
    (var-set next-product-id (+ product-id u1))
    (ok product-id)
  )
)

;; Issue authenticity certificate for a product
(define-public (issue-certificate 
    (product-id uint) 
    (expiry-date uint)
    (certification-standard (string-ascii 50))
    (verification-details (string-ascii 200)))
  (let (
    (product (unwrap! (map-get? products { product-id: product-id }) (err ERR-PRODUCT-NOT-FOUND)))
    (certificate-id (var-get next-certificate-id))
  )
    ;; Only the contract owner can issue certificates
    (asserts! (is-eq tx-sender CONTRACT-OWNER) (err ERR-NOT-AUTHORIZED))
    ;; Check if product is already certified
    (asserts! (not (get is-certified product)) (err ERR-ALREADY-CERTIFIED))
    
    ;; Create certificate
    (map-insert certificates
      { certificate-id: certificate-id }
      {
        product-id: product-id,
        issuer: tx-sender,
        issue-date: stacks-block-height,
        expiry-date: expiry-date,
        certification-standard: certification-standard,
        verification-details: verification-details
      }
    )
    
    ;; Update product with certificate info
    (map-set products
      { product-id: product-id }
      (merge product {
        is-certified: true,
        certificate-id: (some certificate-id)
      })
    )
    
    (var-set next-certificate-id (+ certificate-id u1))
    (ok certificate-id)
  )
)

;; Purchase a product
(define-public (purchase-product (product-id uint))
  (let (
    (product (unwrap! (map-get? products { product-id: product-id }) (err ERR-PRODUCT-NOT-FOUND)))
    (seller (get seller product))
    (price (get price product))
    (platform-fee (/ (* price (var-get platform-fee-percent)) u100))
    (seller-amount (- price platform-fee))
  )
    ;; Check if product is available
    (asserts! (get is-available product) (err ERR-PRODUCT-NOT-FOUND))
    ;; Check if product is certified
    (asserts! (get is-certified product) (err ERR-NOT-CERTIFIED))
    ;; Check if buyer has enough funds
    (asserts! (>= (stx-get-balance tx-sender) price) (err ERR-INSUFFICIENT-FUNDS))
    ;; Check if buyer is not the seller
    (asserts! (not (is-eq tx-sender seller)) (err ERR-NOT-AUTHORIZED))
    
    ;; Transfer funds
    (unwrap! (stx-transfer? seller-amount tx-sender seller) (err ERR-INSUFFICIENT-FUNDS))
    (unwrap! (stx-transfer? platform-fee tx-sender CONTRACT-OWNER) (err ERR-INSUFFICIENT-FUNDS))
    
    ;; Update product availability
    (map-set products
      { product-id: product-id }
      (merge product {
        quantity: (- (get quantity product) u1),
        is-available: (> (- (get quantity product) u1) u0)
      })
    )
    
    ;; Record purchase
    (map-insert purchases
      { product-id: product-id, buyer: tx-sender }
      {
        purchase-date: stacks-block-height,
        status: "completed",
        rating: none,
        review: none
      }
    )
    
    ;; Update seller reputation
    (let ((current-reputation (default-to 
            { total-ratings: u0, rating-sum: u0, products-sold: u0 }
            (map-get? seller-reputation { seller: seller }))))
      (map-set seller-reputation
        { seller: seller }
        (merge current-reputation {
          products-sold: (+ (get products-sold current-reputation) u1)
        })
      )
    )
    
    (ok true)
  )
)

;; Rate a product after purchase
(define-public (rate-product (product-id uint) (rating uint) (review (optional (string-ascii 200))))
  (let (
    (purchase (unwrap! (map-get? purchases { product-id: product-id, buyer: tx-sender }) (err ERR-NOT-BUYER)))
    (product (unwrap! (map-get? products { product-id: product-id }) (err ERR-PRODUCT-NOT-FOUND)))
    (seller (get seller product))
  )
    ;; Check if rating is valid (1-5)
    (asserts! (and (>= rating u1) (<= rating u5)) (err ERR-INVALID-RATING))
    
    ;; Update purchase with rating
    (map-set purchases
      { product-id: product-id, buyer: tx-sender }
      (merge purchase {
        rating: (some rating),
        review: review
      })
    )
    
    ;; Update seller reputation
    (let ((current-reputation (default-to 
            { total-ratings: u0, rating-sum: u0, products-sold: u0 }
            (map-get? seller-reputation { seller: seller }))))
      (map-set seller-reputation
        { seller: seller }
        {
          total-ratings: (+ (get total-ratings current-reputation) u1),
          rating-sum: (+ (get rating-sum current-reputation) rating),
          products-sold: (get products-sold current-reputation)
        }
      )
    )
    
    (ok true)
  )
)

;; Create a dispute for a purchase
(define-public (create-dispute (product-id uint) (reason (string-ascii 200)))
  (let (
    (purchase (unwrap! (map-get? purchases { product-id: product-id, buyer: tx-sender }) (err ERR-NOT-BUYER)))
  )
    ;; Check if dispute already exists
    (asserts! (is-none (map-get? disputes { product-id: product-id, buyer: tx-sender })) (err ERR-DISPUTE-EXISTS))
    
    ;; Create dispute
    (map-insert disputes
      { product-id: product-id, buyer: tx-sender }
      {
        reason: reason,
        status: "open",
        created-at: stacks-block-height
      }
    )
    
    ;; Update purchase status
    (map-set purchases
      { product-id: product-id, buyer: tx-sender }
      (merge purchase { status: "disputed" })
    )
    
    (ok true)
  )
)

;; Resolve a dispute (only contract owner)
(define-public (resolve-dispute (product-id uint) (buyer principal) (in-favor-of-buyer bool))
  (let (
    (dispute (unwrap! (map-get? disputes { product-id: product-id, buyer: buyer }) (err ERR-PRODUCT-NOT-FOUND)))
    (purchase (unwrap! (map-get? purchases { product-id: product-id, buyer: buyer }) (err ERR-PRODUCT-NOT-FOUND)))
    (product (unwrap! (map-get? products { product-id: product-id }) (err ERR-PRODUCT-NOT-FOUND)))
    (seller (get seller product))
  )
    ;; Only contract owner can resolve disputes
    (asserts! (is-eq tx-sender CONTRACT-OWNER) (err ERR-NOT-AUTHORIZED))
    
    ;; Update dispute status
    (map-set disputes
      { product-id: product-id, buyer: buyer }
      (merge dispute {
        status: (if in-favor-of-buyer "resolved-buyer" "resolved-seller")
      })
    )
    
    ;; If resolved in favor of buyer, refund
    (if in-favor-of-buyer
      (begin
        (unwrap! (stx-transfer? (get price product) CONTRACT-OWNER buyer) (err ERR-INSUFFICIENT-FUNDS))
        (map-set purchases
          { product-id: product-id, buyer: buyer }
          (merge purchase { status: "refunded" })
        )
      )
      (map-set purchases
        { product-id: product-id, buyer: buyer }
        (merge purchase { status: "completed" })
      )
    )
    
    (ok true)
  )
)

;; Update platform fee (only contract owner)
(define-public (update-platform-fee (new-fee-percent uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) (err ERR-NOT-AUTHORIZED))
    ;; (asserts! (<= new-fee-percent u10) (err u111)) ;; Max 10% fee
    (var-set platform-fee-percent new-fee-percent)
    (ok true)
  )
)

;; ----- Read-only Functions -----

;; Get product details
(define-read-only (get-product (product-id uint))
  (map-get? products { product-id: product-id })
)

;; Get certificate details
(define-read-only (get-certificate (certificate-id uint))
  (map-get? certificates { certificate-id: certificate-id })
)

;; Get seller reputation
(define-read-only (get-seller-reputation (seller principal))
  (let ((reputation (default-to 
          { total-ratings: u0, rating-sum: u0, products-sold: u0 }
          (map-get? seller-reputation { seller: seller }))))
    (if (> (get total-ratings reputation) u0)
      (some {
        average-rating: (/ (* (get rating-sum reputation) u100) (get total-ratings reputation)),
        total-ratings: (get total-ratings reputation),
        products-sold: (get products-sold reputation)
      })
      none
    )
  )
)

;; Get purchase details
(define-read-only (get-purchase (product-id uint) (buyer principal))
  (map-get? purchases { product-id: product-id, buyer: buyer })
)

;; Get dispute details
(define-read-only (get-dispute (product-id uint) (buyer principal))
  (map-get? disputes { product-id: product-id, buyer: buyer })
)

;; Check if a product is certified
(define-read-only (is-product-certified (product-id uint))
  (match (map-get? products { product-id: product-id })
    product (get is-certified product)
    false
  )
)

;; Get platform fee percentage
(define-read-only (get-platform-fee)
  (var-get platform-fee-percent)
)



(define-map subscriptions
  { subscriber: principal, seller: principal }
  {
    start-block: uint,
    duration-blocks: uint,
    payment-amount: uint,
    is-active: bool,
    last-payment: uint
  }
)

(define-public (create-subscription 
    (seller principal)
    (duration-blocks uint)
    (payment-amount uint))
    
  (let ((current-block stacks-block-height))
    (asserts! (> payment-amount u0) (err u200))
    (asserts! (> duration-blocks u0) (err u201))
    
    (map-set subscriptions
      { subscriber: tx-sender, seller: seller }
      {
        start-block: current-block,
        duration-blocks: duration-blocks,
        payment-amount: payment-amount,
        is-active: true,
        last-payment: current-block
      }
    )
    
    (try! (stx-transfer? payment-amount tx-sender seller))
    (ok true))
)

(define-public (process-subscription-payment 
    (subscriber principal)
    (seller principal))
    
  (let (
    (sub (unwrap! (map-get? subscriptions { subscriber: subscriber, seller: seller }) (err u202)))
    (current-block stacks-block-height)
    (next-payment (+ (get last-payment sub) (get duration-blocks sub)))
  )
    (asserts! (get is-active sub) (err u203))
    (asserts! (>= current-block next-payment) (err u204))
    
    (try! (stx-transfer? (get payment-amount sub) subscriber seller))
    
    (map-set subscriptions
      { subscriber: subscriber, seller: seller }
      (merge sub { last-payment: current-block })
    )
    (ok true))
)

(define-public (cancel-subscription (seller principal))
  (let ((sub (unwrap! (map-get? subscriptions { subscriber: tx-sender, seller: seller }) (err u202))))
    (map-set subscriptions
      { subscriber: tx-sender, seller: seller }
      (merge sub { is-active: false })
    )
    (ok true))
)


(define-data-var next-tracking-id uint u1)

(define-map supply-chain-events
  { tracking-id: uint }
  {
    product-id: uint,
    event-type: (string-ascii 20),
    location: (string-ascii 100),
    timestamp: uint,
    handler: principal,
    temperature: (optional uint),
    notes: (optional (string-ascii 200)),
    previous-tracking-id: (optional uint)
  }
)

(define-map product-tracking
  { product-id: uint }
  {
    current-tracking-id: (optional uint),
    total-events: uint,
    last-updated: uint
  }
)

(define-map authorized-handlers
  { handler: principal }
  { is-authorized: bool }
)

(define-public (authorize-handler (handler principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) (err ERR-NOT-AUTHORIZED))
    (map-set authorized-handlers
      { handler: handler }
      { is-authorized: true }
    )
    (ok true)
  )
)

(define-public (revoke-handler (handler principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) (err ERR-NOT-AUTHORIZED))
    (map-set authorized-handlers
      { handler: handler }
      { is-authorized: false }
    )
    (ok true)
  )
)

(define-public (add-supply-chain-event
    (product-id uint)
    (event-type (string-ascii 20))
    (location (string-ascii 100))
    (temperature (optional uint))
    (notes (optional (string-ascii 200))))
  (let (
    (product (unwrap! (map-get? products { product-id: product-id }) (err ERR-PRODUCT-NOT-FOUND)))
    (tracking-id (var-get next-tracking-id))
    (current-tracking (default-to 
      { current-tracking-id: none, total-events: u0, last-updated: u0 }
      (map-get? product-tracking { product-id: product-id })))
    (handler-auth (default-to 
      { is-authorized: false }
      (map-get? authorized-handlers { handler: tx-sender })))
  )
    (asserts! (or 
      (is-eq tx-sender (get seller product))
      (is-eq tx-sender CONTRACT-OWNER)
      (get is-authorized handler-auth)
    ) (err ERR-NOT-AUTHORIZED))
    
    (map-insert supply-chain-events
      { tracking-id: tracking-id }
      {
        product-id: product-id,
        event-type: event-type,
        location: location,
        timestamp: stacks-block-height,
        handler: tx-sender,
        temperature: temperature,
        notes: notes,
        previous-tracking-id: (get current-tracking-id current-tracking)
      }
    )
    
    (map-set product-tracking
      { product-id: product-id }
      {
        current-tracking-id: (some tracking-id),
        total-events: (+ (get total-events current-tracking) u1),
        last-updated: stacks-block-height
      }
    )
    
    (var-set next-tracking-id (+ tracking-id u1))
    (ok tracking-id)
  )
)

(define-read-only (get-supply-chain-event (tracking-id uint))
  (map-get? supply-chain-events { tracking-id: tracking-id })
)

(define-read-only (get-product-tracking-summary (product-id uint))
  (map-get? product-tracking { product-id: product-id })
)

(define-read-only (get-latest-tracking-event (product-id uint))
  (let ((tracking-summary (map-get? product-tracking { product-id: product-id })))
    (match tracking-summary
      summary (match (get current-tracking-id summary)
        tracking-id (map-get? supply-chain-events { tracking-id: tracking-id })
        none
      )
      none
    )
  )
)

(define-read-only (is-handler-authorized (handler principal))
  (let ((handler-data (map-get? authorized-handlers { handler: handler })))
    (match handler-data
      data (get is-authorized data)
      false
    )
  )
)

;; ----- CSA (Community-Supported Agriculture) Features -----

;; CSA error constants
(define-constant ERR-CSA-NOT-FOUND (err u400))
(define-constant ERR-CSA-ALREADY-EXISTS (err u401))
(define-constant ERR-CSA-SHARE-NOT-FOUND (err u402))
(define-constant ERR-CSA-SEASON-ENDED (err u403))
(define-constant ERR-CSA-SEASON-NOT-STARTED (err u404))
(define-constant ERR-CSA-INSUFFICIENT-SHARES (err u405))
(define-constant ERR-CSA-ALREADY-MEMBER (err u406))
(define-constant ERR-CSA-NOT-MEMBER (err u407))
(define-constant ERR-CSA-DISTRIBUTION-EXISTS (err u408))
(define-constant ERR-CSA-INVALID-WEEK (err u409))

;; CSA data variables
(define-data-var next-csa-id uint u1)
(define-data-var next-distribution-id uint u1)

;; CSA program definition
(define-map csa-programs
  { csa-id: uint }
  {
    farmer: principal,
    program-name: (string-ascii 50),
    season-start-block: uint,
    season-end-block: uint,
    total-shares: uint,
    available-shares: uint,
    share-price: uint,
    distribution-frequency: uint, ;; weeks between distributions
    farm-description: (string-ascii 200),
    pickup-location: (string-ascii 100),
    is-active: bool
  }
)

;; CSA member shares
(define-map csa-memberships
  { csa-id: uint, member: principal }
  {
    shares-owned: uint,
    join-date: uint,
    total-paid: uint,
    credits-balance: uint,
    is-active: bool,
    delivery-preference: (string-ascii 20) ;; "pickup" or "delivery"
  }
)

;; Weekly distribution tracking
(define-map csa-distributions
  { distribution-id: uint }
  {
    csa-id: uint,
    week-number: uint,
    distribution-date: uint,
    box-contents: (string-ascii 300),
    estimated-value: uint,
    total-members-served: uint,
    distribution-notes: (optional (string-ascii 200))
  }
)

;; Member distribution pickup/delivery tracking
(define-map member-distributions
  { distribution-id: uint, member: principal }
  {
    pickup-status: (string-ascii 20), ;; "pending", "picked-up", "delivered", "missed"
    pickup-date: (optional uint),
    satisfaction-rating: (optional uint),
    feedback: (optional (string-ascii 150))
  }
)

;; Crop voting for member input
(define-map crop-votes
  { csa-id: uint, member: principal, crop-name: (string-ascii 30) }
  {
    vote-weight: uint,
    vote-date: uint
  }
)

;; Crop voting summary
(define-map crop-vote-summary
  { csa-id: uint, crop-name: (string-ascii 30) }
  {
    total-votes: uint,
    total-weight: uint
  }
)

;; Create a new CSA program
(define-public (create-csa-program
    (program-name (string-ascii 50))
    (season-start-block uint)
    (season-end-block uint)
    (total-shares uint)
    (share-price uint)
    (distribution-frequency uint)
    (farm-description (string-ascii 200))
    (pickup-location (string-ascii 100)))
  (let ((csa-id (var-get next-csa-id)))
    ;; Validate inputs
    (asserts! (> season-end-block season-start-block) (err u410))
    (asserts! (> total-shares u0) (err u411))
    (asserts! (> share-price u0) (err u412))
    (asserts! (> distribution-frequency u0) (err u413))
    
    ;; Create CSA program
    (map-insert csa-programs
      { csa-id: csa-id }
      {
        farmer: tx-sender,
        program-name: program-name,
        season-start-block: season-start-block,
        season-end-block: season-end-block,
        total-shares: total-shares,
        available-shares: total-shares,
        share-price: share-price,
        distribution-frequency: distribution-frequency,
        farm-description: farm-description,
        pickup-location: pickup-location,
        is-active: true
      }
    )
    
    (var-set next-csa-id (+ csa-id u1))
    (ok csa-id)
  )
)

;; Join a CSA program by purchasing shares
(define-public (join-csa-program
    (csa-id uint)
    (shares-requested uint)
    (delivery-preference (string-ascii 20)))
  (let (
    (csa-program (unwrap! (map-get? csa-programs { csa-id: csa-id }) ERR-CSA-NOT-FOUND))
    (total-cost (* shares-requested (get share-price csa-program)))
    (existing-member (map-get? csa-memberships { csa-id: csa-id, member: tx-sender }))
  )
    ;; Validate membership and availability
    (asserts! (is-none existing-member) ERR-CSA-ALREADY-MEMBER)
    (asserts! (get is-active csa-program) ERR-CSA-NOT-FOUND)
    (asserts! (<= stacks-block-height (get season-start-block csa-program)) ERR-CSA-SEASON-NOT-STARTED)
    (asserts! (>= (get available-shares csa-program) shares-requested) ERR-CSA-INSUFFICIENT-SHARES)
    (asserts! (>= (stx-get-balance tx-sender) total-cost) ERR-INSUFFICIENT-FUNDS)
    
    ;; Transfer payment to farmer
    (try! (stx-transfer? total-cost tx-sender (get farmer csa-program)))
    
    ;; Create membership record
    (map-insert csa-memberships
      { csa-id: csa-id, member: tx-sender }
      {
        shares-owned: shares-requested,
        join-date: stacks-block-height,
        total-paid: total-cost,
        credits-balance: u0,
        is-active: true,
        delivery-preference: delivery-preference
      }
    )
    
    ;; Update available shares
    (map-set csa-programs
      { csa-id: csa-id }
      (merge csa-program {
        available-shares: (- (get available-shares csa-program) shares-requested)
      })
    )
    
    (ok true)
  )
)

;; Farmer records weekly distribution
(define-public (record-distribution
    (csa-id uint)
    (week-number uint)
    (box-contents (string-ascii 300))
    (estimated-value uint)
    (distribution-notes (optional (string-ascii 200))))
  (let (
    (csa-program (unwrap! (map-get? csa-programs { csa-id: csa-id }) ERR-CSA-NOT-FOUND))
    (distribution-id (var-get next-distribution-id))
  )
    ;; Only farmer can record distributions
    (asserts! (is-eq tx-sender (get farmer csa-program)) ERR-NOT-AUTHORIZED)
    ;; Check if season is active
    (asserts! (and 
      (>= stacks-block-height (get season-start-block csa-program))
      (<= stacks-block-height (get season-end-block csa-program))
    ) ERR-CSA-SEASON-ENDED)
    
    ;; Record distribution
    (map-insert csa-distributions
      { distribution-id: distribution-id }
      {
        csa-id: csa-id,
        week-number: week-number,
        distribution-date: stacks-block-height,
        box-contents: box-contents,
        estimated-value: estimated-value,
        total-members-served: u0,
        distribution-notes: distribution-notes
      }
    )
    
    (var-set next-distribution-id (+ distribution-id u1))
    (ok distribution-id)
  )
)

;; Member confirms pickup or delivery
(define-public (confirm-pickup
    (distribution-id uint)
    (satisfaction-rating (optional uint))
    (feedback (optional (string-ascii 150))))
  (let (
    (distribution (unwrap! (map-get? csa-distributions { distribution-id: distribution-id }) ERR-CSA-NOT-FOUND))
    (membership (unwrap! (map-get? csa-memberships { csa-id: (get csa-id distribution), member: tx-sender }) ERR-CSA-NOT-MEMBER))
  )
    ;; Validate rating if provided
    (if (is-some satisfaction-rating)
      (asserts! (and 
        (>= (unwrap-panic satisfaction-rating) u1) 
        (<= (unwrap-panic satisfaction-rating) u5)
      ) ERR-INVALID-RATING)
      true
    )
    
    ;; Record member pickup
    (map-insert member-distributions
      { distribution-id: distribution-id, member: tx-sender }
      {
        pickup-status: "picked-up",
        pickup-date: (some stacks-block-height),
        satisfaction-rating: satisfaction-rating,
        feedback: feedback
      }
    )
    
    ;; Update distribution counter
    (map-set csa-distributions
      { distribution-id: distribution-id }
      (merge distribution {
        total-members-served: (+ (get total-members-served distribution) u1)
      })
    )
    
    (ok true)
  )
)

;; Member votes for crops they want grown
(define-public (vote-for-crop
    (csa-id uint)
    (crop-name (string-ascii 30)))
  (let (
    (membership (unwrap! (map-get? csa-memberships { csa-id: csa-id, member: tx-sender }) ERR-CSA-NOT-MEMBER))
    (vote-weight (get shares-owned membership))
    (existing-vote (map-get? crop-votes { csa-id: csa-id, member: tx-sender, crop-name: crop-name }))
    (current-summary (default-to 
      { total-votes: u0, total-weight: u0 }
      (map-get? crop-vote-summary { csa-id: csa-id, crop-name: crop-name })))
  )
    ;; Check if member is active
    (asserts! (get is-active membership) ERR-CSA-NOT-MEMBER)
    
    ;; If no existing vote, create new one
    (if (is-none existing-vote)
      (begin
        (map-insert crop-votes
          { csa-id: csa-id, member: tx-sender, crop-name: crop-name }
          {
            vote-weight: vote-weight,
            vote-date: stacks-block-height
          }
        )
        
        ;; Update vote summary
        (map-set crop-vote-summary
          { csa-id: csa-id, crop-name: crop-name }
          {
            total-votes: (+ (get total-votes current-summary) u1),
            total-weight: (+ (get total-weight current-summary) vote-weight)
          }
        )
        (ok true)
      )
      (ok false) ;; Already voted
    )
  )
)

;; Add credits to member account for missed distributions
(define-public (add-member-credits
    (csa-id uint)
    (member principal)
    (credit-amount uint))
  (let (
    (csa-program (unwrap! (map-get? csa-programs { csa-id: csa-id }) ERR-CSA-NOT-FOUND))
    (membership (unwrap! (map-get? csa-memberships { csa-id: csa-id, member: member }) ERR-CSA-NOT-MEMBER))
  )
    ;; Only farmer can add credits
    (asserts! (is-eq tx-sender (get farmer csa-program)) ERR-NOT-AUTHORIZED)
    
    ;; Add credits to member balance
    (map-set csa-memberships
      { csa-id: csa-id, member: member }
      (merge membership {
        credits-balance: (+ (get credits-balance membership) credit-amount)
      })
    )
    
    (ok true)
  )
)

;; Read-only functions for CSA features
(define-read-only (get-csa-program (csa-id uint))
  (map-get? csa-programs { csa-id: csa-id })
)

(define-read-only (get-csa-membership (csa-id uint) (member principal))
  (map-get? csa-memberships { csa-id: csa-id, member: member })
)

(define-read-only (get-distribution (distribution-id uint))
  (map-get? csa-distributions { distribution-id: distribution-id })
)

(define-read-only (get-member-distribution (distribution-id uint) (member principal))
  (map-get? member-distributions { distribution-id: distribution-id, member: member })
)

(define-read-only (get-crop-vote-summary (csa-id uint) (crop-name (string-ascii 30)))
  (map-get? crop-vote-summary { csa-id: csa-id, crop-name: crop-name })
)


(define-data-var next-season-id uint u1)

(define-map seasonal-produce-types
  { season-id: uint }
  {
    name: (string-ascii 50),
    season-start-month: uint,
    season-end-month: uint,
    typical-harvest-duration: uint,
    created-by: principal
  }
)

(define-map seasonal-interest
  { season-id: uint, buyer: principal }
  {
    max-price: uint,
    desired-quantity: uint,
    registered-at: uint,
    is-active: bool
  }
)

(define-map seasonal-demand-summary
  { season-id: uint }
  {
    total-interested-buyers: uint,
    total-demand-quantity: uint,
    average-max-price: uint,
    last-updated: uint
  }
)

(define-map seasonal-pre-orders
  { season-id: uint, buyer: principal, farmer: principal }
  {
    quantity: uint,
    agreed-price: uint,
    delivery-month: uint,
    status: (string-ascii 20),
    created-at: uint
  }
)

(define-public (register-seasonal-produce
    (name (string-ascii 50))
    (season-start-month uint)
    (season-end-month uint)
    (typical-harvest-duration uint))
  (let ((season-id (var-get next-season-id)))
    (asserts! (and (<= season-start-month u12) (>= season-start-month u1)) (err u300))
    (asserts! (and (<= season-end-month u12) (>= season-end-month u1)) (err u301))
    (asserts! (> typical-harvest-duration u0) (err u302))
    
    (map-insert seasonal-produce-types
      { season-id: season-id }
      {
        name: name,
        season-start-month: season-start-month,
        season-end-month: season-end-month,
        typical-harvest-duration: typical-harvest-duration,
        created-by: tx-sender
      }
    )
    
    (map-insert seasonal-demand-summary
      { season-id: season-id }
      {
        total-interested-buyers: u0,
        total-demand-quantity: u0,
        average-max-price: u0,
        last-updated: stacks-block-height
      }
    )
    
    (var-set next-season-id (+ season-id u1))
    (ok season-id)
  )
)

(define-public (register-seasonal-interest
    (season-id uint)
    (max-price uint)
    (desired-quantity uint))
  (let (
    (season-type (unwrap! (map-get? seasonal-produce-types { season-id: season-id }) (err u303)))
    (current-summary (unwrap! (map-get? seasonal-demand-summary { season-id: season-id }) (err u304)))
    (existing-interest (map-get? seasonal-interest { season-id: season-id, buyer: tx-sender }))
  )
    (asserts! (> max-price u0) (err u305))
    (asserts! (> desired-quantity u0) (err u306))
    
    (if (is-some existing-interest)
      (begin
        (map-set seasonal-interest
          { season-id: season-id, buyer: tx-sender }
          {
            max-price: max-price,
            desired-quantity: desired-quantity,
            registered-at: stacks-block-height,
            is-active: true
          }
        )
        (ok true)
      )
      (begin
        (map-insert seasonal-interest
          { season-id: season-id, buyer: tx-sender }
          {
            max-price: max-price,
            desired-quantity: desired-quantity,
            registered-at: stacks-block-height,
            is-active: true
          }
        )
        
        (let (
          (new-total-buyers (+ (get total-interested-buyers current-summary) u1))
          (new-total-quantity (+ (get total-demand-quantity current-summary) desired-quantity))
          (new-price-sum (+ (* (get average-max-price current-summary) (get total-interested-buyers current-summary)) max-price))
          (new-average-price (if (> new-total-buyers u0) (/ new-price-sum new-total-buyers) u0))
        )
          (map-set seasonal-demand-summary
            { season-id: season-id }
            {
              total-interested-buyers: new-total-buyers,
              total-demand-quantity: new-total-quantity,
              average-max-price: new-average-price,
              last-updated: stacks-block-height
            }
          )
        )
        (ok true)
      )
    )
  )
)

(define-public (create-seasonal-pre-order
    (season-id uint)
    (buyer principal)
    (quantity uint)
    (agreed-price uint)
    (delivery-month uint))
  (let (
    (season-type (unwrap! (map-get? seasonal-produce-types { season-id: season-id }) (err u303)))
    (buyer-interest (unwrap! (map-get? seasonal-interest { season-id: season-id, buyer: buyer }) (err u307)))
  )
    (asserts! (> quantity u0) (err u308))
    (asserts! (> agreed-price u0) (err u309))
    (asserts! (and (<= delivery-month u12) (>= delivery-month u1)) (err u310))
    (asserts! (<= agreed-price (get max-price buyer-interest)) (err u311))
    (asserts! (<= quantity (get desired-quantity buyer-interest)) (err u312))
    
    (map-insert seasonal-pre-orders
      { season-id: season-id, buyer: buyer, farmer: tx-sender }
      {
        quantity: quantity,
        agreed-price: agreed-price,
        delivery-month: delivery-month,
        status: "confirmed",
        created-at: stacks-block-height
      }
    )
    
    (ok true)
  )
)

(define-public (cancel-seasonal-interest (season-id uint))
  (let (
    (interest (unwrap! (map-get? seasonal-interest { season-id: season-id, buyer: tx-sender }) (err u307)))
    (current-summary (unwrap! (map-get? seasonal-demand-summary { season-id: season-id }) (err u304)))
  )
    (map-set seasonal-interest
      { season-id: season-id, buyer: tx-sender }
      (merge interest { is-active: false })
    )
    
    (let (
      (new-total-buyers (- (get total-interested-buyers current-summary) u1))
      (new-total-quantity (- (get total-demand-quantity current-summary) (get desired-quantity interest)))
    )
      (map-set seasonal-demand-summary
        { season-id: season-id }
        {
          total-interested-buyers: new-total-buyers,
          total-demand-quantity: new-total-quantity,
          average-max-price: (get average-max-price current-summary),
          last-updated: stacks-block-height
        }
      )
    )
    (ok true)
  )
)

(define-read-only (get-seasonal-produce-type (season-id uint))
  (map-get? seasonal-produce-types { season-id: season-id })
)

(define-read-only (get-seasonal-demand-summary (season-id uint))
  (map-get? seasonal-demand-summary { season-id: season-id })
)

(define-read-only (get-seasonal-interest (season-id uint) (buyer principal))
  (map-get? seasonal-interest { season-id: season-id, buyer: buyer })
)

(define-read-only (get-seasonal-pre-order (season-id uint) (buyer principal) (farmer principal))
  (map-get? seasonal-pre-orders { season-id: season-id, buyer: buyer, farmer: farmer })
)

(define-read-only (calculate-demand-score (season-id uint))
  (let ((summary (map-get? seasonal-demand-summary { season-id: season-id })))
    (match summary
      data (+ (* (get total-interested-buyers data) u10) (/ (get total-demand-quantity data) u10))
      u0
    )
  )
)




