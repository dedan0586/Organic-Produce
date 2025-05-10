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

