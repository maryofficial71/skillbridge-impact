;; SkillBridge - Decentralized Social Impact Platform
;; Comprehensive Smart Contract for Professional Skill Verification and Community Impact

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-EXISTS (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-INVALID-INPUT (err u104))
(define-constant ERR-INACTIVE-REALM (err u105))
(define-constant ERR-INVALID-VERIFICATION (err u106))
(define-constant ERR-CHALLENGE-EXPIRED (err u107))
(define-constant ERR-ALREADY-VERIFIED (err u108))
(define-constant ERR-INSUFFICIENT-STAKE (err u109))
(define-constant ERR-INVALID-COLLABORATION (err u110))
(define-constant ERR-COMMUNITY-REJECTION (err u111))
(define-constant ERR-IMPACT-THRESHOLD-NOT-MET (err u112))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MINIMUM-STAKE u1000000) ;; 1 STX in microSTX
(define-constant VERIFICATION-PERIOD u144) ;; ~24 hours in blocks
(define-constant IMPACT-THRESHOLD u100)
(define-constant MAX-POD-SIZE u8)

;; Data Variables
(define-data-var next-realm-id uint u1)
(define-data-var next-challenge-id uint u1)
(define-data-var next-nft-id uint u1)
(define-data-var next-pod-id uint u1)
(define-data-var platform-fee uint u50) ;; 0.5%
(define-data-var total-impact-score uint u0)
(define-data-var active-season-id uint u0)

;; Data Maps
(define-map impact-realms
    { realm-id: uint }
    { 
        name: (string-ascii 64),
        location: (string-ascii 128),
        challenge-type: (string-ascii 64),
        active: bool,
        total-impact: uint,
        verification-threshold: uint,
        creator: principal,
        created-at: uint
    })

(define-map professional-profiles
    { professional: principal }
    {
        skills: (list 10 (string-ascii 32)),
        impact-score: uint,
        verified-contributions: uint,
        cultural-bridge-level: uint,
        staked-tokens: uint,
        active-pods: (list 5 uint),
        reputation: uint,
        joined-at: uint
    })

(define-map impact-nfts
    { nft-id: uint }
    {
        owner: principal,
        nft-type: (string-ascii 32),
        realm-id: uint,
        impact-value: uint,
        metadata: (string-ascii 256),
        verified: bool,
        created-at: uint,
        expiry: (optional uint)
    })

(define-map collaboration-pods
    { pod-id: uint }
    {
        name: (string-ascii 64),
        members: (list 8 principal),
        active-challenge: (optional uint),
        total-impact: uint,
        cultural-bridge-score: uint,
        created-at: uint,
        leader: principal
    })

(define-map development-challenges
    { challenge-id: uint }
    {
        title: (string-ascii 128),
        description: (string-ascii 512),
        sponsor: principal,
        realm-id: uint,
        required-skills: (list 5 (string-ascii 32)),
        reward-amount: uint,
        participants: (list 20 principal),
        deadline: uint,
        status: (string-ascii 16),
        winner: (optional principal)
    })

(define-map community-verifications
    { verification-id: (string-ascii 64) }
    {
        professional: principal,
        community-validator: principal,
        challenge-id: uint,
        verified: bool,
        impact-rating: uint,
        feedback: (string-ascii 256),
        timestamp: uint
    })

(define-map skill-categories
    { category: (string-ascii 32) }
    {
        active: bool,
        impact-multiplier: uint,
        required-verifications: uint
    })

(define-map impact-seasons
    { season-id: uint }
    {
        name: (string-ascii 64),
        start-block: uint,
        end-block: uint,
        total-prize-pool: uint,
        active-challenges: (list 10 uint),
        participants: uint
    })

(define-map professional-stakes
    { professional: principal, challenge-id: uint }
    {
        amount: uint,
        locked-at: uint,
        released: bool
    })

(define-map hybrid-nft-classes
    { class-name: (string-ascii 64) }
    {
        required-skills: (list 5 (string-ascii 32)),
        min-impact-score: uint,
        cultural-bridge-requirement: uint,
        active: bool
    })

;; Owner/Admin Functions
(define-public (create-impact-realm (name (string-ascii 64)) (location (string-ascii 128)) (challenge-type (string-ascii 64)) (verification-threshold uint))
    (let ((realm-id (var-get next-realm-id)))
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> (len name) u0) ERR-INVALID-INPUT)
        (asserts! (> verification-threshold u0) ERR-INVALID-INPUT)
        (map-set impact-realms 
            { realm-id: realm-id }
            {
                name: name,
                location: location,
                challenge-type: challenge-type,
                active: true,
                total-impact: u0,
                verification-threshold: verification-threshold,
                creator: tx-sender,
                created-at: block-height
            })
        (var-set next-realm-id (+ realm-id u1))
        (ok realm-id)))

(define-public (activate-impact-season (name (string-ascii 64)) (duration uint) (prize-pool uint))
    (let ((season-id (+ (var-get active-season-id) u1)))
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> duration u0) ERR-INVALID-INPUT)
        (asserts! (> prize-pool u0) ERR-INVALID-INPUT)
        (map-set impact-seasons
            { season-id: season-id }
            {
                name: name,
                start-block: block-height,
                end-block: (+ block-height duration),
                total-prize-pool: prize-pool,
                active-challenges: (list),
                participants: u0
            })
        (var-set active-season-id season-id)
        (ok season-id)))

(define-public (register-skill-category (category (string-ascii 32)) (impact-multiplier uint) (required-verifications uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> (len category) u0) ERR-INVALID-INPUT)
        (asserts! (> impact-multiplier u0) ERR-INVALID-INPUT)
        (map-set skill-categories
            { category: category }
            {
                active: true,
                impact-multiplier: impact-multiplier,
                required-verifications: required-verifications
            })
        (ok true)))

(define-public (create-hybrid-nft-class (class-name (string-ascii 64)) (required-skills (list 5 (string-ascii 32))) (min-impact-score uint) (cultural-bridge-requirement uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> (len class-name) u0) ERR-INVALID-INPUT)
        (asserts! (> min-impact-score u0) ERR-INVALID-INPUT)
        (map-set hybrid-nft-classes
            { class-name: class-name }
            {
                required-skills: required-skills,
                min-impact-score: min-impact-score,
                cultural-bridge-requirement: cultural-bridge-requirement,
                active: true
            })
        (ok true)))

;; Public Functions
(define-public (register-professional (skills (list 10 (string-ascii 32))))
    (let ((existing-profile (map-get? professional-profiles { professional: tx-sender })))
        (asserts! (is-none existing-profile) ERR-ALREADY-EXISTS)
        (asserts! (> (len skills) u0) ERR-INVALID-INPUT)
        (map-set professional-profiles
            { professional: tx-sender }
            {
                skills: skills,
                impact-score: u0,
                verified-contributions: u0,
                cultural-bridge-level: u0,
                staked-tokens: u0,
                active-pods: (list),
                reputation: u100,
                joined-at: block-height
            })
        (ok true)))

(define-public (create-development-challenge (title (string-ascii 128)) (description (string-ascii 512)) (realm-id uint) (required-skills (list 5 (string-ascii 32))) (reward-amount uint) (duration uint))
    (let ((challenge-id (var-get next-challenge-id))
          (realm (map-get? impact-realms { realm-id: realm-id })))
        (asserts! (is-some realm) ERR-NOT-FOUND)
        (asserts! (get active (unwrap-panic realm)) ERR-INACTIVE-REALM)
        (asserts! (> (len title) u0) ERR-INVALID-INPUT)
        (asserts! (> reward-amount u0) ERR-INVALID-INPUT)
        (asserts! (> duration u0) ERR-INVALID-INPUT)
        (try! (stx-transfer? reward-amount tx-sender (as-contract tx-sender)))
        (map-set development-challenges
            { challenge-id: challenge-id }
            {
                title: title,
                description: description,
                sponsor: tx-sender,
                realm-id: realm-id,
                required-skills: required-skills,
                reward-amount: reward-amount,
                participants: (list),
                deadline: (+ block-height duration),
                status: "active",
                winner: none
            })
        (var-set next-challenge-id (+ challenge-id u1))
        (ok challenge-id)))

(define-public (join-challenge (challenge-id uint))
    (let ((challenge (map-get? development-challenges { challenge-id: challenge-id }))
          (profile (map-get? professional-profiles { professional: tx-sender })))
        (asserts! (is-some challenge) ERR-NOT-FOUND)
        (asserts! (is-some profile) ERR-NOT-FOUND)
        (asserts! (< block-height (get deadline (unwrap-panic challenge))) ERR-CHALLENGE-EXPIRED)
        (asserts! (>= (stx-get-balance tx-sender) MINIMUM-STAKE) ERR-INSUFFICIENT-BALANCE)
        (try! (stx-transfer? MINIMUM-STAKE tx-sender (as-contract tx-sender)))
        (map-set professional-stakes
            { professional: tx-sender, challenge-id: challenge-id }
            {
                amount: MINIMUM-STAKE,
                locked-at: block-height,
                released: false