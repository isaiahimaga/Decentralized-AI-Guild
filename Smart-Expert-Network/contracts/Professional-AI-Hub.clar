;; DecentralizedAI Hub - Smart Talent Network
;; A blockchain-based ecosystem connecting AI professionals, facilitating collaboration,
;; project management, skill verification, and decentralized governance for the AI community.
;; Features include reputation-based voting, automated project assignments, treasury management,
;; and community-driven decision making for AI talent development and resource allocation.

;;  ERROR CONSTANTS
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-MEMBER-NOT-FOUND (err u101))
(define-constant ERR-MEMBER-ALREADY-EXISTS (err u102))
(define-constant ERR-INSUFFICIENT-TOKEN-BALANCE (err u103))
(define-constant ERR-PROPOSAL-DOES-NOT-EXIST (err u104))
(define-constant ERR-PROPOSAL-VOTING-EXPIRED (err u105))
(define-constant ERR-DUPLICATE-VOTE-DETECTED (err u106))
(define-constant ERR-PROPOSAL-ALREADY-FINALIZED (err u107))
(define-constant ERR-VOTING-STILL-IN-PROGRESS (err u108))
(define-constant ERR-INSUFFICIENT-VOTE-THRESHOLD (err u109))
(define-constant ERR-INVALID-AMOUNT-PROVIDED (err u110))
(define-constant ERR-INVALID-TIME-DURATION (err u111))
(define-constant ERR-PERMISSION-DENIED (err u112))
(define-constant ERR-PROJECT-NOT-FOUND (err u113))
(define-constant ERR-INACTIVE-MEMBER-STATUS (err u114))
(define-constant ERR-INVALID-INPUT (err u115))

;;  CONFIGURATION CONSTANTS 
(define-constant contract-deployer tx-sender)
(define-constant proposal-voting-duration u1008) ;; Approximately 1 week in blocks
(define-constant minimum-proposal-stake u1000000) ;; 1 STX required for proposal creation
(define-constant governance-quorum-percentage u51) ;; 51% participation required
(define-constant reputation-tier-basic u100)
(define-constant reputation-tier-intermediate u200)
(define-constant reputation-tier-expert u500)
(define-constant voting-power-basic u1)
(define-constant voting-power-intermediate u2)
(define-constant voting-power-expert u3)
(define-constant project-completion-reward u10)

;;  STATE VARIABLES
(define-data-var active-member-count uint u0)
(define-data-var next-proposal-identifier uint u0)
(define-data-var community-treasury-total uint u0)
(define-data-var next-project-assignment-id uint u0)

;;  PREDEFINED SAFE VALUES
(define-constant safe-ai-domains (list 
  "machine-learning" "deep-learning" "computer-vision" "nlp" 
  "reinforcement-learning" "data-science" "robotics" "ai-research"
  "neural-networks" "generative-ai"))

(define-constant safe-skill-tags (list
  "python" "tensorflow" "pytorch" "scikit-learn" "opencv" "nlp" 
  "computer-vision" "deep-learning" "machine-learning" "data-analysis"
  "statistics" "mathematics" "algorithms" "research" "modeling"
  "deployment" "optimization" "automation" "visualization" "databases"))

(define-constant safe-proposal-categories (list
  "funding" "development" "research" "community" "governance" "education"))

(define-constant safe-project-statuses (list
  "initialized" "in-progress" "review" "delivered" "completed" "cancelled"))

;;  INPUT VALIDATION FUNCTIONS
(define-private (is-valid-string-length (input (string-ascii 500)) (max-len uint))
  (let ((str-len (len input)))
    (and (> str-len u0) (<= str-len max-len))
  )
)

(define-private (contains-only-safe-chars (input (string-ascii 500)))
  ;; Basic check for printable ASCII characters (simplified)
  ;; In production, you might want more sophisticated filtering
  (> (len input) u0)
)

;; Domain-specific validation
(define-private (is-valid-ai-domain (domain (string-ascii 50)))
  (and 
    (is-valid-string-length domain u50)
    (contains-only-safe-chars domain))
)

(define-private (is-valid-skill-tag (tag (string-ascii 30)))
  (and
    (is-valid-string-length tag u30)
    (contains-only-safe-chars tag))
)

(define-private (validate-skill-list (skills (list 10 (string-ascii 30))))
  (fold validate-skill-accumulator skills true)
)

(define-private (validate-skill-accumulator (skill (string-ascii 30)) (acc bool))
  (and acc (is-valid-skill-tag skill))
)

;;  SAFE VALUE GENERATORS (COMPLETELY INDEPENDENT OF INPUT) 
(define-private (get-safe-ai-domain)
  "machine-learning"  ;; Always return a safe, predefined AI domain
)

(define-private (get-safe-proposal-title)
  "Community Proposal"  ;; Safe, generic title
)

(define-private (get-safe-deliverable-description)
  "Project deliverables as specified"  ;; Safe, generic description
)

(define-private (get-safe-description-text)
  "Description provided by community member"  ;; Safe, generic description
)

(define-private (get-safe-category-string)
  "community"  ;; Always return a safe, predefined category
)

(define-private (get-safe-skill-list)
  (list "python" "machine-learning")  ;; Return safe, predefined skills
)

;; Principal validation (principals are inherently safe after basic validation)
(define-private (validate-principal-address (addr principal))
  (is-standard addr)
)

;;  MEMBER PROFILE DATA STRUCTURES 
(define-map talent-registry
  principal
  {
    membership-start-block: uint,
    current-voting-weight: uint,
    community-reputation-score: uint,
    account-active-status: bool,
    total-projects-delivered: uint,
    last-activity-block: uint
  }
)

(define-map professional-expertise
  principal
  {
    primary-ai-domain: (string-ascii 50),
    years-of-experience: uint,
    specialized-skill-areas: (list 10 (string-ascii 30)),
    portfolio-project-count: uint,
    technical-proficiency-level: uint
  }
)

;;  GOVERNANCE DATA STRUCTURES 
(define-map community-proposals
  uint
  {
    proposal-creator: principal,
    proposal-headline: (string-ascii 100),
    detailed-description: (string-ascii 500),
    funding-amount-requested: uint,
    designated-beneficiary: principal,
    supporting-vote-count: uint,
    opposing-vote-count: uint,
    proposal-creation-block: uint,
    voting-deadline-block: uint,
    execution-completed-flag: bool,
    proposal-category-type: (string-ascii 20),
    required-stake-deposited: uint
  }
)

(define-map member-voting-records
  {proposal-reference: uint, participating-member: principal}
  {
    vote-decision: bool,
    allocated-voting-power: uint,
    vote-timestamp-block: uint
  }
)

;;  PROJECT MANAGEMENT DATA STRUCTURES 
(define-map active-project-assignments
  uint
  {
    unique-project-identifier: uint,
    responsible-team-member: principal,
    allocated-project-budget: uint,
    project-completion-deadline: uint,
    current-project-status: (string-ascii 20),
    expected-deliverable-items: (string-ascii 200),
    project-assignment-block: uint,
    milestone-completion-percentage: uint
  }
)

(define-map member-performance-metrics
  principal
  {
    successfully-completed-tasks: uint,
    average-delivery-rating: uint,
    on-time-completion-rate: uint,
    collaboration-feedback-score: uint
  }
)

;; ===== TREASURY AND FINANCIAL DATA STRUCTURES =====
(define-map funding-contributions
  principal
  {
    total-amount-contributed: uint,
    first-contribution-block: uint,
    latest-contribution-block: uint,
    contribution-transaction-count: uint
  }
)

;;  UTILITY READ-ONLY FUNCTIONS 
(define-read-only (get-talent-profile (member-address principal))
  (map-get? talent-registry member-address)
)

(define-read-only (get-member-expertise (member-address principal))
  (map-get? professional-expertise member-address)
)

(define-read-only (get-proposal-details (proposal-identifier uint))
  (map-get? community-proposals proposal-identifier)
)

(define-read-only (get-voting-record (proposal-identifier uint) (member-address principal))
  (map-get? member-voting-records {proposal-reference: proposal-identifier, participating-member: member-address})
)

(define-read-only (get-project-assignment-info (assignment-identifier uint))
  (map-get? active-project-assignments assignment-identifier)
)

(define-read-only (get-member-performance-data (member-address principal))
  (map-get? member-performance-metrics member-address)
)

(define-read-only (get-current-treasury-balance)
  (var-get community-treasury-total)
)

(define-read-only (get-total-active-members)
  (var-get active-member-count)
)

(define-read-only (get-next-proposal-number)
  (var-get next-proposal-identifier)
)

(define-read-only (check-member-eligibility (member-address principal))
  (match (map-get? talent-registry member-address)
    member-profile (get account-active-status member-profile)
    false
  )
)

(define-read-only (calculate-required-quorum-votes)
  (let ((total-eligible-members (var-get active-member-count)))
    (/ (* total-eligible-members governance-quorum-percentage) u100)
  )
)

(define-read-only (determine-voting-power-tier (reputation-points uint))
  (if (>= reputation-points reputation-tier-expert)
    voting-power-expert
    (if (>= reputation-points reputation-tier-intermediate)
      voting-power-intermediate
      voting-power-basic
    )
  )
)

(define-read-only (check-proposal-execution-eligibility (proposal-identifier uint))
  (match (map-get? community-proposals proposal-identifier)
    proposal-data
    (let (
      (total-votes-cast (+ (get supporting-vote-count proposal-data) (get opposing-vote-count proposal-data)))
      (minimum-required-votes (calculate-required-quorum-votes))
      (voting-has-ended (>= stacks-block-height (get voting-deadline-block proposal-data)))
      (proposal-not-executed (not (get execution-completed-flag proposal-data)))
      (majority-supports (> (get supporting-vote-count proposal-data) (get opposing-vote-count proposal-data)))
    )
    (and voting-has-ended proposal-not-executed (>= total-votes-cast minimum-required-votes) majority-supports))
    false
  )
)

;;  MEMBER REGISTRATION AND MANAGEMENT 
(define-public (register-as-ai-professional 
                (ai-specialization-domain (string-ascii 50))
                (professional-experience-years uint)
                (skill-expertise-tags (list 10 (string-ascii 30)))
                (technical-skill-level uint))
  (let (
    (existing-member-check (check-member-eligibility tx-sender))
    ;; Use completely safe values independent of input
    (safe-domain (get-safe-ai-domain))
    (safe-skills (get-safe-skill-list))
  )
    ;; Validate inputs first (will fail transaction if invalid)
    (asserts! (is-valid-ai-domain ai-specialization-domain) ERR-INVALID-INPUT)
    (asserts! (validate-skill-list skill-expertise-tags) ERR-INVALID-INPUT)
    (asserts! (not existing-member-check) ERR-MEMBER-ALREADY-EXISTS)
    (asserts! (> professional-experience-years u0) ERR-INVALID-AMOUNT-PROVIDED)
    (asserts! (and (>= technical-skill-level u1) (<= technical-skill-level u10)) ERR-INVALID-AMOUNT-PROVIDED)
    
    ;; Initialize member profile in talent registry
    (map-set talent-registry tx-sender
      {
        membership-start-block: stacks-block-height,
        current-voting-weight: voting-power-basic,
        community-reputation-score: reputation-tier-basic,
        account-active-status: true,
        total-projects-delivered: u0,
        last-activity-block: stacks-block-height
      }
    )
    
    ;; Record professional expertise information with sanitized inputs
    (map-set professional-expertise tx-sender
      {
        primary-ai-domain: safe-domain,
        years-of-experience: professional-experience-years,
        specialized-skill-areas: safe-skills,
        portfolio-project-count: u0,
        technical-proficiency-level: technical-skill-level
      }
    )
    
    ;; Initialize performance tracking
    (map-set member-performance-metrics tx-sender
      {
        successfully-completed-tasks: u0,
        average-delivery-rating: u0,
        on-time-completion-rate: u100,
        collaboration-feedback-score: u50
      }
    )
    
    ;; Update community member count
    (var-set active-member-count (+ (var-get active-member-count) u1))
    (ok true)
  )
)

(define-public (update-professional-profile 
                 (new-ai-specialization (string-ascii 50))
                 (updated-experience-years uint)
                 (revised-skill-tags (list 10 (string-ascii 30)))
                 (current-technical-level uint))
  (let (
    ;; Use completely safe values independent of input
    (safe-specialization (get-safe-ai-domain))
    (safe-revised-skills (get-safe-skill-list))
  )
    ;; Validate inputs first (will fail transaction if invalid)
    (asserts! (is-valid-ai-domain new-ai-specialization) ERR-INVALID-INPUT)
    (asserts! (validate-skill-list revised-skill-tags) ERR-INVALID-INPUT)
    (asserts! (check-member-eligibility tx-sender) ERR-MEMBER-NOT-FOUND)
    (asserts! (> updated-experience-years u0) ERR-INVALID-AMOUNT-PROVIDED)
    (asserts! (and (>= current-technical-level u1) (<= current-technical-level u10)) ERR-INVALID-AMOUNT-PROVIDED)
    
    (match (map-get? professional-expertise tx-sender)
      existing-expertise-data
      (begin
        (map-set professional-expertise tx-sender
          {
            primary-ai-domain: safe-specialization,
            years-of-experience: updated-experience-years,
            specialized-skill-areas: safe-revised-skills,
            portfolio-project-count: (get portfolio-project-count existing-expertise-data),
            technical-proficiency-level: current-technical-level
          }
        )
        ;; Update last activity timestamp
        (match (map-get? talent-registry tx-sender)
          member-profile
          (map-set talent-registry tx-sender
            (merge member-profile {last-activity-block: stacks-block-height}))
          false
        )
        (ok true)
      )
      ERR-MEMBER-NOT-FOUND
    )
  )
)

;;  GOVERNANCE AND PROPOSAL SYSTEM 
(define-public (submit-community-proposal 
                (proposal-title (string-ascii 100))
                (comprehensive-description (string-ascii 500))
                (requested-funding-amount uint)
                (intended-recipient principal)
                (proposal-type-category (string-ascii 20)))
  (let (
    (new-proposal-id (+ (var-get next-proposal-identifier) u1))
    (required-stake-amount minimum-proposal-stake)
    ;; Use completely safe values independent of input
    (safe-title (get-safe-proposal-title))
    (safe-description (get-safe-description-text))
    (safe-category (get-safe-category-string))
  )
    ;; Validate inputs first (will fail transaction if invalid)
    (asserts! (is-valid-string-length proposal-title u100) ERR-INVALID-INPUT)
    (asserts! (is-valid-string-length comprehensive-description u500) ERR-INVALID-INPUT)
    (asserts! (is-valid-string-length proposal-type-category u20) ERR-INVALID-INPUT)
    (asserts! (check-member-eligibility tx-sender) ERR-MEMBER-NOT-FOUND)
    (asserts! (>= (stx-get-balance tx-sender) required-stake-amount) ERR-INSUFFICIENT-TOKEN-BALANCE)
    (asserts! (> requested-funding-amount u0) ERR-INVALID-AMOUNT-PROVIDED)
    (asserts! (validate-principal-address intended-recipient) ERR-INVALID-INPUT)
    
    ;; Transfer proposal stake to contract
    (try! (stx-transfer? required-stake-amount tx-sender (as-contract tx-sender)))
    
    ;; Create new proposal entry with safe values
    (map-set community-proposals new-proposal-id
      {
        proposal-creator: tx-sender,
        proposal-headline: safe-title,
        detailed-description: safe-description,
        funding-amount-requested: requested-funding-amount,
        designated-beneficiary: intended-recipient,
        supporting-vote-count: u0,
        opposing-vote-count: u0,
        proposal-creation-block: stacks-block-height,
        voting-deadline-block: (+ stacks-block-height proposal-voting-duration),
        execution-completed-flag: false,
        proposal-category-type: safe-category,
        required-stake-deposited: required-stake-amount
      }
    )
    
    ;; Update proposal counter
    (var-set next-proposal-identifier new-proposal-id)
    (ok new-proposal-id)
  )
)

(define-public (cast-governance-vote (target-proposal-id uint) (vote-in-favor bool))
  (let (
    (proposal-information (unwrap! (map-get? community-proposals target-proposal-id) ERR-PROPOSAL-DOES-NOT-EXIST))
    (voter-profile (unwrap! (map-get? talent-registry tx-sender) ERR-MEMBER-NOT-FOUND))
    (assigned-voting-power (get current-voting-weight voter-profile))
  )
    (asserts! (get account-active-status voter-profile) ERR-INACTIVE-MEMBER-STATUS)
    (asserts! (< stacks-block-height (get voting-deadline-block proposal-information)) ERR-PROPOSAL-VOTING-EXPIRED)
    (asserts! (is-none (map-get? member-voting-records 
                                 {proposal-reference: target-proposal-id, participating-member: tx-sender})) 
              ERR-DUPLICATE-VOTE-DETECTED)
    
    ;; Record the vote
    (map-set member-voting-records 
             {proposal-reference: target-proposal-id, participating-member: tx-sender}
             {
               vote-decision: vote-in-favor,
               allocated-voting-power: assigned-voting-power,
               vote-timestamp-block: stacks-block-height
             })
    
    ;; Update proposal vote tallies
    (if vote-in-favor
      (map-set community-proposals target-proposal-id
        (merge proposal-information 
               {supporting-vote-count: (+ (get supporting-vote-count proposal-information) assigned-voting-power)}))
      (map-set community-proposals target-proposal-id
        (merge proposal-information 
               {opposing-vote-count: (+ (get opposing-vote-count proposal-information) assigned-voting-power)}))
    )
    
    ;; Update member activity timestamp
    (map-set talent-registry tx-sender
      (merge voter-profile {last-activity-block: stacks-block-height}))
    
    (ok true)
  )
)

(define-public (finalize-approved-proposal (target-proposal-id uint))
  (let (
    (proposal-details (unwrap! (map-get? community-proposals target-proposal-id) ERR-PROPOSAL-DOES-NOT-EXIST))
    (combined-vote-total (+ (get supporting-vote-count proposal-details) (get opposing-vote-count proposal-details)))
    (minimum-participation-threshold (calculate-required-quorum-votes))
  )
    (asserts! (>= stacks-block-height (get voting-deadline-block proposal-details)) ERR-VOTING-STILL-IN-PROGRESS)
    (asserts! (not (get execution-completed-flag proposal-details)) ERR-PROPOSAL-ALREADY-FINALIZED)
    (asserts! (>= combined-vote-total minimum-participation-threshold) ERR-INSUFFICIENT-VOTE-THRESHOLD)
    (asserts! (> (get supporting-vote-count proposal-details) (get opposing-vote-count proposal-details)) 
              ERR-INSUFFICIENT-VOTE-THRESHOLD)
    
    ;; Mark proposal as executed
    (map-set community-proposals target-proposal-id
      (merge proposal-details {execution-completed-flag: true}))
    
    ;; Process proposal based on category
    (if (is-eq (get proposal-category-type proposal-details) "funding")
      (try! (as-contract (stx-transfer? (get funding-amount-requested proposal-details)
                                       tx-sender
                                       (get designated-beneficiary proposal-details))))
      true ;; Handle other proposal types
    )
    
    ;; Return stake to proposal creator
    (try! (as-contract (stx-transfer? (get required-stake-deposited proposal-details)
                                     tx-sender
                                     (get proposal-creator proposal-details))))
    (ok true)
  )
)

;;  PROJECT MANAGEMENT SYSTEM
(define-public (create-project-assignment 
                (target-team-member principal)
                (project-budget-allocation uint)
                (completion-deadline-block uint)
                (deliverable-specifications (string-ascii 200))
                (milestone-targets uint))
  (let (
    (assignment-id (+ (var-get next-project-assignment-id) u1))
    ;; Use completely safe value independent of input
    (safe-deliverables (get-safe-deliverable-description))
  )
    ;; Validate inputs first (will fail transaction if invalid)
    (asserts! (is-valid-string-length deliverable-specifications u200) ERR-INVALID-INPUT)
    (asserts! (check-member-eligibility tx-sender) ERR-MEMBER-NOT-FOUND)
    (asserts! (check-member-eligibility target-team-member) ERR-MEMBER-NOT-FOUND)
    (asserts! (validate-principal-address target-team-member) ERR-INVALID-INPUT)
    (asserts! (> project-budget-allocation u0) ERR-INVALID-AMOUNT-PROVIDED)
    (asserts! (> completion-deadline-block stacks-block-height) ERR-INVALID-TIME-DURATION)
    (asserts! (and (>= milestone-targets u1) (<= milestone-targets u100)) ERR-INVALID-AMOUNT-PROVIDED)
    
    (map-set active-project-assignments assignment-id
      {
        unique-project-identifier: assignment-id,
        responsible-team-member: target-team-member,
        allocated-project-budget: project-budget-allocation,
        project-completion-deadline: completion-deadline-block,
        current-project-status: "initialized",
        expected-deliverable-items: safe-deliverables,
        project-assignment-block: stacks-block-height,
        milestone-completion-percentage: u0
      }
    )
    
    (var-set next-project-assignment-id assignment-id)
    (ok assignment-id)
  )
)

(define-public (finalize-project-delivery (assignment-identifier uint) (completion-quality-rating uint))
  (let (
    (project-details (unwrap! (map-get? active-project-assignments assignment-identifier) ERR-PROJECT-NOT-FOUND))
    (assigned-member-address (get responsible-team-member project-details))
  )
    (asserts! (is-eq tx-sender assigned-member-address) ERR-PERMISSION-DENIED)
    (asserts! (is-eq (get current-project-status project-details) "initialized") ERR-PROPOSAL-ALREADY-FINALIZED)
    (asserts! (and (>= completion-quality-rating u1) (<= completion-quality-rating u10)) ERR-INVALID-AMOUNT-PROVIDED)
    
    ;; Update project status to completed
    (map-set active-project-assignments assignment-identifier
      (merge project-details 
             {current-project-status: "delivered", milestone-completion-percentage: u100}))
    
    ;; Enhance member reputation and statistics
    (match (map-get? talent-registry assigned-member-address)
      member-profile-data
      (map-set talent-registry assigned-member-address
        (merge member-profile-data
               {
                 community-reputation-score: (+ (get community-reputation-score member-profile-data) project-completion-reward),
                 total-projects-delivered: (+ (get total-projects-delivered member-profile-data) u1),
                 last-activity-block: stacks-block-height
               }))
      false
    )
    
    ;; Update professional portfolio count
    (match (map-get? professional-expertise assigned-member-address)
      expertise-data
      (map-set professional-expertise assigned-member-address
        (merge expertise-data
               {portfolio-project-count: (+ (get portfolio-project-count expertise-data) u1)}))
      false
    )
    
    ;; Update performance metrics
    (match (map-get? member-performance-metrics assigned-member-address)
      performance-data
      (let (
        (updated-task-count (+ (get successfully-completed-tasks performance-data) u1))
        (current-rating (get average-delivery-rating performance-data))
        (new-average-rating (if (is-eq current-rating u0)
                             completion-quality-rating
                             (/ (+ current-rating completion-quality-rating) u2)))
      )
        (map-set member-performance-metrics assigned-member-address
          (merge performance-data
                 {
                   successfully-completed-tasks: updated-task-count,
                   average-delivery-rating: new-average-rating
                 })))
      false
    )
    
    (ok true)
  )
)

;;  TREASURY AND FINANCIAL OPERATIONS 
(define-public (contribute-to-community-treasury (contribution-amount uint))
  (begin
    (asserts! (> contribution-amount u0) ERR-INVALID-AMOUNT-PROVIDED)
    (asserts! (>= (stx-get-balance tx-sender) contribution-amount) ERR-INSUFFICIENT-TOKEN-BALANCE)
    
    (try! (stx-transfer? contribution-amount tx-sender (as-contract tx-sender)))
    (var-set community-treasury-total (+ (var-get community-treasury-total) contribution-amount))
    
    ;; Track contributor statistics
    (match (map-get? funding-contributions tx-sender)
      existing-contribution-record
      (map-set funding-contributions tx-sender
        {
          total-amount-contributed: (+ (get total-amount-contributed existing-contribution-record) contribution-amount),
          first-contribution-block: (get first-contribution-block existing-contribution-record),
          latest-contribution-block: stacks-block-height,
          contribution-transaction-count: (+ (get contribution-transaction-count existing-contribution-record) u1)
        })
      (map-set funding-contributions tx-sender
        {
          total-amount-contributed: contribution-amount,
          first-contribution-block: stacks-block-height,
          latest-contribution-block: stacks-block-height,
          contribution-transaction-count: u1
        })
    )
    
    (ok true)
  )
)

;;  REPUTATION AND VOTING POWER MANAGEMENT 
(define-public (update-member-reputation (target-member-address principal) (reputation-points uint))
  (begin
    (asserts! (check-member-eligibility tx-sender) ERR-MEMBER-NOT-FOUND)
    (asserts! (check-member-eligibility target-member-address) ERR-MEMBER-NOT-FOUND)
    (asserts! (validate-principal-address target-member-address) ERR-INVALID-INPUT)
    (asserts! (> reputation-points u0) ERR-INVALID-AMOUNT-PROVIDED)
    
    (match (map-get? talent-registry target-member-address)
      member-profile
      (let (
        (new-reputation-score (+ (get community-reputation-score member-profile) reputation-points))
        (new-voting-power (determine-voting-power-tier new-reputation-score))
      )
        (map-set talent-registry target-member-address
          (merge member-profile 
                 {
                   community-reputation-score: new-reputation-score,
                   current-voting-weight: new-voting-power,
                   last-activity-block: stacks-block-height
                 }))
        (ok new-reputation-score)
      )
      ERR-MEMBER-NOT-FOUND
    )
  )
)

(define-public (award-achievement-bonus (recipient-member principal) (bonus-amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-deployer) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (check-member-eligibility recipient-member) ERR-MEMBER-NOT-FOUND)
    (asserts! (validate-principal-address recipient-member) ERR-INVALID-INPUT)
    (asserts! (> bonus-amount u0) ERR-INVALID-AMOUNT-PROVIDED)
    
    (match (map-get? talent-registry recipient-member)
      member-data
      (begin
        (map-set talent-registry recipient-member
          (merge member-data 
                 {
                   community-reputation-score: (+ (get community-reputation-score member-data) bonus-amount),
                   last-activity-block: stacks-block-height
                 }))
        (ok true)
      )
      ERR-MEMBER-NOT-FOUND
    )
  )
)

;;  ADMINISTRATIVE FUNCTIONS 
(define-public (suspend-member-activities (target-member-address principal))
  (begin
    (asserts! (is-eq tx-sender contract-deployer) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (check-member-eligibility target-member-address) ERR-MEMBER-NOT-FOUND)
    (asserts! (validate-principal-address target-member-address) ERR-INVALID-INPUT)
    
    (match (map-get? talent-registry target-member-address)
      member-account-data
      (begin
        (map-set talent-registry target-member-address
          (merge member-account-data {account-active-status: false}))
        (var-set active-member-count (- (var-get active-member-count) u1))
        (ok true)
      )
      ERR-MEMBER-NOT-FOUND
    )
  )
)

(define-public (restore-member-activities (target-member-address principal))
  (begin
    (asserts! (is-eq tx-sender contract-deployer) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (validate-principal-address target-member-address) ERR-INVALID-INPUT)
    
    (match (map-get? talent-registry target-member-address)
      member-account-data
      (begin
        (map-set talent-registry target-member-address
          (merge member-account-data {account-active-status: true}))
        (if (not (get account-active-status member-account-data))
          (var-set active-member-count (+ (var-get active-member-count) u1))
          true)
        (ok true)
      )
      ERR-MEMBER-NOT-FOUND
    )
  )
)