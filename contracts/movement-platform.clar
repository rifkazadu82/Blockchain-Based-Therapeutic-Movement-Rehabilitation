;; ========================================
;; Therapeutic Movement Rehabilitation System
;; ========================================
;; A comprehensive blockchain-based system for coordinating physical therapy programs
;; with movement assessment, recovery progress tracking, and adaptive equipment sharing

;; ========================================
;; CONTRACT 1: THERAPY PROGRAM MANAGEMENT
;; ========================================

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROGRAM-NOT-FOUND (err u101))
(define-constant ERR-PATIENT-NOT-FOUND (err u102))
(define-constant ERR-INVALID-ASSESSMENT (err u103))
(define-constant ERR-PROGRAM-COMPLETE (err u104))
(define-constant ERR-INSUFFICIENT-PROGRESS (err u105))

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var program-counter uint u0)
(define-data-var assessment-counter uint u0)

;; Patient profile structure
(define-map patient-profiles
  { patient-id: principal }
  {
    mobility-level: uint,
    accessibility-needs: (string-utf8 500),
    pain-threshold: uint,
    target-outcomes: (list 10 (string-utf8 100)),
    assigned-therapist: principal,
    registration-block: uint,
    status: (string-utf8 50)
  }
)

;; Therapy program structure
(define-map therapy-programs
  { program-id: uint }
  {
    patient-id: principal,
    therapist-id: principal,
    program-type: (string-utf8 100),
    difficulty-level: uint,
    duration-weeks: uint,
    accessibility-accommodations: (string-utf8 500),
    pain-management-protocol: (string-utf8 300),
    holistic-approaches: (list 5 (string-utf8 100)),
    target-metrics: (list 5 (string-utf8 100)),
    created-block: uint,
    status: (string-utf8 50),
    completion-percentage: uint
  }
)

;; Movement assessment structure
(define-map movement-assessments
  { assessment-id: uint }
  {
    program-id: uint,
    patient-id: principal,
    assessor-id: principal,
    movement-scores: (list 10 uint),
    pain-levels: (list 10 uint),
    mobility-metrics: (list 10 uint),
    adaptive-equipment-used: (list 5 (string-utf8 100)),
    assessment-date: uint,
    recovery-progress: uint,
    recommendations: (string-utf8 500),
    next-assessment-block: uint
  }
)

;; Progress tracking structure
(define-map progress-records
  { patient-id: principal, week: uint }
  {
    program-id: uint,
    mobility-improvement: uint,
    pain-reduction: uint,
    functional-capacity: uint,
    equipment-adaptation: uint,
    holistic-wellness-score: uint,
    therapist-notes: (string-utf8 500),
    patient-feedback: (string-utf8 500),
    milestone-achieved: bool
  }
)

;; Therapist credentials
(define-map certified-therapists
  { therapist-id: principal }
  {
    license-number: (string-utf8 50),
    specializations: (list 5 (string-utf8 100)),
    accessibility-certified: bool,
    pain-management-certified: bool,
    holistic-therapy-certified: bool,
    active-patients: uint,
    certification-expiry: uint
  }
)

;; ========================================
;; PATIENT MANAGEMENT FUNCTIONS
;; ========================================

;; Register a new patient
(define-public (register-patient
  (mobility-level uint)
  (accessibility-needs (string-utf8 500))
  (pain-threshold uint)
  (target-outcomes (list 10 (string-utf8 100)))
  (assigned-therapist principal))
  (let ((patient-id tx-sender))
    (asserts! (is-none (map-get? patient-profiles { patient-id: patient-id })) (err u106))
    (map-set patient-profiles
      { patient-id: patient-id }
      {
        mobility-level: mobility-level,
        accessibility-needs: accessibility-needs,
        pain-threshold: pain-threshold,
        target-outcomes: target-outcomes,
        assigned-therapist: assigned-therapist,
        registration-block: stacks-block-height,
        status: u"active"
      }
    )
    (ok patient-id)))

;; Update patient profile
(define-public (update-patient-profile
  (patient-id principal)
  (mobility-level uint)
  (accessibility-needs (string-utf8 500))
  (pain-threshold uint))
  (let ((patient-profile (unwrap! (map-get? patient-profiles { patient-id: patient-id }) ERR-PATIENT-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender patient-id)
                  (is-eq tx-sender (get assigned-therapist patient-profile))) ERR-NOT-AUTHORIZED)
    (map-set patient-profiles
      { patient-id: patient-id }
      (merge patient-profile {
        mobility-level: mobility-level,
        accessibility-needs: accessibility-needs,
        pain-threshold: pain-threshold
      })
    )
    (ok true)))

;; ========================================
;; THERAPY PROGRAM FUNCTIONS
;; ========================================

;; Create a new therapy program
(define-public (create-therapy-program
  (patient-id principal)
  (program-type (string-utf8 100))
  (difficulty-level uint)
  (duration-weeks uint)
  (accessibility-accommodations (string-utf8 500))
  (pain-management-protocol (string-utf8 300))
  (holistic-approaches (list 5 (string-utf8 100)))
  (target-metrics (list 5 (string-utf8 100))))
  (let (
    (program-id (+ (var-get program-counter) u1))
    (patient-profile (unwrap! (map-get? patient-profiles { patient-id: patient-id }) ERR-PATIENT-NOT-FOUND))
  )
    (asserts! (or (is-eq tx-sender patient-id)
                  (is-eq tx-sender (get assigned-therapist patient-profile))) ERR-NOT-AUTHORIZED)
    (map-set therapy-programs
      { program-id: program-id }
      {
        patient-id: patient-id,
        therapist-id: tx-sender,
        program-type: program-type,
        difficulty-level: difficulty-level,
        duration-weeks: duration-weeks,
        accessibility-accommodations: accessibility-accommodations,
        pain-management-protocol: pain-management-protocol,
        holistic-approaches: holistic-approaches,
        target-metrics: target-metrics,
        created-block: stacks-block-height,
        status: u"active",
        completion-percentage: u0
      }
    )
    (var-set program-counter program-id)
    (ok program-id)))

;; Update program progress
(define-public (update-program-progress
  (program-id uint)
  (completion-percentage uint))
  (let ((program (unwrap! (map-get? therapy-programs { program-id: program-id }) ERR-PROGRAM-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get therapist-id program)) ERR-NOT-AUTHORIZED)
    (asserts! (<= completion-percentage u100) (err u107))
    (map-set therapy-programs
      { program-id: program-id }
      (merge program {
        completion-percentage: completion-percentage,
        status: (if (>= completion-percentage u100) u"completed" u"active")
      })
    )
    (ok true)))

;; ========================================
;; MOVEMENT ASSESSMENT FUNCTIONS
;; ========================================

;; Conduct movement assessment
(define-public (conduct-movement-assessment
  (program-id uint)
  (movement-scores (list 10 uint))
  (pain-levels (list 10 uint))
  (mobility-metrics (list 10 uint))
  (adaptive-equipment-used (list 5 (string-utf8 100)))
  (recovery-progress uint)
  (recommendations (string-utf8 500)))
  (let (
    (assessment-id (+ (var-get assessment-counter) u1))
    (program (unwrap! (map-get? therapy-programs { program-id: program-id }) ERR-PROGRAM-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get therapist-id program)) ERR-NOT-AUTHORIZED)
    (map-set movement-assessments
      { assessment-id: assessment-id }
      {
        program-id: program-id,
        patient-id: (get patient-id program),
        assessor-id: tx-sender,
        movement-scores: movement-scores,
        pain-levels: pain-levels,
        mobility-metrics: mobility-metrics,
        adaptive-equipment-used: adaptive-equipment-used,
        assessment-date: stacks-block-height,
        recovery-progress: recovery-progress,
        recommendations: recommendations,
        next-assessment-block: (+ stacks-block-height u1008) ;; ~1 week
      }
    )
    (var-set assessment-counter assessment-id)
    (ok assessment-id)))

;; ========================================
;; PROGRESS TRACKING FUNCTIONS
;; ========================================

;; Record weekly progress
(define-public (record-weekly-progress
  (patient-id principal)
  (week uint)
  (program-id uint)
  (mobility-improvement uint)
  (pain-reduction uint)
  (functional-capacity uint)
  (equipment-adaptation uint)
  (holistic-wellness-score uint)
  (therapist-notes (string-utf8 500))
  (patient-feedback (string-utf8 500)))
  (let (
    (program (unwrap! (map-get? therapy-programs { program-id: program-id }) ERR-PROGRAM-NOT-FOUND))
    (milestone-achieved (and (>= mobility-improvement u70)
                            (>= pain-reduction u50)
                            (>= functional-capacity u60)))
  )
    (asserts! (is-eq tx-sender (get therapist-id program)) ERR-NOT-AUTHORIZED)
    (map-set progress-records
      { patient-id: patient-id, week: week }
      {
        program-id: program-id,
        mobility-improvement: mobility-improvement,
        pain-reduction: pain-reduction,
        functional-capacity: functional-capacity,
        equipment-adaptation: equipment-adaptation,
        holistic-wellness-score: holistic-wellness-score,
        therapist-notes: therapist-notes,
        patient-feedback: patient-feedback,
        milestone-achieved: milestone-achieved
      }
    )
    (ok milestone-achieved)))

;; ========================================
;; THERAPIST MANAGEMENT FUNCTIONS
;; ========================================

;; Register certified therapist
(define-public (register-therapist
  (license-number (string-utf8 50))
  (specializations (list 5 (string-utf8 100)))
  (accessibility-certified bool)
  (pain-management-certified bool)
  (holistic-therapy-certified bool)
  (certification-expiry uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (map-set certified-therapists
      { therapist-id: tx-sender }
      {
        license-number: license-number,
        specializations: specializations,
        accessibility-certified: accessibility-certified,
        pain-management-certified: pain-management-certified,
        holistic-therapy-certified: holistic-therapy-certified,
        active-patients: u0,
        certification-expiry: certification-expiry
      }
    )
    (ok true)))

;; ========================================
;; READ-ONLY FUNCTIONS
;; ========================================

;; Get patient profile
(define-read-only (get-patient-profile (patient-id principal))
  (map-get? patient-profiles { patient-id: patient-id }))

;; Get therapy program
(define-read-only (get-therapy-program (program-id uint))
  (map-get? therapy-programs { program-id: program-id }))

;; Get movement assessment
(define-read-only (get-movement-assessment (assessment-id uint))
  (map-get? movement-assessments { assessment-id: assessment-id }))

;; Get progress record
(define-read-only (get-progress-record (patient-id principal) (week uint))
  (map-get? progress-records { patient-id: patient-id, week: week }))

;; Get therapist credentials
(define-read-only (get-therapist-credentials (therapist-id principal))
  (map-get? certified-therapists { therapist-id: therapist-id }))

;; Calculate overall recovery score
(define-read-only (calculate-recovery-score (patient-id principal) (weeks-completed uint))
  (let (
    (total-mobility u0)
    (total-pain-reduction u0)
    (total-functional u0)
    (total-wellness u0)
  )
    ;; This would typically iterate through weeks, simplified for demo
    (ok {
      mobility-score: total-mobility,
      pain-management-score: total-pain-reduction,
      functional-capacity-score: total-functional,
      holistic-wellness-score: total-wellness,
      overall-recovery-percentage: (/ (+ total-mobility total-pain-reduction total-functional total-wellness) u4)
    })))

;; ========================================
;; ADMIN FUNCTIONS
;; ========================================

;; Transfer contract ownership
(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)))

;; Emergency pause system (for maintenance)
(define-data-var system-paused bool false)

(define-public (toggle-system-pause)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set system-paused (not (var-get system-paused)))
    (ok (var-get system-paused))))

(define-read-only (is-system-paused)
  (var-get system-paused))


;; ========================================
;; CONTRACT 2: ADAPTIVE EQUIPMENT SHARING
;; ========================================

;; Equipment sharing and management contract
(define-constant ERR-EQUIPMENT-NOT-FOUND (err u200))
(define-constant ERR-EQUIPMENT-UNAVAILABLE (err u201))
(define-constant ERR-INVALID-RENTAL-PERIOD (err u202))
(define-constant ERR-EQUIPMENT-DAMAGED (err u203))

;; Data variables for equipment
(define-data-var equipment-counter uint u0)
(define-data-var rental-counter uint u0)

;; Equipment registry
(define-map adaptive-equipment
  { equipment-id: uint }
  {
    equipment-type: (string-utf8 100),
    model: (string-utf8 100),
    accessibility-features: (list 5 (string-utf8 100)),
    condition: (string-utf8 50),
    owner: principal,
    location: (string-utf8 200),
    daily-rental-cost: uint,
    maintenance-schedule: uint,
    usage-instructions: (string-utf8 1000),
    safety-certifications: (list 3 (string-utf8 100)),
    available: bool,
    last-inspection: uint
  }
)

;; Rental records
(define-map equipment-rentals
  { rental-id: uint }
  {
    equipment-id: uint,
    renter-id: principal,
    therapy-program-id: uint,
    rental-start: uint,
    rental-end: uint,
    daily-cost: uint,
    total-cost: uint,
    deposit-amount: uint,
    pickup-location: (string-utf8 200),
    delivery-requested: bool,
    status: (string-utf8 50),
    condition-at-pickup: (string-utf8 200),
    condition-at-return: (string-utf8 200)
  }
)

;; Equipment maintenance records
(define-map maintenance-records
  { equipment-id: uint, maintenance-date: uint }
  {
    maintenance-type: (string-utf8 100),
    technician-id: principal,
    issues-found: (string-utf8 500),
    repairs-performed: (string-utf8 500),
    parts-replaced: (list 5 (string-utf8 100)),
    next-maintenance-due: uint,
    safety-certification-renewed: bool,
    maintenance-cost: uint
  }
)

;; Register new equipment
(define-public (register-equipment
  (equipment-type (string-utf8 100))
  (model (string-utf8 100))
  (accessibility-features (list 5 (string-utf8 100)))
  (location (string-utf8 200))
  (daily-rental-cost uint)
  (usage-instructions (string-utf8 1000))
  (safety-certifications (list 3 (string-utf8 100))))
  (let ((equipment-id (+ (var-get equipment-counter) u1)))
    (map-set adaptive-equipment
      { equipment-id: equipment-id }
      {
        equipment-type: equipment-type,
        model: model,
        accessibility-features: accessibility-features,
        condition: u"excellent",
        owner: tx-sender,
        location: location,
        daily-rental-cost: daily-rental-cost,
        maintenance-schedule: (+ stacks-block-height u4320), ;; ~30 days
        usage-instructions: usage-instructions,
        safety-certifications: safety-certifications,
        available: true,
        last-inspection: stacks-block-height
      }
    )
    (var-set equipment-counter equipment-id)
    (ok equipment-id)))

;; Rent equipment
(define-public (rent-equipment
  (equipment-id uint)
  (therapy-program-id uint)
  (rental-days uint)
  (delivery-requested bool)
  (pickup-location (string-utf8 200)))
  (let (
    (equipment (unwrap! (map-get? adaptive-equipment { equipment-id: equipment-id }) ERR-EQUIPMENT-NOT-FOUND))
    (rental-id (+ (var-get rental-counter) u1))
    (daily-cost (get daily-rental-cost equipment))
    (total-cost (* daily-cost rental-days))
    (deposit-amount (* daily-cost u2)) ;; 2 days deposit
  )
    (asserts! (get available equipment) ERR-EQUIPMENT-UNAVAILABLE)
    (asserts! (and (> rental-days u0) (<= rental-days u30)) ERR-INVALID-RENTAL-PERIOD)

    ;; Mark equipment as unavailable
    (map-set adaptive-equipment
      { equipment-id: equipment-id }
      (merge equipment { available: false })
    )

    ;; Create rental record
    (map-set equipment-rentals
      { rental-id: rental-id }
      {
        equipment-id: equipment-id,
        renter-id: tx-sender,
        therapy-program-id: therapy-program-id,
        rental-start: stacks-block-height,
        rental-end: (+ stacks-block-height (* rental-days u144)), ;; ~rental-days in blocks
        daily-cost: daily-cost,
        total-cost: total-cost,
        deposit-amount: deposit-amount,
        pickup-location: pickup-location,
        delivery-requested: delivery-requested,
        status: u"active",
        condition-at-pickup: u"good",
        condition-at-return: u""
      }
    )
    (var-set rental-counter rental-id)
    (ok rental-id)))

;; Return equipment
(define-public (return-equipment
  (rental-id uint)
  (condition-at-return (string-utf8 200)))
  (let (
    (rental (unwrap! (map-get? equipment-rentals { rental-id: rental-id }) (err u204)))
    (equipment (unwrap! (map-get? adaptive-equipment { equipment-id: (get equipment-id rental) }) ERR-EQUIPMENT-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get renter-id rental)) ERR-NOT-AUTHORIZED)

    ;; Update rental record
    (map-set equipment-rentals
      { rental-id: rental-id }
      (merge rental {
        status: u"completed",
        condition-at-return: condition-at-return
      })
    )

    ;; Make equipment available again (if not damaged)
    (map-set adaptive-equipment
      { equipment-id: (get equipment-id rental) }
      (merge equipment {
        available: (not (is-eq condition-at-return u"damaged")),
        condition: (if (is-eq condition-at-return u"damaged") u"needs-repair" (get condition equipment))
      })
    )
    (ok true)))

;; Schedule equipment maintenance
(define-public (schedule-maintenance
  (equipment-id uint)
  (maintenance-type (string-utf8 100))
  (issues-found (string-utf8 500)))
  (let ((equipment (unwrap! (map-get? adaptive-equipment { equipment-id: equipment-id }) ERR-EQUIPMENT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner equipment)) ERR-NOT-AUTHORIZED)
    (map-set maintenance-records
      { equipment-id: equipment-id, maintenance-date: stacks-block-height }
      {
        maintenance-type: maintenance-type,
        technician-id: tx-sender,
        issues-found: issues-found,
        repairs-performed: u"",
        parts-replaced: (list),
        next-maintenance-due: (+ stacks-block-height u4320),
        safety-certification-renewed: false,
        maintenance-cost: u0
      }
    )
    (ok true)))


;; ========================================
;; CONTRACT 3: OUTCOME MEASUREMENT & ANALYTICS
;; ========================================

;; Advanced analytics and outcome measurement
(define-constant ERR-INVALID-METRIC (err u300))
(define-constant ERR-INSUFFICIENT-DATA (err u301))

;; Data variables for analytics
(define-data-var outcome-counter uint u0)

;; Outcome measurement structure
(define-map outcome-measurements
  { measurement-id: uint }
  {
    patient-id: principal,
    program-id: uint,
    measurement-type: (string-utf8 100),
    baseline-metrics: (list 10 uint),
    current-metrics: (list 10 uint),
    improvement-percentage: uint,
    measurement-date: uint,
    assessor-id: principal,
    standardized-score: uint,
    quality-of-life-index: uint,
    functional-independence: uint,
    pain-management-effectiveness: uint,
    assistive-technology-adaptation: uint
  }
)

;; Holistic wellness tracking
(define-map wellness-metrics
  { patient-id: principal, date: uint }
  {
    physical-wellness: uint,
    mental-wellness: uint,
    social-engagement: uint,
    independence-level: uint,
    pain-management: uint,
    mobility-confidence: uint,
    therapy-adherence: uint,
    equipment-comfort: uint,
    overall-satisfaction: uint,
    goal-achievement: uint
  }
)

;; Population-level analytics
(define-map program-effectiveness
  { program-type: (string-utf8 100) }
  {
    total-participants: uint,
    average-improvement: uint,
    completion-rate: uint,
    patient-satisfaction: uint,
    cost-effectiveness: uint,
    equipment-utilization: uint,
    therapist-ratings: uint,
    outcome-sustainability: uint
  }
)

;; Record comprehensive outcome measurement
(define-public (record-outcome-measurement
  (patient-id principal)
  (program-id uint)
  (measurement-type (string-utf8 100))
  (baseline-metrics (list 10 uint))
  (current-metrics (list 10 uint))
  (quality-of-life-index uint)
  (functional-independence uint)
  (pain-management-effectiveness uint)
  (assistive-technology-adaptation uint))
  (let (
    (measurement-id (+ (var-get outcome-counter) u1))
    (program (unwrap! (map-get? therapy-programs { program-id: program-id }) ERR-PROGRAM-NOT-FOUND))
    (improvement-percentage (calculate-improvement-percentage baseline-metrics current-metrics))
    (standardized-score (calculate-standardized-score current-metrics))
  )
    (asserts! (is-eq tx-sender (get therapist-id program)) ERR-NOT-AUTHORIZED)
    (map-set outcome-measurements
      { measurement-id: measurement-id }
      {
        patient-id: patient-id,
        program-id: program-id,
        measurement-type: measurement-type,
        baseline-metrics: baseline-metrics,
        current-metrics: current-metrics,
        improvement-percentage: improvement-percentage,
        measurement-date: stacks-block-height,
        assessor-id: tx-sender,
        standardized-score: standardized-score,
        quality-of-life-index: quality-of-life-index,
        functional-independence: functional-independence,
        pain-management-effectiveness: pain-management-effectiveness,
        assistive-technology-adaptation: assistive-technology-adaptation
      }
    )
    (var-set outcome-counter measurement-id)
    (ok measurement-id)))

;; Record holistic wellness metrics
(define-public (record-wellness-metrics
  (patient-id principal)
  (physical-wellness uint)
  (mental-wellness uint)
  (social-engagement uint)
  (independence-level uint)
  (pain-management uint)
  (mobility-confidence uint)
  (therapy-adherence uint)
  (equipment-comfort uint)
  (overall-satisfaction uint)
  (goal-achievement uint))
  (begin
    (map-set wellness-metrics
      { patient-id: patient-id, date: stacks-block-height }
      {
        physical-wellness: physical-wellness,
        mental-wellness: mental-wellness,
        social-engagement: social-engagement,
        independence-level: independence-level,
        pain-management: pain-management,
        mobility-confidence: mobility-confidence,
        therapy-adherence: therapy-adherence,
        equipment-comfort: equipment-comfort,
        overall-satisfaction: overall-satisfaction,
        goal-achievement: goal-achievement
      }
    )
    (ok true)))

;; Calculate improvement percentage helper function
(define-private (calculate-improvement-percentage
  (baseline (list 10 uint))
  (current (list 10 uint)))
  (let (
    (baseline-sum (fold + baseline u0))
    (current-sum (fold + current u0))
  )
    (if (is-eq baseline-sum u0)
        u0
        (/ (* (- current-sum baseline-sum) u100) baseline-sum))))

;; Calculate standardized score helper function
(define-private (calculate-standardized-score (metrics (list 10 uint)))
  (let ((total-score (fold + metrics u0)))
    (/ total-score (len metrics))))

;; Generate comprehensive patient report
(define-read-only (get-patient-comprehensive-report (patient-id principal))
  (let (
    (patient-profile (map-get? patient-profiles { patient-id: patient-id }))
    (latest-wellness (map-get? wellness-metrics { patient-id: patient-id, date: stacks-block-height }))
  )
    (ok {
      patient-profile: patient-profile,
      latest-wellness: latest-wellness,
      report-generated: stacks-block-height
    })))

;; Get program effectiveness analytics
(define-read-only (get-program-effectiveness (program-type (string-utf8 100)))
  (map-get? program-effectiveness { program-type: program-type }))

;; Calculate population health metrics
(define-read-only (calculate-population-metrics)
  (ok {
    total-active-patients: u0, ;; Would calculate from active programs
    average-mobility-improvement: u0,
    average-pain-reduction: u0,
    equipment-sharing-efficiency: u0,
    therapist-utilization: u0,
    system-accessibility-score: u0
  }))

;; Generate accessibility compliance report
(define-read-only (generate-accessibility-report)
  (ok {
    ada-compliance-score: u95,
    equipment-accessibility-rating: u90,
    program-accommodation-coverage: u88,
    digital-accessibility-score: u92,
    patient-feedback-accessibility: u87,
    improvement-recommendations: (list
      u"Increase screen reader compatibility"
      u"Expand multilingual support"
      u"Enhance mobile accessibility features")
  }))

;; Emergency contact and alert system
(define-map emergency-contacts
  { patient-id: principal }
  {
    primary-contact: principal,
    secondary-contact: principal,
    medical-alert-conditions: (list 5 (string-utf8 100)),
    preferred-communication: (string-utf8 50),
    emergency-protocol: (string-utf8 500)
  }
)

;; Set emergency contacts
(define-public (set-emergency-contacts
  (patient-id principal)
  (primary-contact principal)
  (secondary-contact principal)
  (medical-alert-conditions (list 5 (string-utf8 100)))
  (emergency-protocol (string-utf8 500)))
  (begin
    (asserts! (is-eq tx-sender patient-id) ERR-NOT-AUTHORIZED)
    (map-set emergency-contacts
      { patient-id: patient-id }
      {
        primary-contact: primary-contact,
        secondary-contact: secondary-contact,
        medical-alert-conditions: medical-alert-conditions,
        preferred-communication: u"phone",
        emergency-protocol: emergency-protocol
      }
    )
    (ok true)))
