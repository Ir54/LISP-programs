;; ===========================================================
;; Project Metrics Computation (Functional Scheme Program)
;; ================================

;; Each task: (Task-ID Planned-Cost Planned-Duration Actual-Cost Actual-Duration Completed?)
(define project-tasks
  '((1 12000 7 10000 6 #t)
    (2 50000 10 75000 14 #t)
    (3 20000 5 0 0 #f)))

;; --------------------------------------------
;; Helper functions for accessing task fields
;; --------------------------------------------
(define (task-id t) (list-ref t 0))
(define (planned-cost t) (list-ref t 1))
(define (planned-duration t) (list-ref t 2))
(define (actual-cost t) (list-ref t 3))
(define (actual-duration t) (list-ref t 4))
(define (completed? t) (list-ref t 5))

;; --------------------------------------------
;; Per-task computations
;; --------------------------------------------
(define (cost-variance t)
  (- (actual-cost t) (planned-cost t)))

(define (schedule-variance t)
  (- (actual-duration t) (planned-duration t)))

(define (cost-performance-index t)
  (if (zero? (planned-cost t))
      0
      (/ (actual-cost t) (planned-cost t))))

(define (schedule-performance-index t)
  (if (zero? (planned-duration t))
      0
      (/ (actual-duration t) (planned-duration t))))

;; --------------------------------------------
;; Recursive utilities
;; --------------------------------------------
(define (count-completed tasks)
  (if (null? tasks)
      0
      (+ (if (completed? (car tasks)) 1 0)
         (count-completed (cdr tasks)))))

(define (task-count tasks)
  (if (null? tasks)
      0
      (+ 1 (task-count (cdr tasks)))))

(define (completion-rate tasks)
  (if (zero? (task-count tasks))
      0
      (/ (count-completed tasks)
         (task-count tasks))))

;; --------------------------------------------
;; Compute all task-level metrics
;; --------------------------------------------
(define (compute-task-metrics t)
  (list
   (list 'Task-ID (task-id t))
   (list 'Cost-Variance (cost-variance t))
   (list 'Schedule-Variance (schedule-variance t))
   (list 'Cost-Performance-Index (cost-performance-index t))
   (list 'Schedule-Performance-Index (schedule-performance-index t))
   (list 'Completed (completed? t))))

(define (compute-all-metrics tasks)
  (if (null? tasks)
      '()
      (cons (compute-task-metrics (car tasks))
            (compute-all-metrics (cdr tasks)))))

;; --------------------------------------------
;; Final project summary
;; --------------------------------------------
(define (project-summary tasks)
  (list
   (list 'All-Tasks (compute-all-metrics tasks))
   (list 'Completion-Rate (completion-rate tasks))))

;; --------------------------------------------
;; Run the computation and display the result
;; --------------------------------------------
(display (project-summary project-tasks))
(newline)
