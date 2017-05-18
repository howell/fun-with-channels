#lang racket

(provide start-timer-driver
         (struct-out set-timer)
         timer-expired)

(module+ test
  (require rackunit))

;; a SetTimer is a (set-timer Ms (Channelof TimerExpired)) where the Ms is an
;; absolute time at which the timer driver will send a TimerExpired on the given
;; channel
(struct set-timer (ms resp-chan) #:transparent)

;; a TimerExpired is 'timer-expired, sent by (an agent of) the
;; driver to inform of a timer firing
(define timer-expired 'timer-expired)

;; a Ms is a natural number, representing an absolute time in milliseconds

;; -> (Channelof SetTimer)
(define (start-timer-driver)
  (define request-chan (make-channel))
  (thread
   (thunk
    (let loop ()
      (match-define (set-timer ms resp-chan) (channel-get request-chan))
      (spawn-timer ms resp-chan)
      (loop))))
  request-chan)

;; Ms (Channelof TimerExpired) -> Void
(define (spawn-timer ms resp-chan)
  (thread
   (thunk
    (sync (alarm-evt ms))
    (channel-put resp-chan timer-expired))))

(module+ test
  (define request-chan (start-timer-driver))
  (define begin-time (current-inexact-milliseconds))
  (define Δ 500)
  (define ϵ 10)
  (define observation #f)
  (define client
    (thread
     (thunk
      (define response-chan (make-channel))
      (channel-put request-chan (set-timer (+ begin-time Δ) response-chan))
      (channel-get response-chan)
      (set! observation (current-inexact-milliseconds)))))
  (thread-wait client)
  (displayln observation)
  (check-= observation (+ begin-time Δ) ϵ))