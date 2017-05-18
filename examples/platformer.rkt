#lang racket

(require "../drivers/timer.rkt")
(require "../../platformer-lib/platform_lib.rkt")
(require racket/gui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pub/Sub

;; an Unsubscribe is (unsubscribe), informing the recepient to stop sennding
;; new notifications on an already established communication channel
(struct unsubscribe () #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entities, Data

;; an Id is a Symbol, representing the identitiy of an entity in the game
;; n.b. distinct from PIDs!
(define player-id 'player)

;; an Enemy is (enemy Id PID Rect), representing the identity, controlling PID,
;; location, and shape of an enemy
(struct enemy (id pid rect) #:transparent)

(struct death () #:transparent)
(struct y-collision () #:transparent)

(define FRAMES-PER-SEC 30)
(define FRAME-PERIOD (floor (/ 1000 FRAMES-PER-SEC)))

(define GRAVITY-PER-SEC 6)
(define JUMP-V-PER-SEC -200)

(define EFFECTIVE-GRAVITY (/ GRAVITY-PER-SEC FRAMES-PER-SEC))
(define EFFECTIVE-JUMP-V (/ JUMP-V-PER-SEC FRAMES-PER-SEC))

(define TERMINAL-VELOCITY-PER-SEC 200)
(define EFFECTIVE-TERMINAL-VELOCITY
  (/ TERMINAL-VELOCITY-PER-SEC FRAMES-PER-SEC))

(define DX-PER-SEC 75)
(define EFFECTIVE-DX (/ DX-PER-SEC FRAMES-PER-SEC))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Clock

;; a Tick is a (tick) signifying a tick of the game clock
;; a TickSubscription is a
;;  (tick-subscription (Channelof Tick) (Channelof Unsubscribe))
;; informing the game clock a Tick along the first channel for every timer
;; tick until an Unsubscribe is sent along the second channel
(struct tick () #:transparent)
(struct tick-subscription (notify-chan unsub-chan) #:transparent)

;; (Channelof SetTimer) Ms -> (Channelof TickSuscription)
(define (start-game-clock timer-chan frame-period-ms)
  (define request-chan (make-channel))
  (thread
   (thunk
    (define timer-elapsed-chan (make-channel))
    (define begin (current-inexact-milliseconds))
    
    ;; subscribers : (Listof (Cons (Channelof Tick) (Channelof Unsubscribe)))
    (let loop ([subscribers '()]
               [now begin])
      (channel-put timer-chan
                   (set-timer (+ now frame-period-ms) timer-elapsed-chan))
      ;; wait for the timer to tick
      (channel-get timer-elapsed-chan)
      ;; check for new subscribers
      (define more-subscribers
        (let new-sub-loop ([new-subs '()])
          (match (channel-try-get request-chan)
            [(tick-subscription notify-chan unsub-chan)
             (new-sub-loop (cons (cons notify-chan unsub-chan) new-subs))]
            [_ new-subs])))
      ;; notify and/or unsubscribe all subscribers
      (define new-subscribers
        (for/fold ([subs '()])
                  ([chans (sequence-append (in-list subscribers)
                                           (in-list more-subscribers))])
          (match-define (cons notify-chan unsub-chan) chans)
          (sync (handle-evt (channel-put-evt notify-chan (tick))
                            (lambda (e) (cons chans subs)))
                (handle-evt unsub-chan
                            (lambda (e) subs)))))
      (loop new-subscribers (+ now frame-period-ms)))))
  request-chan)

#|

(define timer-chan (start-timer-driver))
(define game-clock-chan (start-game-clock timer-chan 1000))

(define (client n)
  (thread
   (thunk
    (define tick-chan (make-channel))
    (define unsub-chan (make-channel))
    (channel-put game-clock-chan (tick-subscription tick-chan unsub-chan))
    (let loop ([m 0])
      (cond
        [(< m n)
         (channel-get tick-chan)
         (printf "~a\n" m)
         (loop (add1 m))]
        [else
         (channel-put unsub-chan (unsubscribe))
         (when (positive? n)
           (client (sub1 n)))])))))
(client 10)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Driver

;; a KeyEvent is one of (key-press KeyCode) or (key-release KeyCode) signifying
;; the press and release of a key, respectively.
;; a KeyCode is as defined by the gui
(struct key-press (key) #:transparent)
(struct key-release (key) #:transparent)

;; Any -> Bool
(define (key-event? x)
  (or (key-press? x) (key-release? x)))

;; a KeyboardSubscription is a
;;  (keyboard-subscription (Channelof KeyEvent) (Channelof Unsubscribe))
;; informing the keyboard driver to send KeyEvent messages along the first
;; channel until an Unsubscribe is sent on the second
(struct keyboard-subscription (key-chan unsub-chan) #:transparent)

;; (Channelof KeyEvent) -> (Channelof KeyboardSubscription)
(define (start-keyboard-driver key-event-chan)
  (define request-chan (make-channel))
  (thread
   (thunk
    ;; subscribers : (Listof (Cons (Channelof KeyEvent)
    ;;                             (Channelof Unsubscribe)))
    (let loop ([subscribers '()])
      (apply sync
             (handle-evt
              key-event-chan
              (lambda (ke)
                (loop (notify-subscribers ke subscribers))))
             (handle-evt
              request-chan
              (lambda (r)
                (match-define (keyboard-subscription key-chan unsub-chan) r)
                (loop (cons (cons key-chan unsub-chan) subscribers))))
             (for/list ([chans (in-list subscribers)])
               (define unsub-chan (cdr chans))
               (handle-evt
                unsub-chan
                (lambda (u)
                  (loop (remove chans subscribers))))))))))

;; KeyEvent (Listof (Cons (Channelof KeyEvent) (Channelof Unsubscribe))) ->
;; (Listof (Cons (Channelof KeyEvent) (Channelof Unsubscribe)))
(define (notify-subscribers ke subscribers)
  (for/fold ([still-subscribed '()])
            ([chans (in-list subscribers)])
    (match-define (cons key-chan unsub-chan) chans)
    (sync (handle-evt
           (channel-put-evt key-chan ke)
           (lambda (x)
             (cons chans still-subscribed)))
          (handle-evt
           unsub-chan
           (lambda (u)
             still-subscribed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gui stuff

(define game-canvas%
  (class canvas%
    (init-field key-handler)
    (define/override (on-char event)
      (define key-code (send event get-key-code))
      (define release-code (send event get-key-release-code))
      (cond
        [(release? key-code) (key-handler (key-release release-code))]
        [else (key-handler (key-press key-code))]))
    (super-new)))

(define (space? key)
  (equal? key #\space))

(define (release? key)
  (equal? key 'release))

(define (arrow? key)
  (match key
    [(or 'left 'right 'up 'down) #t]
    [_ #f]))

;; global (mutable) variable with the canvas's bottom-right posn 
(define canvas-bot-right #f)

;; (Channelof KeyEvent) PositiveInteger PositiveInteger -> dc<%>
(define (make-frame key-channel width height)
  (parameterize ((current-eventspace (make-eventspace)))
    (define frame (new frame%
                       [label "My Frame"]
                       [width width]
                       [height height]))
    (define canvas
      (new game-canvas%
           [parent frame]
           ;; TODO - really seems like blocking here could cause trouble
           [key-handler (lambda (x) (channel-put key-channel x))]))
    (send canvas focus)
    (send frame show #t)
    (define-values (x-max y-max) (send canvas get-client-size))
    (set! canvas-bot-right (posn x-max y-max))
    (send canvas get-dc)))
