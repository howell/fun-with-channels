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
;; Game Logic

;; Entities send the game logic movement requests. Requests carry the Id of the
;; entity involved as well as a channel along which the game logic sends back a
;; response.
;; A MoveRequest is one of:
;;   (move-x Id Integer (Channelof MoveResponse))
;;   (move-y Id Integer (Channelof MoveResponse))
;;   (player-can-jump? (Channelof MoveResponse))
(struct move-x (id dx resp-chan) #:transparent)
(struct move-y (id dy resp-chan) #:transparent)
(struct player-can-jump? (resp-chan) #:transparent)

;; a MoveResponse is one of
;;  'ok
;;  (y-collision)
;;  (can-jump)
;;  (cannot-jump)
(struct y-collision () #:transparent)
(struct can-jump () #:transparent)
(struct cannot-jump () #:transparent)

;; a GameEvent is one of
;;   'level-complete for when the player reaches the goal
;;   'defeat for when the player dies
;;   (death) for the death of an entity
(struct death () #:transparent)

;; Pieces of the environment come into being by sending (make-env Rect) to the
;; game-logic process
(struct make-env (r) #:transparent)

;; an Enemy is (enemy Id Rect (Channelof GameEvent)), representing the identity,
;; a channel the game logic will use to notify the enemy of relevant events,
;; location, and shape of an enemy
(struct enemy (id rect chan) #:transparent)

;; Enemies come into being by sending (make-enemy Id (Channelof GameEvent) Rect)
;; to the game-logic process. (death) is sent on the channel if the player kills
;; the enemy
(struct make-enemy (id chan r) #:transparent)

;; a GameState is a
;;   (game-state Rect (Listof Rect) Rect (Hashof Id Enemy) Posn)
;; representing the location of the player, the environment, the location
;; of the goal, the location of each enemy, and the size of the level
(struct game-state (player env goal enemies level-size) #:transparent)

;; a LevelOver is one of
;;  'level-complete
;;  'defeat

(define game-logic-where #f)

;; (Channelof LevelOver)
;; (Channelof Any)
;; (Channelof TickSuscription)
;; (Channelof RenderGamestate)
;; Rect
;; Rect
;; Posn
;;  -> (Channelof MoveRequest)
(define (start-game-logic level-over-chan
                          kill-player-chan
                          clock-chan
                          render-chan
                          player0
                          goal0
                          level-size)
  (define move-requests (make-channel))
  (thread
   (thunk
    (define clock-ticks (make-channel))
    (define unsub-timer (make-channel))
    (channel-put clock-chan (tick-subscription 'game-logic clock-ticks unsub-timer))
    ;; LevelOver (Listof Enemey) -> Void
    (define (shutdown! reason enemies)
      (set! game-logic-where 1)
      (channel-put kill-player-chan 'DDDDDIEEEE!)
      (set! game-logic-where 2)
      (for ([e (in-list enemies)])
        (channel-put (enemy-chan e) reason))
      (set! game-logic-where 3)
      (channel-put unsub-timer (unsubscribe))
      (set! game-logic-where 4)
      (channel-put level-over-chan reason)
      (set! game-logic-where #f))
    (let loop ([gs (game-state player0 '() goal0 (hash) level-size)])
      (set! game-logic-where 0)
      (sync (handle-evt
             clock-ticks
             (lambda (t)
               (channel-put render-chan (render-gamestate gs))
               (loop gs)))
            (handle-evt
             move-requests
             (match-lambda
               [(move-x (== player-id) dx resp-chan)
                ;; x-axis movement is always ok
                (channel-put resp-chan 'ok)
                (match (player-motion-x gs dx)
                  [(? game-state? next-gs)
                   (loop next-gs)]
                  [done
                   (shutdown! done (hash-values (game-state-enemies gs)))])]
               [(move-y (== player-id) dy resp-chan)
                (match (player-motion-y gs dy resp-chan)
                  [(? game-state? next-gs)
                   (loop next-gs)]
                  [done
                   (shutdown! done (hash-values (game-state-enemies gs)))])]
               [(move-x id dx resp-chan)
                (channel-put resp-chan 'ok)
                (match (enemy-motion-x gs id dx)
                  [(? game-state? next-gs)
                   (loop next-gs)]
                  [done
                   (shutdown! done (hash-values (game-state-enemies gs)))])]
               [(move-y id dy resp-chan)
                (match (enemy-motion-y gs id dy resp-chan)
                  [(? game-state? next-gs)
                   (loop next-gs)]
                  [done
                   (shutdown! done (hash-values (game-state-enemies gs)))])]
               [(player-can-jump? resp-chan)
                (define on-top-of-something?
                  (cdr (move-player-y (game-state-player gs)
                                      1
                                      (game-state-env gs))))
                (if on-top-of-something?
                    (channel-put resp-chan (can-jump))
                    (channel-put resp-chan (cannot-jump)))
                (loop gs)]
               [(make-enemy id chan r)
                (loop (add-enemy gs id chan r))]
               [(make-env r)
                (define new-env (cons r (game-state-env gs)))
                (define next-state (struct-copy game-state gs [env new-env]))
                (loop next-state)]))))))
  move-requests)

;; GameState Number return! -> (U GameState LevelOver)
;; move the player along the x-axis
(define (player-motion-x gs dx)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (match-define (posn x-limit y-limit) lsize)
  (define level-rect (rect (posn 0 0) x-limit y-limit))
  (define player-n (car (move-player-x player-old dx env-old)))
  (cond
    [(overlapping-rects? player-n cur-goal)
     'level-complete]
    [(not (overlapping-rects? player-n level-rect))
     'defeat]
    [(hit-enemy? enemies-old player-n)
     'defeat]
    [else
     (game-state player-n env-old cur-goal enemies-old lsize)]))

;; GameState Number (Channelof MoveResponse) ->
;; (U GameState LevelOver)
;; move the player along the y-axis
(define (player-motion-y gs dy resp-chan)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (match-define (posn x-limit y-limit) lsize)
  (define level-rect (rect (posn 0 0) x-limit y-limit))
  (match-define (cons player-n col?) (move-player-y player-old dy env-old))
  (define col-enemies
    (for/list ([e (hash-values enemies-old)]
               #:when (overlapping-rects? player-n (enemy-rect e)))
      e))
  (define enemies-new (hash-remove-enemies enemies-old col-enemies))
  (cond
    [(overlapping-rects? player-n cur-goal)
     (channel-put resp-chan 'ok)
     'level-complete]
    [(not (overlapping-rects? player-n level-rect))
     (channel-put resp-chan 'ok)
     'defeat]
    [(and (not (empty? col-enemies)) (negative? dy))
     (channel-put resp-chan 'ok)
     ;; moved upwards into an enemy
     'defeat]
    [else
     (if col?
         (channel-put resp-chan (y-collision))
         (channel-put resp-chan 'ok))
     (for ([e (in-list col-enemies)])
       (channel-put (enemy-chan e) (death)))
     (game-state player-n env-old cur-goal enemies-new lsize)]))

;; (Hashof Id Enemy) (Listof Enemy) -> (Hashof Id Enemy)
;; remove a bunch of enemies from a hash
(define (hash-remove-enemies h enemies)
  (hash-remove* h (map enemy-id enemies)))

;; (hashof Key Any) (listof Key) -> (hashof Key Any)
;; remove a bunch of keys from a hash
(define (hash-remove* h keys)
  (for/fold ([acc h])
            ([k keys])
    (hash-remove acc k)))

;; (Hashof Id Enemy) Rect -> Boolean
(define (hit-enemy? enemies-old player-n)
  (for/or ([e (in-hash-values enemies-old)])
    (overlapping-rects? player-n (enemy-rect e))))

;; GameState Id Number -> (U GameState 'defeat)
;; move an enemy along the x-axis
(define (enemy-motion-x gs id dx)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (define maybe-enemy (hash-ref enemies-old id #f))
  ;; the enemy might not be in the hash if it was recently killed
  (cond
    [maybe-enemy
     (match-define (enemy _  e-rect e-chan) maybe-enemy)
     (define e-rect-new (car (move-player-x e-rect dx env-old)))
     (cond
       [(overlapping-rects? player-old e-rect-new)
        'defeat]
       [else
        (define enemies-new (hash-set enemies-old id (enemy id e-rect-new e-chan)))
        (game-state player-old env-old cur-goal enemies-new lsize)])]
    [else gs]))

;; GameState Id Number (Channelof MoveResponse) -> (U GameState 'defeat)
(define (enemy-motion-y gs id dy resp-chan)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (define maybe-enemy (hash-ref enemies-old id #f))
  ;; the enemy might not be in the hash if it was recently killed
  (cond
    [maybe-enemy
     (match-define (enemy _ e-rect ge-chan) maybe-enemy)
     (match-define (cons e-rect-new col?) (move-player-y e-rect dy env-old))
     (define enemies-new
       (hash-set enemies-old id (enemy id e-rect-new ge-chan)))
     (define player-collision? (overlapping-rects? player-old e-rect-new))
     (cond
       [(and player-collision? (positive? dy))
        ;; enemy fell on player
        (channel-put resp-chan 'ok)
        'defeat]
       [player-collision?
        ;; enemy moved upward into player
        (channel-put resp-chan 'ok)
        (channel-put ge-chan (death))
        (define enemies-final (hash-remove enemies-new id))
        (game-state player-old env-old cur-goal enemies-final lsize)]
       [else
        (if col?
            (channel-put resp-chan (y-collision))
            (channel-put resp-chan 'ok))
        (game-state player-old env-old cur-goal enemies-new lsize)])]
    [else
     (channel-put resp-chan 'ok)
     gs]))

;; GameState Id Rect (Channelof GameEvent) -> GameState
(define (add-enemy gs id r chan)
  (define old-enemies (game-state-enemies gs))
  (define new-enemies (hash-set old-enemies id (enemy id r chan)))
  (struct-copy game-state gs [enemies new-enemies]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player Avatar

;; Translate keyboard events to commands to the game logic

;; (Channelof KeyboardSubscription)
;; (Channelof MoveRequest)
;; (Channelof TickSubscription)
;; (Channelof Any)
;; -> ??
(define (start-player-avatar keyboard-driver game-logic game-clock kill-player)
  (define dx EFFECTIVE-DX)
  (define gravity EFFECTIVE-GRAVITY)
  (define jump-v EFFECTIVE-JUMP-V)
  (thread
   (thunk
    (define timer-ticks (make-channel))
    (define unsub-timer (make-channel))
    (channel-put game-clock (tick-subscription 'player timer-ticks unsub-timer))
    (define key-events (make-channel))
    (define unsub-keyboard (make-channel))
    (channel-put keyboard-driver
                 (keyboard-subscription key-events unsub-keyboard))
    (define move-response (make-channel))
    (define (shutdown!)
      (channel-put unsub-timer (unsubscribe))
      (channel-put unsub-keyboard (unsubscribe)))
    (define listen-for-death
      (handle-evt kill-player (lambda (_) (shutdown!))))
    (let loop ([left-down? #f]
               [right-down? #f]
               [vy 0])
      (sync
       (handle-evt
        kill-player
        (lambda (k)
          (shutdown!)))
       (handle-evt
        timer-ticks
        (lambda (t)
          (define vx (- (if right-down? dx 0)
                        (if left-down? dx 0)))
          (sync
           listen-for-death
           (handle-evt
            (channel-put-evt
             game-logic
             (move-x player-id vx move-response))
            (lambda (_)
              (channel-get move-response)
              (sync
               listen-for-death
               (handle-evt
                (channel-put-evt game-logic (move-y player-id vy move-response))
                (lambda (_)
                  (define vy-new
                    (match (channel-get move-response)
                      [(y-collision) 0]
                      [_ (min (+ vy gravity) EFFECTIVE-TERMINAL-VELOCITY)]))
                  (loop left-down? right-down? vy-new)))))))))
       (handle-evt
        key-events
        (match-lambda
          [(key-press #\space)
           (sync
            listen-for-death
            (handle-evt
             (channel-put-evt game-logic (player-can-jump? move-response))
             (lambda (_)
               (match (channel-get move-response)
                 [(can-jump) (loop left-down? right-down? jump-v)]
                 [(cannot-jump) (loop left-down? right-down? vy)]))))]
          [(key-press 'left)
           (loop #t right-down? vy)]
          [(key-release 'left)
           (loop #f right-down? vy)]
          [(key-press 'right)
           (loop left-down? #t vy)]
          [(key-release 'right)
           (loop left-down? #f vy)]
          [(? key-event?)
           (loop left-down? right-down? vy)])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Level Managment

;; a Level is a (level Rect (Listof Rect) Rect EnemySpawner Posn)
;; representing the beginning position of the player,
;; the layout of the environment,
;; the initial position of the goal,
;; a procedure that will spawn processes for the initial enemies in the level
;;   when given the pids of the game-logic and game-clock actors
;; and the size of the level as an xy coordinate.
(struct level (player0 env0 goal make-enemies size) #:transparent)

;; an EnemySpawner is a
;;  ((Channelof MoveRequest) (Channelof TickSubscription) -> Any)
;; which effectfully boots enemies and links them to the game logic and
;; game clock

;; (NonemptyListof Level)
;; (Channelof TickSubscription)
;; (Channelof RenderMessage)
;; (Channelof KeyboardSubscription)
;; -> ??
(define (start-level-manager levels game-clock renderer keyboard-driver)
  (thread
   (thunk
    (define level-over (make-channel))
    (let loop ([levels levels])
      (match-define (cons level1 next-levels) levels)
      (let run-current ()
        (define kill-player (make-channel))
        (define game-logic
          (start-game-logic level-over
                            kill-player
                            game-clock
                            renderer
                            (level-player0 level1)
                            (level-goal level1)
                            (level-size level1)))
        (define player
          (start-player-avatar keyboard-driver
                               game-logic
                               game-clock
                               kill-player))
        (load-level! level1 game-logic game-clock)
        (define over (channel-get level-over))
        (match over
          ['defeat
           (run-current)]
          ['level-complete
           (cond
             [(empty? next-levels)
              (channel-put renderer (render-victory))]
             [else
              (loop next-levels)])]))))))

;; Level PID PID -> Void
(define (load-level! lvl game-logic game-clock)
  (match-define (level _ env _ mk-es _) lvl)
  (for ([r (in-list env)])
    (channel-put game-logic (make-env r)))
  (mk-es game-logic game-clock))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Clock

;; a Tick is a (tick) signifying a tick of the game clock
;; a TickSubscription is a
;;  (tick-subscription Any (Channelof Tick) (Channelof Unsubscribe))
;; the first field is a name to associate with the subscription for debugging
;; informing the game clock a Tick along the first channel for every timer
;; tick until an Unsubscribe is sent along the second channel
(struct tick () #:transparent)
(struct tick-subscription (name notify-chan unsub-chan) #:transparent)

(define waiting-on #f)

;; (Channelof SetTimer) Ms -> (Channelof TickSuscription)
(define (start-game-clock timer-chan frame-period-ms)
  (define request-chan (make-channel))
  (thread
   (thunk
    (define timer-elapsed-chan (make-channel))
    (define begin (current-inexact-milliseconds))
    
    ;; subscribers : (Listof (List Any (Channelof Tick) (Channelof Unsubscribe)))
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
            [(tick-subscription name notify-chan unsub-chan)
             (new-sub-loop (cons (list name notify-chan unsub-chan) new-subs))]
            [_ new-subs])))
      ;; notify and/or unsubscribe all subscribers
      (define new-subscribers
        (for/fold ([subs '()])
                  ([chans (sequence-append (in-list subscribers)
                                           (in-list more-subscribers))])
          (match-define (list name notify-chan unsub-chan) chans)
          (set! waiting-on name)
          (sync (handle-evt (channel-put-evt notify-chan (tick))
                            (lambda (e) (cons chans subs)))
                (handle-evt unsub-chan
                            (lambda (e) subs)))))
      (set! waiting-on #f)
      (loop new-subscribers (+ now frame-period-ms)))))
  request-chan)

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
                  (loop (remove chans subscribers)))))))))
  request-chan)

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
;; Enemies

(define enemies-where
  (make-weak-hash))

;; Number Number Number Number (Natural Id (Channelof GameEvent) -> Boolean) -> PID
(define (start-enemy game-logic game-clock x0 y0 w h behavior)
  (define id (gensym 'enemy))
  (thread
   (thunk
    (define clock-tick (make-channel))
    (define unsub-timer (make-channel))
    (define game-event (make-channel))
    (channel-put game-clock (tick-subscription id clock-tick unsub-timer))
    (channel-put game-logic (make-enemy id (rect (posn x0 y0) w h) game-event))
    (define where-am-I (box #f))
    (hash-set! enemies-where id where-am-I)
    (let loop ([n 0])
      (set-box! where-am-I 1)
      (sync
       (handle-evt
        clock-tick
        (lambda (t)
          (set-box! where-am-I 2)
          (cond
            [(behavior n id game-event)
             (loop (add1 n))]
            [else
             (set-box! where-am-I 3)
             (channel-put unsub-timer (unsubscribe))])))
       (handle-evt
        game-event
        (lambda (_)
          (set-box! where-am-I 4)
          (channel-put unsub-timer (unsubscribe))
          (set-box! where-am-I #f))))))))

;; spawn an enemy that travels from (x0, y0) to (x0 + x-dist, y0) then back to
;; (x0, y0) at a rate of dx per clock tick
(define (make-horiz-enemy game-logic game-clock x0 y0 w h x-dist dx0)
  (define dx (/ (* dx0 24) FRAMES-PER-SEC))
  (define THRESHOLD (/ x-dist dx))
  (define move-response (make-channel))
  (start-enemy game-logic game-clock
               x0 y0 w h
               (lambda (n id game-event)
                 (define right? (< (modulo n (floor (* 2 THRESHOLD)))
                                   THRESHOLD))
                 (define motion (if right? dx (- dx)))
                 (sync
                  (handle-evt
                   (channel-put-evt game-logic (move-x id motion move-response))
                   (lambda (_)
                     (channel-get move-response)
                     'continue))
                  (handle-evt
                   game-event
                   (lambda (_)
                     #f))))))

;; spawn an enemy that travels from (x0, y0) to (x0, y0 + y-dist) then back to
;; (x0, y0) at a rate of dy per clock tick
(define (make-vert-enemy game-logic game-clock x0 y0 w h y-dist dy0)
  (define dy (/ (* dy0 24) FRAMES-PER-SEC))
  (define THRESHOLD (/ y-dist dy))
  (define move-response (make-channel))
  (start-enemy game-logic game-clock
               x0 y0 w h
               (lambda (n id game-event)
                 (define up? (< (modulo n (floor (* 2 THRESHOLD))) THRESHOLD))
                 (define motion (if up? dy (- dy)))
                 (sync (handle-evt
                        (channel-put-evt game-logic (move-y id motion move-response))
                        (lambda (_)
                           (channel-get move-response)
                          'continue))
                       (handle-evt
                        game-event
                        (lambda (_)
                          #f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering

;; a RenderMessge is a RenderGamestate or RenderVictory
;; a RenderGamestate message, (render-gamestate GameState), is sent to the
;; renderer to draw the gamestate to the screen.
;; a RenderVictory is (render-victory)
(struct render-gamestate (gs) #:transparent)
(struct render-victory () #:transparent)

;; dc<%> -> (Channelof RenderMessage)
(define (start-renderer dc)
  (define in-chan (make-channel))
  (thread
   (thunk
    (let loop ()
      (match (channel-get in-chan)
        [(render-victory)
         (draw-victory dc)]
        [(render-gamestate gs)
         (draw-game-state dc gs)
         (loop)]))))
  in-chan)

(define (draw-victory dc)
  (big-text dc "Victory!" "green"))

(define (big-text dc text color)
  (send dc suspend-flush)
  (send dc clear)
  (send dc set-text-mode 'solid)
  (send dc set-text-foreground color)
  (define fnt (make-object font% 100 'default))
  (send dc set-font fnt)
  (define tl-x (/ (posn-x canvas-bot-right) 6))
  (define tl-y (/ (posn-y canvas-bot-right) 4))
  (send dc draw-text text tl-x tl-y)
  (send dc resume-flush))

;; DC GameState -> Void
(define (draw-game-state dc gs)
  (match-define (game-state old-player old-env old-goal old-enemies lsize) gs)
  (render-game dc old-player old-env old-goal (hash-values old-enemies) lsize))

(define (star-points scale)
  (map (lambda (pr) (cons (* scale (car pr)) (* scale (cdr pr))))
       `((0 . 10)
         (2 . 6)
         (0 . 4)
         (3 . 4)
         (5 . 0)
         (7 . 4)
         (10 . 4)
         (8 . 6)
         (10 . 10)
         (5 . 7))))

;; drawing-context goal -> void
;; draws the goal as a 50x50 yellow star
(define (draw-goal dc g)
  (match-define (rect (posn x0 y0) _ _) g)
  (send dc set-brush "yellow" 'solid)
  (send dc set-pen "yellow" 1 'solid)
  (send dc set-smoothing 'aligned)
  (send dc draw-polygon (star-points 5) x0 y0))

;; drawing-context rect color -> void
;; draws a solid rectangle
(define (draw-rect dc r color)
  (match-define (rect (posn x0 y0) w h) r)
  (send dc set-brush color 'solid)
  (send dc set-pen color 1 'solid)
  (send dc draw-rectangle x0 y0 w h))

;; drawing-context rect (listof rect) goal (listof enemy) -> void
;; draws the game
(define (draw-game dc player env gl enemies)
  (for ([r env])
    (draw-rect dc r "black"))
  (draw-goal dc gl)
  (for ([e enemies])
    (draw-rect dc (enemy-rect e) "red"))
  (draw-rect dc player "blue"))

;; num num num -> num
;; determine an offset for side-scrolling
(define (scroll-offset player canvas-size level-size)
  (define csize/2 (/ canvas-size 2))
  (cond
    ;; don't scroll when the player is close to the beginning of the level
    [(< (- player csize/2) 0) 0]
    ;; similarly, don't scroll when near the end
    [(> (+ player csize/2) level-size) (- level-size canvas-size)]
    ;; otherwise put the player at the center of the screen
    [else (- player csize/2)]))

(define (render-game canvas-dc player env gl enemies lsize)
  (match-define (posn x-size y-size) canvas-bot-right)
  (match-define (posn player-x player-y) (rect-top-left player))
  (match-define (posn x-limit y-limit) lsize)
  (define src-x (scroll-offset player-x x-size x-limit))
  (define src-y (scroll-offset player-y y-size y-limit))
  (define bitmap (make-object bitmap% x-limit y-limit))
  (define bitmap-dc (send bitmap make-dc))
  (draw-game bitmap-dc player env gl enemies)
  (send canvas-dc suspend-flush)
  (send canvas-dc clear)
  (send canvas-dc draw-bitmap-section bitmap 0 0 src-x src-y x-size y-size)
  (send canvas-dc resume-flush))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Booting the game

(define (run-the-game)
  (define timer-driver (start-timer-driver))
  (define game-clock (start-game-clock timer-driver FRAME-PERIOD))
  (define key-events (make-channel))
  (define keyboard-driver (start-keyboard-driver key-events))
  (define dc (make-frame key-events 600 400))
  (define renderer (start-renderer dc))
  (start-level-manager ALL-LEVELS game-clock renderer keyboard-driver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Level Data

(define (make-player x0 y0)
  (rect (posn x0 y0) 8 32))

(define (make-goal x0 y0)
  (rect (posn x0 y0) 50 50))

(define PLAYER0 (make-player 0 0))
(define GOAL0 (make-goal 900 150))

(define level0
  (level PLAYER0
         (list (rect (posn 0 200) 150 10)
               (rect (posn 400 200) 1000 10)
               (rect (posn 200 178) 50 10)
               (rect (posn 300 150) 50 10))
         GOAL0
         (lambda (game-logic game-clock)
           (make-horiz-enemy game-logic game-clock 0 180 20 20 130 2)
           (make-horiz-enemy game-logic game-clock 200 158 20 20 30 1)
           (make-horiz-enemy game-logic game-clock 300 130 20 20 30 1)
           (make-horiz-enemy game-logic game-clock 400 180 20 20 180 3))
         (posn 1000 400)))

(define GOAL1 (make-goal 500 150))

(define level1
  (level PLAYER0
         (list (rect (posn 0 200) 600 10))
         GOAL1
         (lambda (game-logic game-clock)
           (make-horiz-enemy game-logic game-clock 0 180 20 20 580 4)
           (make-horiz-enemy game-logic game-clock 0 140 20 20 580 8)
           (make-vert-enemy game-logic game-clock 50 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 100 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 150 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 200 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 250 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 300 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 350 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 400 125 20 20 75 4))
         (posn 600 400)))

;; int int int int int nat nat -> (list rect)
;; make a stair case starting at a given position
(define (ascending-stairs x0 y0 hdist vdist w h n)
  (for/list ([i (in-range n)])
    (define dx (* hdist i))
    (define dy (* vdist i))
    (rect (posn (+ x0 dx) (+ y0 dy)) w h)))

(define level2
  (let ([stairs (ascending-stairs (+ 50 50) (- 800 40)
                                  100 -40
                                  50 10
                                  10)]
        [birdies (lambda (game-logic game-clock)
                  (for/list ([i (in-range 5)])
                    (make-vert-enemy game-logic game-clock
                                     (+ 160 (* i 200))
                                     (- 650 (* i 80))
                                     20
                                     20
                                     120
                                     4)))])
    (level (make-player 0 750)
           (flatten (list stairs
                          (rect (posn 0 800) 50 200)))
           (make-goal 1100 950)
           birdies
           (posn 2000 1000))))

(define ALL-LEVELS (list level0 level1 level2))