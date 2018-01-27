(use-modules (srfi srfi-1)        (web response)  (web uri))
(include     "./slackrequest.scm" "./channel.scm"
             "./user.scm"         "./util.scm"    "./exceptions.scm")

(define* (Server token #:optional [connect #t] [proxies #f] [vars '()])
  #|
  The Server object owns the websocket connection and all attached channel
  information.
  |#
  (define varsLen (length vars))
  (fil
  (define (find-channel chnls name)
    (fold
      (lambda (chnl previous)
        (cond
         [(list? chnl)         (append (find-channel chnl name) previous)]
         [(chnl #:name=? name) (cons   chnl                     previous)]
         [else                                                   previous]))
      chnls))

  (let ([username     (if (< varsLen 1) #f                (list-ref vars 0))]
        [domain       (if (< varsLen 2) #f                (list-ref vars 1))]
        [loginData    (if (< varsLen 3) #f                (list-ref vars 2))]
        [websocket    (if (< varsLen 4) #f                (list-ref vars 3))]
        [connected    (if (< varsLen 5) #f                (list-ref vars 4))]
        [wsURL        (if (< varsLen 6) #f                (list-ref vars 5))]
        [apiRequester (if (< varsLen 7) (SlackRequest
                                          proxies)        (list-ref vars 6))]
        [users        (if (< varsLen 8) (make-hash-table) (list-ref vars 7))]
        [channels     (if (< varsLen 9) '()               (list-ref vars 8))])
    (define (parseChannelData channelData server)
      (reverse (fold (lambda (channel previous)
                       (let ([channelID (hash-ref channel "id")])
                         (unless (hash-ref channel "name")
                           (hash-set! channel "name"    channelID))
                         (unless (hash-ref channel "members")
                           (hash-set! channel "members" '()))

                         (if (null? (find-channel channels channelID))
                             (cons (Channel
                                     server
                                     (hash-ref channel "name")
                                     channelID
                                     (hash-ref channel "members")) previous)
                           previous))) '() channelData)))
    (define (parseUserData userData server)
      (for-each
        (lambda (user)
          (let ([userID  (hash-ref user      "id")]
                [profile (hash-ref user "profile")])
            (unless (hash-ref user "tz")
              (hash-set! user "tz" "unknown"))
            (unless (hash-ref user "real_name")
              (hash-set! user "real_name" (hash-ref user "name")))
            (unless (hash-ref (hash-ref user "profile") "email")
              (hash-set! profile "email" ""))

            (hash-set! users userID (User
                                      server
                                      (hash-ref user    "name")
                                      userID
                                      (hash-ref user    "real_name")
                                      (hash-ref user    "tz")
                                      (hash-ref profile "email")))))
        userData))

    (define (connectSlackWebsocket wURL)
      #|
      Uses HTTP proxy if available
      |#
      (let ([pred (and proxies (string=? (substring proxies 0 7) "http://"))])
        (

    (define (parseSlackLoginData loginData useRtmStart nwu)
      (let* ([tmpVars (list
                        (hash-ref (hash-ref loginData "self") "name")
                        (hash-ref (hash-ref loginData "team") "domain")
                        loginData
                        websocket
                        connected
                        nwu
                        apiRequester)]
             [usrChan (list users channels)]
             [serv    (Server connect proxies (append tmpVars usrChan))])
        (Server
          connect
          proxies
          (append
            tmpVars
            (if useRtmStart
                (list
                  (begin
                    (parseUserData (hash-ref loginData "users") serv)
                    users)
                  (append
                    channels
                    (parseChannelData (hash-ref loginData "channels") serv)
                    (parseChannelData (hash-ref loginData "groups")   serv)
                    (parseChannelData (hash-ref loginData "ims")      serv)))
              usrChan)))))

    (lambda (method . xs)
      (case method
        [(#:rtmConnect)
              (let* ([argsLen                            (length xs)]
                     [reconnect   (if (< argsLen 1) #f  (car    xs))]
                     [timeout     (if (< argsLen 2) #f  (cadr   xs))]
                     [useRtmStart (if (< argsLen 3) #t  (caddr  xs))]
                     [kwargs      (if (< argsLen 4) '() (cadddr xs))]
                     ;; rtm.start returns user and channel
                     ;; info, rtm.connect does not.
                     [replyRB     (apiRequester #:do token  (if useRtmStart
                                                                "rtm.start"
                                                              "rtm.connect")
                                                     kwargs timeout)])
                (if (not (= (response-code (car replyRB)) 200))
                    (SlackConnectionError #:reply replyRB)
                  (if (hash-ref (cdr replyRB) "ok")
                      (let* ([new_wsURL (hash-ref (cdr replyRB) "url")]
                             [websock   (connectSlackWebsocket new_wsURL)])
                        (when (not reconnect)
                          (parseSlackLoginData
                            (cdr replyRB)
                            useRtmStart
                            new_wsURL)))
                    (SlackLoginError #:reply replyRB))))]
        []))
  )
