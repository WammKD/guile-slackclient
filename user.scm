(define (User server name userID realName tz eMail)
  (lambda (method . xs)
    (case method
      [(#:equal?)       (let ([compareStr (car xs)])
                          (or
                            (string=? compareStr userID)
                            (string=? compareStr   name)))]
      [(#:hash)                         (hash userID 5000)]
      [(#:user->string)             #|Handle this later?|#]
      [(#:get-server)                               server]
      [(#:get-name)                                   name]
      [(#:get-userID)                               userID]
      [(#:get-realName)                           realName]
      [(#:get-tz)                                       tz]
      [(#:get-eMail)                                 eMail])))
