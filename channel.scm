(define* (Channel server name channelID #:optional [members '()])
  #|
  A Channel represents a public or private Slack Channel instance
  |#
  (lambda (method . xs)
    (case method
      [(#:name=?)          (let ([compareStr (car xs)])
                             (or
                               (string=? name                     compareStr)
                               (string=? (string-append "#" name) compareStr)
                               (string=? channelID                compareStr)))]
      [(#:hash)            (hash channelID 500)]
      [(#:channel->string) #|Handle this later?|#]
      #|
      Sends a message to this Channel.

      Include the parent message's thread_ts
      value in `thread` to send to a thread.

      :Args:
          message         (message)   - the string you'd like to
                                            send to the channel
          thread          (str or #f) - the parent message ID, if
                                            sending to a thread
          reply_broadcast (boolean)   - if messaging a thread, whether to also
                                            send the message back to the channel

      :Returns:
          None (change this, later)
      |#
      [(#:send-message)    (let* ([argsLen          (length xs)]
                                  [message             (car xs)]
                                  [thread         (if (< argsLen 2)
                                                      #f
                                                    (cadr  xs))]
                                  [replyBroadcast (if (< argsLen 3)
                                                      #f
                                                    (caddr xs))])
                             (server #:rtm-send-message
                                       channelID message
                                       thread    replyBroadcast))])))
