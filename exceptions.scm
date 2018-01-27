(define* (SlackClientError #:optional [msg (string-append
                                             "An error occurred in "
                                             "the SlackClient library")] . xs)
  #|
  Base exception for all errors raised by the SlackClient library
  |#
  (if (null? xs) (error msg) (apply error (cons msg xs))))

(define (ParseResponseError responseBody originalException)
  #|
  Error raised when responses to Web API methods cannot be parsed as valid JSON
  |#
  (SlackClientError (string-append
                      "Slack API response body could not be parsed: "
                      responseBody
                      ". Original exception: "
                      originalException)))

(define* (SlackConnectionError #:key [message ""] [reply #f])
  (if reply (SlackClientError message reply) (SlackClientError message)))

(define* (SlackLoginError      #:key [message ""] [reply #f])
  (if reply (SlackClientError message reply) (SlackClientError message)))
