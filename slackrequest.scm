(use-modules (srfi srfi-1)      (json)
             (rnrs bytevectors) (ice-9 receive) (web client))
(include     "./version.scm")

(define CLIENT_NAME    "slackclient")
(define CLIENT_VERSION       VERSION)

(define* (python-replace init target new #:optional [startIndex 0])
  (define targetIndex (string-contains init target startIndex))

  (if targetIndex
      (let ([targetLen (string-length target)]
            [  initLen (string-length   init)])
        (python-replace
          (string-replace init new targetIndex (+ (if (> targetLen initLen)
                                                      initLen
                                                    targetLen) targetIndex))
          target
          new
          (+ targetIndex (string-length new))))
    init))

(define* (SlackRequest #:optional [proxies #f] [customUserAgent #f])
  ;; Construct the user-agent header with the package
  ;; info, Python version and OS version.
  (define userAgentString (fold
                            (lambda (keyAndVal previous)
                              (string-append previous (cdr keyAndVal) " "))
                            ""
                            (list
                              (cons 'client (string-append/shared
                                              CLIENT_NAME
                                              "/"
                                              CLIENT_VERSION))
                              (cons 'guile  (string-append/shared
                                              "Guile/"
                                              (version)))
                              (cons 'system "Unknown")  ;; Drop if not feasible
                              (cons 'custom (if customUserAgent
                                                (fold
                                                  (lambda (s previous)
                                                    (string-append
                                                      previous
                                                      (substring
                                                        (string-fold
                                                          (lambda (c prev)
                                                            (string-append
                                                              prev
                                                              (string c #\/)))
                                                          ""
                                                          s)
                                                        0
                                                        (1- (*
                                                              (string-length s)
                                                              2)))
                                                      " "))
                                                  ""
                                                  customUserAgent)
                                              customUserAgent)))))

  (lambda (method . xs)
    (case method
      [(#:get-user-agent)    userAgentString]
      [(#:append-user-agent) (let ([name (car xs)] [version (cadr xs)])
                               (SlackRequest
                                 proxies
                                 (if customUserAgent
                                     (append
                                       customUserAgent
                                       (list
                                         (python-replace name    "/" ":")
                                         (python-replace version "/" ":")))
                                   (list (list name version)))))]
      #|
      Perform a POST request to the Slack Web API

      Args:
          token    (string): your authentication token
          request  (string): the method to call from the Slack API.
                             For example: 'channels.list'
          timeout  (number): stop waiting for a response after
                             a given number of seconds
          postData (a-list): key/value arguments to pass for the request.
                             For example: '(("channel" . "CABC12345"))
          domain   (string): if, for some reason, you want to send your
                             request to something other than slack.com
      |#
      [(#:do)
            (let* ([argsLen                                       (length xs)]
                   [token                                            (car xs)]
                   [request  (if (< argsLen 2) "?"               (cadr   xs))]
                   [postData (if (< argsLen 3) (make-hash-table) (caddr  xs))]
                   [timeout  (if (< argsLen 5) "slack.com"       (cadddr xs))]
                   [domain   (if (< argsLen 4) #f                (car (cddddr
                                                                        xs)))]
                   [token    (or token (hash-ref postData 'token))]
                   [files    (let ([pdFile (hash-ref postData 'file)])
                               ;; Pull file out so it isn't JSON encoded like
                               ;; normal fields:
                               ;;
                               ;; Only do this for requests that are UPLOADING
                               ;; files; downloading files use the 'file'
                               ;; argument to point to a File ID.
                               (if (or
                                     (string=? request "files.upload")
                                     pdFile)
                                   (let ([h (make-hash-table)])
                                     (hash-set! h 'file pdFile)
                                     (hash-remove! postData 'file))
                                 #f))])
              (receive (response body)
                  (parameterize ([current-http-proxy proxies])
                    (http-post
                      (string-append "https://" domain "/api/" request)
                      #:headers (list
                                  (cons 'user-agent    userAgentString)
                                  (cons 'Content-Type  "application/json")
                                  (cons 'Authorization (string-append
                                                         "Bearer "
                                                         token)))
                      #:body    (scm->json-string postData)
                      ;; Need to figure out how to do files
                      ;; Need to see if timeouts are feasible
                      ))
                (cons response (json-string->scm (utf8->string body)))))])))
