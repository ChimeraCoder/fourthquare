(ql:quickload 'yason)
(ql:quickload "cxml")
(ql:quickload "drakma")
(ql:quickload "ironclad")
(ql:quickload 'local-time)
(ql:quickload 'lisp-unit)

(load "./bootstrap.lisp")

(defun authorize-uri (client-id client-secret redirect-uri)
  (concatenate 'string "https://foursquare.com/oauth2/authenticate?client_id=" client-id "&response_type=code&redirect_uri=" redirect-uri))

(defun set-token (client-id client-secret redirect-uri code)
  (concatenate 'string "https://foursquare.com/oauth2/access_token?client_id=" client-id "&client_secret=" client-secret "&grant_type=authorization_code&redirect_uri=" redirect-uri "&code=" code ))

(defun json-to-list (str)
    "Parse the json string input and return an equivalent a-list"
      (let ((result (json:decode-json-from-string str)))
              result))


(defun encode-url-params (params)
    "Encode the specified parameter alist as GET values (with Toodledo's format)"
      (let ((key (caar params))
                    (value (cdar params)))
            (if (and key value)
                    (concatenate 'string (string key) "=" (string value) ";" (encode-url-params (cdr params)))
                          'NIL )))

(defun query (authenticator endpoint &optional extra-fields params)
  (let* ((uri
            (concatenate 
                    'string 
                    "https://api.foursquare.com/v2/" 
                    (string endpoint)))
         (response
           (drakma:http-request uri :parameters 
                                (acons "oauth_token" (auth-param authenticator)
                                       (acons "v" "20120324" params)))))
    (progn
      (print uri)
      (json-to-list (octets-to-string response)))))
      

(defun auth-param (authenticator)
  authenticator)

(defstruct user
  authenticator
  data)

(defun find-user (authenticator userid &optional extra-fields params)
  "Return the user associated with the given id."
  (make-user 
    :authenticator 'NIL
    :data (cadr (caddr 
                     (query authenticator 
                            (concatenate 'string "users/" (string userid)) 
                            extra-fields params)))))

