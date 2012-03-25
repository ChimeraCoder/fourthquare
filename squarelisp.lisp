(ql:quickload 'yason)
(ql:quickload "drakma")
(ql:quickload "ironclad")
(ql:quickload "alexandria")

(load "./bootstrap.lisp")

(defun authorize-uri (client-id client-secret redirect-uri)
  (concatenate 'string "https://foursquare.com/oauth2/authenticate?client_id=" client-id "&response_type=code&redirect_uri=" redirect-uri))

(defun hash-keys (hsh)
  (alexandria.0.dev:hash-table-keys hsh))

(defun auth-param (authenticator)
  authenticator)

(defun set-token (client-id client-secret redirect-uri code)
  (concatenate 'string "https://foursquare.com/oauth2/access_token?client_id=" client-id "&client_secret=" client-secret "&grant_type=authorization_code&redirect_uri=" redirect-uri "&code=" code ))

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
      (json:parse (octets-to-string response)))))

(defstruct user
  authenticator
  data)

(defun find-user (authenticator userid &optional extra-fields params)
  "Return the user associated with the given id."
  (make-user 
    :authenticator 'NIL
    :data (gethash "user"
                   (gethash "response" 
                            (query authenticator 
                                   (concatenate 'string "users/" (string userid)) 
                                   extra-fields params)))))

(defun find-checkin (authenticator checkinid &optional extra-fields params)
  "Return the checkin associated with the given id."
  (gethash "response" (query authenticator
                             (concatenate 'sring "checkins/" (string checkinid))
                             extra-fields params)))

(defun user-checkins (usr)
  (car (get-value (user-data usr) "user" "checkins" "items")))


(defun get-value (data-hash &rest hsh-keys)
  (if (cdr hsh-keys)
   (apply #'get-value (gethash (string (car hsh-keys)) data-hash) (cdr hsh-keys))
    (gethash (string (car hsh-keys)) data-hash)))

(defun symbolize-hash-keys (hsh-table &optional cur remaining )
  (let ((current (if cur
                   cur
                   (make-hash-table)))
        (remaining (if remaining
                     remaining
                     (hash-keys hsh-table))))
    ;;Otherwise, we've already started and are in the middle of the recursion
    (if remaining
      (setf 
        (gethash 
          (car remaining) 
          (symbolize-hash-keys hsh-table current 
                               (cdr remaining))) ;;The (car remaining) will be taken care of, so as long as the rest are, we're fine
        (gethash (car remaining) hsh-table)) ;;Set it to the value that's crrently in the old table
      current)))




