(ql:quickload 'yason)
(ql:quickload "drakma")
(ql:quickload "ironclad")
(ql:quickload "alexandria")

(load "./bootstrap.lisp")

(defun authorize-uri (client-id client-secret redirect-uri)
  (concatenate 'string "https://foursquare.com/oauth2/authenticate?client_id=" client-id "&response_type=code&redirect_uri=" redirect-uri))

(defun hash-keys (hsh)
  (alexandria.0.dev:hash-table-keys hsh))

(defun symbolize-hash-keys (hsh-table)
  (let ((new-hash (make-hash-table)))
    (progn 
      (maphash (lambda (x y)
                        (setf 
                          (gethash (read-from-string x) new-hash)
                          y))
                        hsh-table)
       new-hash)))

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
    (json:parse (octets-to-string response))))

(defstruct user
  authenticator
  data)

(defmethod field-val ((usr user) (field symbol))
  (gethash field (symbolize-hash-keys (user-data usr))))

(defun find-user (authenticator userid &optional extra-fields params)
  "Return the user associated with the given id."
  (let ((usr-data (gethash "user"
                           (gethash "response" 
                                    (query authenticator 
                                           (concatenate 'string "users/" (string userid)) 
                                           extra-fields params)))))
    (make-user 
      :authenticator 'NIL
      :data (symbolize-hash-keys usr-data))))

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


(defun recent-checkins (usr)
  (car (gethash "items" (user-field usr 'checkins))))


(defun checkin-field (chckin field)
  (gethash field (symbolize-hash-keys (checkin-data me))))


