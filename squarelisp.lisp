(ql:quickload 'yason)
(ql:quickload "drakma")
(ql:quickload "ironclad")
(ql:quickload "alexandria")

(load "./bootstrap.lisp")

(defun authorize-uri (client-id client-secret redirect-uri)
  "Generate the authorization URL. Redirect the user (307) to the URL produced"
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
  "Issue a GET request to the URL produced here to receive a JSON response with the access token"
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
      :authenticator authenticator
      :data (symbolize-hash-keys usr-data))))

(defun find-checkin (authenticator checkinid &optional extra-fields params)
  "Return the checkin associated with the given id."
  (let ((checkin-data (gethash "checkin"
                               (gethash "response" 
                                        (query authenticator
                                               (concatenate 'string "checkins/" (string checkinid))
                                                extra-fields params)))))
    (make-checkin 
      :authenticator authenticator
      :data (symbolize-hash-keys checkin-data))))

(defun user-checkins (usr)
  "Given a user object, provide a list of the user's checkins"
  (get-value (user-data usr) 'checkins "items"))

(defun get-value (data-hash &rest hsh-keys)
  (if (cdr hsh-keys)
   (apply #'get-value (gethash (car hsh-keys) data-hash) (cdr hsh-keys))
    (gethash (car hsh-keys) data-hash)))

(defun recent-checkins (usr)
  (car (gethash "items" (gethash 'checkins (user-data usr) ))))



(defstruct checkin
  authenticator
  data)

(defmethod field-val ((checkin checkin) (field symbol))
  (gethash field (symbolize-hash-keys (checkin-data checkin))))

