
(defparameter me (find-user *access-token* "self"))

(defparameter my-checkins (recent-checkins me))

(gethash "id" my-checkins)

(hash-keys my-checkins)

(hash-keys (user-data me))
