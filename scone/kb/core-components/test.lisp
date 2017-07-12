
;;; The HTTP clients in the (1) httplib, (2) urllib, (3) urllib2, and (4) xmlrpclib libraries in 
;;; CPython (aka Python) 2.x before 2.7.9 and 3.x before 3.4.3, 
;;; when accessing an HTTPS URL, do not (a) check the certificate against a trust store or verify 
;;; that the server hostname matches a domain name in the subject's (b) Common Name or (c) subjectAltName 
;;; field of the X.509 certificate, which allows man-in-the-middle attackers to spoof SSL servers via 
;;; an arbitrary valid certificate.	
;;; Publish Date : 2014-12-12	Last Update Date : 2017-06-30


;;; Create New User Groups
(new-type {frontend developer} {user})
(new-type {backend developer} {user})
(new-type {data scientist} {user})

;;; Create New Individual Users
(new-indv {user 1} {user})
(x-is-a-y-of-z {user 1} {member of user} {frontend developer})
(new-indv {user 2} {user})
(x-is-a-y-of-z {user 2} {member of user} {backend developer})
(new-indv {user 3} {user})
(x-is-a-y-of-z {user 3} {member of user} {data scientist})

;;;
;;; TASK
;;;
;;; Create New Individual Tasks
(new-indv {Shopping cart development} {task})
(new-indv {CNN for product recommendation} {task})
(new-indv {Apache server configure} {task})

;;; Create {performs} Relations
(new-statement {user 1} {is performing} {Apache server configure})
(new-statement {user 1} {is performing} {Shopping cart development})
(new-statement {user 3} {is performing} {CNN for product recommendation})

;;; Create instances of {software resources} then Create {requires} Relations
(new-statement {CNN for product recommendation} {requires} (new-indv NIL {Expresso}))
(new-statement {CNN for product recommendation} {requires} (new-indv NIL {Python-3.0}))
(new-statement {CNN for product recommendation} {requires} (new-indv NIL {httplib}))
(new-statement {Shopping cart development} {requires} (new-indv NIL {tornado}))
(new-statement {Shopping cart development} {requires} (new-indv NIL {Laravel}))
(new-statement {Shopping cart development} {requires} (new-indv NIL {Node.js}))
(new-statement {Apache server configure} {requires} (new-indv NIL {Apache}))

;;; Create {is authorized to execute} Relations
(new-statement {user} {is authorized to execute} {python})
(new-statement {data scientist} {is authorized to execute} {Expresso})
(new-statement {backend developer} {is authorized to execute} {tornado})
(new-statement {backend developer} {is authorized to execute} {Apache})

;;; Create {depends on} Relations
(new-statement {Node.js} {depends on} {path})
(new-statement {Node.js} {depends on} {express-session})
(new-statement {Node.js} {depends on} {express})
(new-statement {Node.js} {depends on} {http-parser})
(new-statement {python} {depends on} {httplib})
(new-statement {python} {depends on} {urllib})
(new-statement {python} {depends on} {urllib2})
(new-statement {python} {depends on} {xmlrpclib})


(new-indv {Python-3.0 of CMU MITS} {Python-3.0})
(new-statement {Python-3.0 of CMU MITS} {depends on} {httplib})











