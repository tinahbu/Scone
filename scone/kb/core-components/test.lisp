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

;;; 
;;; USER Individuals
;;;
;;; Create New Individual Users
(new-indv {user 1} {user})
(x-is-a-y-of-z {user 1} {member of user} {frontend developer})
(new-indv {user 2} {user})
(x-is-a-y-of-z {user 2} {member of user} {backend developer})
;;; Test User for Operating System Version Category & Check
(new-indv {user 3} {user})
(x-is-a-y-of-z {user 3} {member of user} {data scientist})
(x-is-the-y-of-z (new-indv NIL {MacOS 10.6}) {os of user} {user 3})

;;; Test User for CPU, GPU brand & version check
(new-indv {user 6} {user})
(x-is-a-y-of-z (new-indv NIL {Intel Core CPU_i5}) {processor of user} {user 6})
(x-is-a-y-of-z (new-indv NIL {Intel Core CPU_i7}) {processor of user} {user 6})
(x-is-a-y-of-z (new-indv NIL {AMD Radeon RX_580}) {processor of user} {user 6})

;;;
;;; TASK Individuals
;;;
;;; Create New Individual Tasks
(new-indv {Shopping cart development} {task})
(new-indv {CNN for product recommendation} {task})
(new-indv {Apache server configure} {task})
(new-indv {VR Game Development} {task})

;;; Create {performs} Relations
(new-statement {user 1} {is performing} {Apache server configure})
(new-statement {user 1} {is performing} {Shopping cart development})
(new-statement {user 3} {is performing} {CNN for product recommendation})
(new-statement {user 3} {is performing} {VR Game Development} )

;;; Create instances of {software resources} then Create {requires} Relations
(new-statement {CNN for product recommendation} {requires software} (new-indv NIL {Expresso}))
(new-statement {CNN for product recommendation} {requires software} (new-indv NIL {Python_3.0}))
(new-statement {CNN for product recommendation} {requires software} (new-indv NIL {httplib}))
(new-statement {Shopping cart development} {requires software} (new-indv NIL {tornado}))
(new-statement {Shopping cart development} {requires software} (new-indv NIL {Laravel}))
(new-statement {Shopping cart development} {requires software} (new-indv NIL {Node.js}))
(new-statement {Apache server configure} {requires software} (new-indv NIL {Apache}))
(new-statement {VR Game Development} {requires processor} (new-indv NIL {Nvidia GeForce GTX_1060}))
(new-statement {VR Game Development} {requires processor} (new-indv NIL {Intel Core CPU_i5}))
(new-statement {VR Game Development} {requires operating system} (new-indv NIL {MacOS 10.2}))

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





