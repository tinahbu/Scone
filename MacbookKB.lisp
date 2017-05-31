;;; Create high-level elements
(new-type {computer} {physical object})
(new-type {hardware} {physical object})
(new-type {software} {information})

;;; Create different computers types
(new-split-subtypes {computer} '({laptop} {desktop}))
(new-type {Macintosh} {computer})
(new-split-subtypes {laptop} '({ChromeBook} {Surface} {Macbook}))
(new-is-a {Macbook} {Macintosh})

;;; Create different software types
(new-type {operating system} {software})
(new-split-subtypes {operating system}  '({MacOS} {Linux} {Windows}))

;;; Create roles of computer
(new-type-role {hardware of computer} {computer} {hardware})
(new-type-role {software of computer} {computer} {software})
(new-type-role {operating system of computer} {computer} {operating system})

;;; Specify that the OS of Macbook can only be MacOS
(x-is-the-y-of-z {operating system of Macbook} {operating system of computer} {Macbook})
(new-is-a {operating system of Macbook} {MacOS})

;;; Version role of software
(new-type {version} {thing})
(new-type-role {version of software} {software} {version})

;;; Create a new Macbook
(new-indv {Macbook_1} {Macbook} :english '("Mac 1"))
(x-is-the-y-of-z {operating system of Macbook_1} {operating system of computer} {Macbook_1})
(new-is-a {operating system of Macbook_1} {MacOS})
(x-is-the-y-of-z (new-string {"10.9.2"}) {version of software} {operating system of Macbook_1})

;;; Create second individual with different mac os version
(new-indv {Macbook_2} {Macbook} :english '("Mac 2"))
(x-is-the-y-of-z {operating system of Macbook_2} {operating system of computer} {Macbook_2})
(new-is-a {operating system of Macbook_2} {MacOS}) ;;; change parent to {MacOS}
(x-is-the-y-of-z (new-string {"10.9.1"}) {version of software} {operating system of Macbook_2})

;;; Testing

;;; extracts the value from the iname 先別用這個
;;(node-value {version of MacOS})
;;; can-x-be-the-y-of-z? returns T or NIL (unhappy split or constraint)
(can-x-be-the-y-of-z? {MacOS} {operating system of computer} {Macbook}) ;; T
(can-x-be-the-y-of-z? {Linux} {operating system of computer} {Macbook}) ;; NIL
(can-x-be-the-y-of-z? {Windows} {operating system of computer} {Macbook}) ;; NIL
(is-x-a-y? {operating system of computer} {operating system}) ;;; YES
;;; the-x-of-y, query a equal relationship or a proper node?
(the-x-of-y {operating system of computer} {Macbook}) ;;; {operating system of Macbook}
(the-x-of-y {operating system of computer} {computer}) ;;; NIL

(is-x-a-y? {operating system of Macbook} {Linux}) ;;; NO
(is-x-a-y? {operating system of Macbook} {MacOS}) ;;; YES
(is-x-a-y? {operating system of Macbook} {Windows}) ;;; NO

;;; query individual node's OS version number
(the-x-of-y {version of software} {operating system of Macbook_1}) ;;; 10.9.2
(the-x-of-y {version of software} {operating system of Macbook_2}) ;;; 10.9.1

(the-x-of-y {operating system of Macbook} {Macbook_1}) ;;; 
(the-x-of-y {operating system of computer} {Macbook_1}) ;;; {}
