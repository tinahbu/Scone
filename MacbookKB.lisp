
;;; Create high-level elements
(new-type {computer} {physical object})
(new-type {hardware} {physical object})
(new-type {software} {information})

;;; Different Computers types
(new-split-subtypes {computer} '({laptop} {desktop}))
(new-type {Macintosh} {computer})
(new-split-subtypes {laptop} '({ChromeBook} {Surface} {Macbook}))
(new-is-a {Macbook} {Macintosh})

;;; Different software types
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
(new-type {version} {string})
(new-type-role {version of software} {software} {version})

;;; Create a new Macbook
(new-indv {Macbook_1} {Macbook} :english '("Mac 1"))
(x-is-the-y-of-z {operating system of Macbook_1} {operating system of Macbook} {Macbook_1})
;;; {operating system of Macbook} cannot be the {operating system of Macbook} of {Macbook}.  Continuing...
(new-is-a {operating system of Macbook_1} {MacOS})
(x-is-the-y-of-z (new-string {"10.9.2"}) {version of software} {operating system of Macbook_1})

;;; Create second individual with different mac os version
(new-indv {Macbook_2} {Macbook} :english '("Mac 2"))
(x-is-the-y-of-z {operating system of Macbook_2} {operating system of computer} {Macbook_2})
(new-is-a {operating system of Macbook_2} {MacOS}) ;;; change parent to {MacOS}
(x-is-the-y-of-z (new-string {"10.9.1"}) {version of software} {operating system of Macbook_2})
