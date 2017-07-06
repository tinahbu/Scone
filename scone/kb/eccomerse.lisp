;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Minimal knowledge base that models users, tasks they are assigned to, and 
;;; softwares/libraries these tasks would require.
;;; Some Lisp functions that implement vulnerability checking and access controll
;;; checking are also included.
;;; 
;;; Author & Maintainer: Tianhong Bu, Yun-Tieh Chen, Qiaoyu Deng, Ang Li from CMU MITS program
;;; ***************************************************************************
;;; Copyright (C) 2017, Carnegie Mellon University.
;;;
;;; *************************************************************

;;; 3 Main Components as type node
(new-type {user} {person})
(new-type {task} {action})
(new-type {software resources} {thing})

;;;
;;; USER
;;;
;;; Add Role Node to {user}
(new-indv-role {userid of user} {user} {string})
(new-indv-role {username of user} {user} {string})
(new-indv-role {email of user} {user} {string})

;;; Create New User Groups
(new-type {frontend developer} {user})
(new-type {backend developer} {user})
(new-type {data scientist} {user})

;;; Create New Individual Users
(new-indv {user 1} {frontend developer}) 
(new-indv {user 2} {backend developer})
(new-indv {user 3} {data scientist})

;;;
;;; TASK
;;;
;;; Create New Individual Tasks
(new-indv {Shopping cart development} {task})
(new-indv {CNN for product recommendation} {task})
(new-indv {Apache server configure} {task})

;;;
;;; SOFTWARE RESOURCES
;;;
;;; Add Role Node to {software resources}
(new-type-role {version of software resources} {software resources} {string})

;;; Create New Software resources Type Node
;;; default is software resources
;;; Sorted by first alphabet 
(new-type {Apache} {software resources})
(new-type {ANSI C} {software resources})
(new-type {BLAS} {software resources})
(new-type {Boost} {software resources})
(new-type {Boost 1.55} {Boost})
(new-type {Caffe} {software resources})
(new-type {Composer} {software resources})
(new-type {cookie} {software resources})
(new-type {C} {software resources})
(new-type {coreutils} {software resources})
(new-type {CUDA} {software resources})
(new-type {CUDA 5.0} {CUDA})
(new-type {CUDA 5.5} {CUDA})
(new-type {CUDA 6.0} {CUDA})
(new-type {CUDA 7.0} {CUDA})
(new-type {C++} {software resources})
(new-type {debug} {software resources})
(new-type {Expresso} {software resources})
(new-type {express} {software resources})
(new-type {express-session} {software resources})
(new-type {eigen} {software resources})
(new-type {Fortran} {software resources})
(new-type {gflags} {software resources})
(new-type {glog} {software resources})
(new-type {gmp} {software resources})
(new-type {GNU make} {software resources})
(new-type {hdf5} {software resources})
(new-type {http-parser} {software resources})
(new-type {JavaScript} {software resources})
(new-type {Laravel} {software resources})
(new-type {libaio} {software resources})
(new-type {libnuma} {software resources})
(new-type {libuv} {software resources})
(new-type {leveldb} {software resources})
(new-type {MySQL} {software resources})
(new-type {MongoDB} {software resources})
(new-type {Node.js} {software resources})
(new-type {npm} {software resources})
(new-type {OpenCV} {software resources})
(new-type {OpenCV 2.4} {OpenCV})
(new-type {OpenCV 3.0} {OpenCV})
(new-type {OpenSSL} {software resources})
(new-type {tornado} {software resources})
(new-type {path} {software resources})
;;; (new-type {process} {software resources}) ;;;Error, process has already been used in Scone
(new-type {protobuf} {software resources})
(new-type {python} {software resources})
(new-type {python 2.7} {python})
(new-type {python 3.0} {python})
(new-type {PHP} {software resources})
(new-type {snappy} {software resources})
(new-type {util} {software resources})
(new-type {V8} {software resources})

(new-type {TensorFlow} {software resources})
(new-type {OpenMPI} {software resources})
(new-type {LLVM} {software resources})
(new-type {Numpy} {software resources})
(new-type {Hadoop} {software resources})
(new-type {JDK} {software resources})
(new-type {Maven} {software resources})
(new-type {C Std library} {software resources})

;;; Assign Version Value to Software's {version of software resources} role node
(x-is-the-y-of-z (new-string {"1.55"}) {version of software resources} {Boost 1.55})
(x-is-the-y-of-z (new-string {"5.0"}) {version of software resources} {CUDA 5.0})
(x-is-the-y-of-z (new-string {"5.5"}) {version of software resources} {CUDA 5.5})
(x-is-the-y-of-z (new-string {"6.0"}) {version of software resources} {CUDA 6.0})
(x-is-the-y-of-z (new-string {"7.0"}) {version of software resources} {CUDA 7.0})
(x-is-the-y-of-z (new-string {"2.4"}) {version of software resources} {OpenCV 2.4})
(x-is-the-y-of-z (new-string {"3.0"}) {version of software resources} {OpenCV 3.0})
(x-is-the-y-of-z (new-string {"2.7"}) {version of software resources} {python 2.7})
(x-is-the-y-of-z (new-string {"3.0"}) {version of software resources} {python 3.0})

;;; 
;;; RELATIONS
;;; 
;;; Create 4 relations between main components
(new-relation {is performing} :a-type-of {user} :b-type-of {task} :transitive T)
(new-relation {requires} :a-type-of {task} :b-type-of {software resources} :transitive T)
(new-relation {is authorized to execute} :a-type-of {user} :b-type-of {software resources} :transitive T)
(new-relation {depends on} :a-type-of {software resources} :b-type-of {software resources} :transitive T)

;;; Create {performs} Relations
(new-statement {user 1} {is performing} {Apache server configure})
(new-statement {user 1} {is performing} {Shopping cart development})
(new-statement {user 2} {is performing} {CNN for product recommendation})

;;; Create instances of {software resources} then Create {requires} Relations
(new-statement {CNN for product recommendation} {requires} (new-indv NIL {Expresso}))
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
(new-statement {BLAS} {depends on} {Fortran})
(new-statement {Boost} {depends on} {C++})
(new-statement {Caffe} {depends on} {OpenCV 2.4})
(new-statement {Caffe} {depends on} {hdf5})
(new-statement {Caffe} {depends on} {gflags})
(new-statement {Caffe} {depends on} {CUDA 6.0})
(new-statement {Caffe} {depends on} {CUDA 7.0})
(new-statement {Caffe} {depends on} {BLAS})
(new-statement {Caffe} {depends on} {Boost 1.55})
(new-statement {Caffe} {depends on} {protobuf})
(new-statement {Caffe} {depends on} {leveldb})
(new-statement {coreutils} {depends on} {gmp})
(new-statement {cookie} {depends on} {debug})
(new-statement {express} {depends on} {cookie})
(new-statement {Expresso} {depends on} {python 3.0})
(new-statement {Expresso} {depends on} {protobuf})
(new-statement {Expresso} {depends on} {Caffe})
(new-statement {express-session} {depends on} {cookie})
(new-statement {eigen} {depends on} {C++})
(new-statement {gflags} {depends on} {C++})
(new-statement {glog} {depends on} {C++})
(new-statement {gmp} {depends on} {C++})
(new-statement {http-parser} {depends on} {C})
(new-statement {http-parser} {depends on} {coreutils})
(new-statement {Laravel} {depends on} {OpenSSL})
(new-statement {Laravel} {depends on} {PHP})
(new-statement {Laravel} {depends on} {Composer})
(new-statement {leveldb} {depends on} {snappy})
(new-statement {libuv} {depends on} {C})
(new-statement {MySQL} {depends on} {OpenSSL})
(new-statement {MySQL} {depends on} {libaio})
(new-statement {MySQL} {depends on} {libnuma})
(new-statement {MongoDB} {depends on} {snappy})
(new-statement {Node.js} {depends on} {path})
(new-statement {Node.js} {depends on} {express-session})
(new-statement {Node.js} {depends on} {express})
(new-statement {Node.js} {depends on} {V8})
(new-statement {Node.js} {depends on} {libuv})
(new-statement {Node.js} {depends on} {http-parser})
(new-statement {Node.js} {depends on} {npm})
(new-statement {Node.js} {depends on} {OpenSSL})
(new-statement {npm} {depends on} {OpenSSL})
(new-statement {OpenCV} {depends on} {leveldb})
(new-statement {OpenCV} {depends on} {gflags})
(new-statement {OpenCV} {depends on} {glog})
;;; (new-statement {path} {depends on} {process})
(new-statement {path} {depends on} {util})
;;; (new-statement {process} {depends on} {debug})
(new-statement {python} {depends on} {eigen})
(new-statement {python} {depends on} {OpenSSL})
(new-statement {PHP} {depends on} {ANSI C})
(new-statement {PHP} {depends on} {Apache})
(new-statement {PHP} {depends on} {Composer})
(new-statement {PHP} {depends on} {GNU make})
(new-statement {PHP} {depends on} {MySQL})
(new-statement {protobuf} {depends on} {C++})
(new-statement {tornado} {depends on} {python 2.7})
(new-statement {tornado} {depends on} {PHP})
(new-statement {util} {depends on} {debug})
(new-statement {V8} {depends on} {C++})

(new-statement {TensorFlow} {depends on} {Cuda})
(new-statement {TensorFlow} {depends on} {OpenMPI})
(new-statement {TensorFlow} {depends on} {Protobuf})
(new-statement {TensorFlow} {depends on} {LLVM})
(new-statement {TensorFlow} {depends on} {Numpy})
(new-statement {TensorFlow} {depends on} {Hadoop})
(new-statement {Cuda} {depends on} {LLVM})
(new-statement {OpenMPI} {depends on} {C Std library})
(new-statement {LLVM} {depends on} {C++})
(new-statement {Numpy} {depends on} {python})
(new-statement {Hadoop} {depends on} {JDK})
(new-statement {Hadoop} {depends on} {Maven})

;;; *********Functions******************************

;;; Vulnerability check without verison. Users that are impacted by the
;;; certain software will be printed.
;;; Example: (user_check_vulnerability_equal {OpenSSL})
(defun user_check_vulnerability_equal (software);;; version exactly equal
  (setq userList '())
  (with-markers (m1 m2)
    (progn
      (mark-rel-inverse {depends on} software m1) 
      (do-marked (x m1)
        (mark-rel-inverse {requires} x m2)
        (do-marked (y m2) 
          (setq userList (nconc userList (list-rel-inverse {is performing} y)))
        )
      )
    )
  )
  (loop for x in userList do (print x))
)



;;; Vulnerability check without verison. Tasks that are impacted by the
;;; certain software will be printed.
;;; Example: (task_check_vulnerability_equal {OpenSSL})
(defun task_check_vulnerability_equal (software)
  (setq taskList '())
  (with-markers (m1 m2)
    (progn
      (mark-rel-inverse {depends on} software m1) 
      (do-marked (x m1)
        (setq taskList (nconc taskList (list-rel-inverse {requires} x)))
      )
    )
  )
  (loop for x in taskList do (print x))
)  


;;; Vulnerability check without verison. Softwares that are impacted by the
;;; certain software will be printed.
;;; Example: (software_check_vulnerability_equal {OpenSSL})

(defun software_check_vulnerability_equal (software)
  (setq softwareList '())
  (setq softwareList (nconc softwareList (list-rel-inverse {depends on} software)))
  (loop for x in softwareList do (print x))  
)
