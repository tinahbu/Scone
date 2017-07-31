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
(new-type {resources} {thing})

;;; Detailed resources: software, hardware, operating system
(new-type {software resources} {resources})
(new-type {hardware resources} {resources})
(new-type {operating system} {resources})

;;; Different categories of hardware
(new-type {graphics} {hardware resources})
(new-type {memory} {hardware resources})
(new-type {storage} {hardware resources})
(new-type {processor} {hardware resources})
(new-type {CPU} {processor})
(new-type {Intel Core CPU} {CPU})
(new-type {Intel Core CPU_i3} {Intel Core CPU})
(new-type {Intel Core CPU_i5} {Intel Core CPU})
(new-type {Intel Core CPU_i7} {Intel Core CPU})
(new-type {GPU} {processor})
(new-type {Nvidia GeForce GTX} {GPU})
(new-type {AMD Radeon RX} {GPU})
(new-type {Nvidia GeForce GTX_1080} {Nvidia GeForce GTX})
(new-type {Nvidia GeForce GTX_1060} {Nvidia GeForce GTX})
(new-type {Nvidia GeForce GTX_1050} {Nvidia GeForce GTX})
(new-type {Nvidia GeForce GTX_980} {Nvidia GeForce GTX})
(new-type {AMD Radeon RX_580} {AMD Radeon RX})
(new-type {AMD Radeon RX_470} {AMD Radeon RX})
(new-type {AMD Radeon RX_390} {AMD Radeon RX})
(new-type {AMD Radeon RX_290} {AMD Radeon RX})

;;; Different types of operating system
(new-split-subtypes {operating system}  '({MacOS} {Linux} {Windows}))
(new-type {MacOS_10.2} {MacOS})
(new-type {MacOS_10.6} {MacOS})
(new-type {Windows_8} {Windows})
(new-type {Windows_10} {Windows})

;;;
;;; USER
;;;
;;; Add Role Node to {user}
(new-indv-role {userid of user} {user} {string})
(new-indv-role {username of user} {user} {string})
(new-indv-role {email of user} {user} {string})
(new-type-role {member of user} {user} {user})
(new-type-role {os of user} {user} {operating system})
(new-type-role {processor of user} {user} {processor})
(new-type-role {storage of user} {user} {storage})

;;; Add Default User Group to {user} with the least privilege
(new-type {default user} {user})


;;; role-node of hardware
(new-indv-role {version of hardware resources} {hardware resources} {string})
(new-indv-role {brand of hardware resources} {hardware resources} {string})


;;; role node of operating system
(new-indv-role {version of operating system} {operating system} {string})
;;; (new-indv-role {brand of operating system} {operating system} {string})

;;; Add Role Node to {software resources}
(new-indv-role {version of software resources} {software resources} {string})

;;;
;;; SOFTWARE RESOURCES
;;;
;;; Create New Software resources Type Node
;;; default is software resources
;;; Sorted by first alphabet
(new-type {Apache} {software resources})
(new-type {ANSI C} {software resources})
(new-type {BLAS} {software resources})
(new-type {Boost} {software resources})
(new-type {Boost_1.55} {Boost})
(new-type {Caffe} {software resources})
(new-type {Composer} {software resources})
(new-type {cookie} {software resources})
(new-type {C} {software resources})
(new-type {coreutils} {software resources})
(new-type {CUDA} {software resources})
(new-type {CUDA_5.0} {CUDA})
(new-type {CUDA_5.5} {CUDA})
(new-type {CUDA_6.0} {CUDA})
(new-type {CUDA_7.0} {CUDA})
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
(new-type {OpenCV_2.4} {OpenCV})
(new-type {OpenCV_3.0} {OpenCV})
(new-type {OpenSSL} {software resources})
(new-type {tornado} {software resources})
(new-type {path} {software resources})
;;; (new-type {process} {software resources}) ;;;Error, process has already been used in Scone
(new-type {protobuf} {software resources})
(new-type {python} {software resources})
(new-type {python_2.7} {python})
(new-type {python_3.0} {python})
(new-type {python_3.2} {python})
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
(new-type {httplib} {software resources})
(new-type {urllib} {software resources})
(new-type {urllib2} {software resources})
(new-type {xmlrpclib} {software resources})

;;; Assign Version Value to Software's {version of software resources} role node
(x-is-the-y-of-z (new-string {"1.55"}) {version of software resources} {Boost_1.55})
(x-is-the-y-of-z (new-string {"5.0"}) {version of software resources} {CUDA_5.0})
(x-is-the-y-of-z (new-string {"5.5"}) {version of software resources} {CUDA_5.5})
(x-is-the-y-of-z (new-string {"6.0"}) {version of software resources} {CUDA_6.0})
(x-is-the-y-of-z (new-string {"7.0"}) {version of software resources} {CUDA_7.0})
(x-is-the-y-of-z (new-string {"2.4"}) {version of software resources} {OpenCV_2.4})
(x-is-the-y-of-z (new-string {"3.0"}) {version of software resources} {OpenCV_3.0})
(x-is-the-y-of-z (new-string {"2.7"}) {version of software resources} {python_2.7})
(x-is-the-y-of-z (new-string {"3.0"}) {version of software resources} {python_3.0})
(x-is-the-y-of-z (new-string {"3.2"}) {version of software resources} {python_3.2})
(x-is-the-y-of-z (new-string {"3"}) {version of hardware resources} {Intel Core CPU_i3})
(x-is-the-y-of-z (new-string {"5"}) {version of hardware resources} {Intel Core CPU_i5})
(x-is-the-y-of-z (new-string {"7"}) {version of hardware resources} {Intel Core CPU_i7})
(x-is-the-y-of-z (new-string {"Intel Core"}) {brand of hardware resources} {Intel Core CPU_i3})
(x-is-the-y-of-z (new-string {"Intel Core"}) {brand of hardware resources} {Intel Core CPU_i5})
(x-is-the-y-of-z (new-string {"Intel Core"}) {brand of hardware resources} {Intel Core CPU_i7})
(x-is-the-y-of-z (new-string {"1080"}) {version of hardware resources} {Nvidia GeForce GTX_1080})
(x-is-the-y-of-z (new-string {"1060"}) {version of hardware resources} {Nvidia GeForce GTX_1060})
(x-is-the-y-of-z (new-string {"1050"}) {version of hardware resources} {Nvidia GeForce GTX_1050})
(x-is-the-y-of-z (new-string {"980"}) {version of hardware resources} {Nvidia GeForce GTX_980})
(x-is-the-y-of-z (new-string {"Nvidia GeForce GTX"}) {brand of hardware resources} {Nvidia GeForce GTX_1080})
(x-is-the-y-of-z (new-string {"Nvidia GeForce GTX"}) {brand of hardware resources} {Nvidia GeForce GTX_1060})
(x-is-the-y-of-z (new-string {"Nvidia GeForce GTX"}) {brand of hardware resources} {Nvidia GeForce GTX_1050})
(x-is-the-y-of-z (new-string {"Nvidia GeForce GTX"}) {brand of hardware resources} {Nvidia GeForce GTX_980})
(x-is-the-y-of-z (new-string {"580"}) {version of hardware resources} {AMD Radeon RX_580})
(x-is-the-y-of-z (new-string {"470"}) {version of hardware resources} {AMD Radeon RX_470})
(x-is-the-y-of-z (new-string {"390"}) {version of hardware resources} {AMD Radeon RX_390})
(x-is-the-y-of-z (new-string {"290"}) {version of hardware resources} {AMD Radeon RX_290})
(x-is-the-y-of-z (new-string {"AMD Radeon RX"}) {brand of hardware resources} {AMD Radeon RX_580})
(x-is-the-y-of-z (new-string {"AMD Radeon RX"}) {brand of hardware resources} {AMD Radeon RX_470})
(x-is-the-y-of-z (new-string {"AMD Radeon RX"}) {brand of hardware resources} {AMD Radeon RX_390})
(x-is-the-y-of-z (new-string {"AMD Radeon RX"}) {brand of hardware resources} {AMD Radeon RX_290})
(x-is-the-y-of-z (new-string {"10.2"}) {version of operating system} {MacOS_10.2})
(x-is-the-y-of-z (new-string {"10.6"}) {version of operating system} {MacOS_10.6})
(x-is-the-y-of-z (new-string {"8"}) {version of operating system} {Windows_8})
(x-is-the-y-of-z (new-string {"10"}) {version of operating system} {Windows_10})

;;;
;;; RELATIONS
;;;
;;; Create 6 relations between main components
(new-relation {is performing} :a-type-of {user} :b-type-of {task} :transitive T)
(new-relation {requires software} :a-type-of {task} :b-type-of {software resources} :transitive T)
(new-relation {requires processor} :a-type-of {task} :b-type-of {processor})
(new-relation {requires operating system} :a-type-of {task} :b-type-of {operating system})
(new-relation {is authorized to execute} :a-type-of {user} :b-type-of {software resources} :transitive T)
(new-relation {depends on} :a-type-of {software resources} :b-type-of {software resources} :transitive T)

;;; Create {depends on} Relations
(new-statement {BLAS} {depends on} {Fortran})
(new-statement {Boost} {depends on} {C++})
(new-statement {Caffe} {depends on} {OpenCV_2.4})
(new-statement {Caffe} {depends on} {hdf5})
(new-statement {Caffe} {depends on} {gflags})
(new-statement {Caffe} {depends on} {CUDA_6.0})
(new-statement {Caffe} {depends on} {CUDA_7.0})
(new-statement {Caffe} {depends on} {BLAS})
(new-statement {Caffe} {depends on} {Boost_1.55})
(new-statement {Caffe} {depends on} {protobuf})
(new-statement {Caffe} {depends on} {leveldb})
(new-statement {coreutils} {depends on} {gmp})
(new-statement {cookie} {depends on} {debug})
(new-statement {express} {depends on} {cookie})
(new-statement {Expresso} {depends on} {python_3.0})
(new-statement {Expresso} {depends on} {python_3.2})
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
(new-statement {Node.js} {depends on} {V8})
(new-statement {Node.js} {depends on} {libuv})
(new-statement {Node.js} {depends on} {npm})
(new-statement {Node.js} {depends on} {OpenSSL})
(new-statement {npm} {depends on} {OpenSSL})
(new-statement {OpenCV} {depends on} {leveldb})
(new-statement {OpenCV} {depends on} {gflags})
(new-statement {OpenCV} {depends on} {glog})
(new-statement {path} {depends on} {util})
(new-statement {python} {depends on} {eigen})
(new-statement {python} {depends on} {OpenSSL})
(new-statement {PHP} {depends on} {ANSI C})
(new-statement {PHP} {depends on} {Apache})
(new-statement {PHP} {depends on} {Composer})
(new-statement {PHP} {depends on} {GNU make})
(new-statement {PHP} {depends on} {MySQL})
(new-statement {protobuf} {depends on} {C++})
(new-statement {tornado} {depends on} {python_2.7})
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
;;; Example: (user_check_vulnerability {OpenSSL})
;;; Example: (user_check_vulnerability {Python})
(defun user_check_vulnerability (software);;; version exactly equal
  (setq userList '())
  (with-markers (m1 m2 m3)
    (progn
      (mark-rel-inverse {depends on} software m1)
      (mark-instances software m1)
      (do-marked (x m1)
        (setq y (type-node? x))
        (if (string= y "NIL") () (unmark x m1)))
      (do-marked (x m1)
        (mark-rel-inverse {requires software} x m2)
        (do-marked (y m2)
          (setq userList (nconc userList (list-rel-inverse {is performing} y)))))))
  (loop for x in userList do (print x)))

;;; Vulnerability check without verison. Tasks that are impacted by the
;;; certain software will be printed.
;;; Example: (task_check_vulnerability {OpenSSL})
;;;(task_check_vulnerability {OpenMPI})
(defun task_check_vulnerability (software)
  (setq taskList '())
  (with-markers (m1 m2)
    (progn
      (mark-rel-inverse {depends on} software m1)
      (mark-instances software m1)
      (do-marked (x m1)
        (setq y (type-node? x))
        (if (string= y "NIL") () (unmark x m1)))
      (do-marked (x m1)
        (setq taskList (nconc taskList (list-rel-inverse {requires software} x))))))
  (loop for x in taskList do (print x)))

;;; Vulnerability check without verison. Softwares that are directly required by tasks
;;; that are impacted by this vulnerability will be printed.
;;; Example: (software_check_vulnerability {OpenSSL})

(defun software_check_vulnerability (software)
  (setq softwareList '())
  (with-markers (m1)
    (progn
      (mark-rel-inverse {depends on} software m1)
      (mark-instances software m1)
      (do-marked (x m1)
        (setq y (type-node? x))
        (if (string= y "NIL") (setq softwareList (nconc softwareList (list x))) ()))))
  (loop for x in softwareList do (print x)))

;;; Vulnerability check WITH verison.
;;; Given a certain version, users that are impacted by the OLDER version (not included)
;;; are printed.
;;; Example: (user_check_vulnerability_older {python} "3.0")
(defun user_check_vulnerability_older (software version)
  (setq userList '())
  (with-markers (m1 m2 m3)
    (progn
      (mark-instances software m1)
      (do-marked (x m1)
        ;;; Get all the version number
        (setq softwareVersion
          (node-value (the-x-of-y {version of software resources} x)))
        (setq tmp (string< softwareVersion version))
        (if (not tmp)
          ;;; softwareVersion is greater or equal/newer than given version
          ()
          ;;; softwareVersion is smaller/older than given version
          (progn
            (mark-rel-inverse {depends on} x m2)
            (mark x m2)
            (do-marked (y m2)
              ;;; Query for all the {task} that {requires software} the potentially impacted {software resource}
              (setq tmp2 (type-node? y))
              (if (string= tmp2 "NIL")(mark-rel-inverse {requires software} y m3)(clear-marker m3))
              (do-marked (z m3)
                ;;; Query for all the {user} that {is performing} the potentially impacted {tasks}
                (setq userList (nconc userList (list-rel-inverse {is performing} z))))))))))
  (loop for x in userList do (print x)))

;;; Vulnerability check WITH verison.
;;; Given a certain version, tasks that are impacted by the OLDER version (not included)
;;; are printed.
;;; Example: (task_check_vulnerability_older {python} "3.0")

(defun task_check_vulnerability_older (software version)
  (setq taskList '())
  (with-markers (m1 m2)
    (progn
      (mark-instances software m1)
      (do-marked (x m1)
        ;;; Get all the version number
        (setq softwareVersion
          (node-value (the-x-of-y {version of software resources} x)))
        (setq tmp (string< softwareVersion version))
        (if (not tmp)
          ;;; softwareVersion is greater or equal/newer than given version
          ()
          ;;; softwareVersion is smaller/older than given version
          (progn
            (mark-rel-inverse {depends on} x m2)
            (mark x m2)
            (do-marked (y m2)
              ;;; Query for all the {task} that {requires software} the potentially impacted {software resource}
              (setq tmp2 (type-node? y))
              (if (string= tmp2 "T") () (setq taskList (nconc taskList (list-rel-inverse {requires software} y))))))))))
  (loop for x in taskList do (print x)))

;;; Vulnerability check WITH verison.
;;; Given a certain version, softwares that are impacted by the OLDER version (not included)
;;; are printed.
;;; Example: (software_check_vulnerability_older {python} "3.0")

(defun software_check_vulnerability_older (software version)
  (setq softwareList '())
  (with-markers (m1 m2)
    (progn
      (mark-instances software m1)
      (do-marked (x m1)
        ;;; Get all the version number
        (setq softwareVersion
          (node-value (the-x-of-y {version of software resources} x)))
        (setq tmp (string< softwareVersion version))
        (if (not tmp)
          ;;; softwareVersion is greater or equal/newer than given version
          ()
          ;;; softwareVersion is smaller/older than given version
          (progn
            (mark-rel-inverse {depends on} x m2)
            (do-marked (y m2)
              (setq tmp2 (type-node? y))
              (if(string= "T" tmp2)()(setq softwareList (nconc softwareList (list y)))))
            (setq softwareList (nconc softwareList (list x))))))))
  (loop for x in softwareList do (print x)))

;;; Vulnerability check WITH verison.
;;; Given a certain version, users that are impacted by the NEWER version (not included)
;;; are printed.
;;; Example: (user_check_vulnerability_newer {python} "2.7")

(defun user_check_vulnerability_newer (software version)
  (setq userList '())
  (with-markers (m1 m2 m3)
    (progn
      (mark-instances software m1)
      (do-marked (x m1)
        ;;; Get all the version number
        (setq softwareVersion
          (node-value (the-x-of-y {version of software resources} x)))
        (setq tmp (string> softwareVersion version))
        (if (not tmp)
          ;;; softwareVersion is older or equal to the given version
          ()
          ;;; softwareVersion is newer than the given version
          (progn
            (mark-rel-inverse {depends on} x m2)
            (mark x m2)
            (do-marked (y m2)
              ;;; Query for all the {task} that {requires software} the potentially impacted {software resource}
              (setq tmp2 (type-node? y))
              (if (string= tmp2 "NIL")(mark-rel-inverse {requires software} y m3)(clear-marker m3))
              (do-marked (z m3)
                ;;; Query for all the {user} that {is performing} the potentially impacted {tasks}
                (setq userList (nconc userList (list-rel-inverse {is performing} z))))))))))
  (loop for x in userList do (print x)))


;;; Vulnerability check WITH verison.
;;; Given a certain version, tasks that are impacted by the NEWER version (not included)
;;; are printed.
;;; Example: (task_check_vulnerability_newer {python} "2.7")
(defun task_check_vulnerability_newer (software version)
  (setq taskList '())
  (with-markers (m1 m2)
    (progn
      (mark-instances software m1)
      (do-marked (x m1)
        ;;; Get all the version number
        (setq softwareVersion
          (node-value (the-x-of-y {version of software resources} x)))
        (setq tmp (string> softwareVersion version))
        (if (not tmp)
          ;;; softwareVersion is older or equal to the given version
          ()
          ;;; softwareVersion is newer than the given version
          (progn
            (mark-rel-inverse {depends on} x m2)
            (mark x m2)
            (do-marked (y m2)
              ;;; Query for all the {task} that {requires software} the potentially impacted {software resource}
              (setq tmp2 (type-node? y))
              (if (string= tmp2 "T") () (setq taskList (nconc taskList (list-rel-inverse {requires software} y))))))))))
  (loop for x in taskList do (print x)))

;;; Vulnerability check WITH verison.
;;; Given a certain version, softwares that are impacted by the NEWER version (not included)
;;; are printed.
;;; Example: (software_check_vulnerability_newer {python} "3.0")

(defun software_check_vulnerability_newer (software version)
  (setq softwareList '())
  (with-markers (m1 m2)
    (progn
      (mark-instances software m1)
      (do-marked (x m1)
        ;;; Get all the version number
        (setq softwareVersion
          (node-value (the-x-of-y {version of software resources} x)))
        (setq tmp (string> softwareVersion version))
        (if (not tmp)
          ;;; softwareVersion is older or equal to the given version
          ()
          ;;; softwareVersion is newer than the given version
          (progn
            (mark-rel-inverse {depends on} x m2)
            (do-marked (y m2)
              (setq tmp2 (type-node? y))
              (if(string= "T" tmp2)()(setq softwareList (nconc softwareList (list y)))))
            (setq softwareList (nconc softwareList (list x))))))))
  (loop for x in softwareList do (print x)))


;;; Vulnerability check WITH verison.
;;; Given a certain version, users that are impacted by the given version
;;; are printed.
;;; Example: (user_check_vulnerability_equal {python} "3.0")

(defun user_check_vulnerability_equal (software version)
  (setq userList '())
  (with-markers (m1 m2 m3)
    (progn
      (mark-instances software m1)
      (do-marked (x m1)
        ;;; Get all the version number
        (setq softwareVersion
          (node-value (the-x-of-y {version of software resources} x)))
        (setq tmp (string= softwareVersion version))
        (if (not tmp)
          ;;; softwareVersion is different from the given version
          ()
          ;;; softwareVersion is the same as the given version
          (progn
            (mark-rel-inverse {depends on} x m2)
            (mark x m2)
            (do-marked (y m2)
              ;;; Query for all the {task} that {requires software} the potentially impacted {software resource}
              (setq tmp2 (type-node? y))
              (if (string= tmp2 "NIL")(mark-rel-inverse {requires software} y m3)(clear-marker m3))
              (do-marked (z m3)
                ;;; Query for all the {user} that {is performing} the potentially impacted {tasks}
                (setq userList (nconc userList (list-rel-inverse {is performing} z))))))))))
  (loop for x in userList do (print x)))


;;; Vulnerability check WITH verison.
;;; Given a certain version, tasks that are impacted by the given version
;;; are printed.
;;; Example: (task_check_vulnerability_equal {python} "3.0")

(defun task_check_vulnerability_equal (software version)
  (setq taskList '())
  (with-markers (m1 m2)
    (progn
      (mark-instances software m1)
      (do-marked (x m1)
        ;;; Get all the version number
        (setq softwareVersion
          (node-value (the-x-of-y {version of software resources} x)))
        (setq tmp (string= softwareVersion version))
        (if (not tmp)
          ;;; softwareVersion is different from the given version
          ()
          ;;; softwareVersion is the same as the given version
          (progn
            (mark-rel-inverse {depends on} x m2)
            (mark x m2)
            (do-marked (y m2)
              ;;; Query for all the {task} that {requires software} the potentially impacted {software resource}
              (setq tmp2 (type-node? y))
              (if (string= tmp2 "T") () (setq taskList (nconc taskList (list-rel-inverse {requires software} y))))))))))
  (loop for x in taskList do (print x)))


;;; Vulnerability check WITH verison.
;;; Given a certain version, softwares that are impacted by the given version
;;; are printed.
;;; Example: (software_check_vulnerability_equal {python} "3.0")

(defun software_check_vulnerability_equal (software version)
  (setq softwareList '())
  (with-markers (m1 m2)
    (progn
      (mark-instances software m1)
      (do-marked (x m1)
        ;;; Get all the version number
        (setq softwareVersion
          (node-value (the-x-of-y {version of software resources} x)))
        (setq tmp (string= softwareVersion version))
        (if (not tmp)
           ;;; softwareVersion is different from the given version
          ()
          ;;; softwareVersion is the same as the given version
          (progn
            (mark-rel-inverse {depends on} x m2)
            (do-marked (y m2)
              (setq tmp2 (type-node? y))
              (if(string= "T" tmp2)()(setq softwareList (nconc softwareList (list y)))))
            (setq softwareList (nconc softwareList (list x))))))))
  (loop for x in softwareList do (print x)))

;;; Given a user and a task, print a list of softwares that
;;; the user is not yet authorized to execute.
;;; Example: (access_check {user 1} {CNN for product recommendation})
;;; Example: (access_check {user 3} {CNN for product recommendation})


; (defun access_check (user task)
;   (setq softwareList '())
;   (setq tmp '())
;   (with-markers (m1 m2 m3 m4 m5 m6)
;     (progn
;       (mark-rel {requires software} task m1) ;;; {Expresso}
;       ;;; query the authorization given to the user
;       (mark-rel {is authorized to execute} user m2) ;;; {python}, {python 3.0}, {python 2.7}
;       ;;; query the authorization given to the group the user belongs to
;       (mark-role-inverse {member of user} user m3)
;       (do-marked (x m3)
;         (setq tmp (nconc tmp (list-rel {is authorized to execute} m4)))
;       )
;       (free-marker m4)
;       (loop for x in tmp do (mark x m5))
;       (mark-boolean m6 (list m1) (list m2 m5))

;       (do-marked (x m6)
;         (setq softwareList (nconc softwareList (list x)))
;       )
;     )
;   )
;   (loop for x in softwareList do (print x))
; )

(defun access_check (user task)
  (setq softwareList '())
  (setq tmp '())
  (with-markers (m1 m2)
    (progn
      (mark-rel {requires software} task m1) ;;; {Expresso}
      (do-marked (x m1)
        (setq cond1 (statement-true? user {is authorized to execute} x))
        (mark-role-inverse {member of user} user m2)
        (do-marked (y m2)
          (setq cond2 (statement-true? y {is authorized to execute} x)))
        (if (or (string= cond1 "T") (string= cond2 "T")) () (setq softwareList (nconc softwareList (list x)))))
  (loop for x in softwareList do (print x)))))


;;; Given a user and a software, print a list of softwares that 
;;; the user is not yet authorized to execute.
;;; Example: (software_access_check {user 1} {Expresso})
;;; Example: (access_check {user 3} {CNN for product recommendation})

;;; (authorized_to_use? {user 1} {Expresso})
;;; (authorized_to_use? {user 3} {Python})
(defun authorized_to_use? (user software)
    (setq cond1 (statement-true? user {is authorized to execute} software))
    (setq cond2 nil)
    (with-markers (m1)
      (progn
        (mark-role-inverse {member of user} user m1)
        (do-marked (x m1)
            (setq tmp (statement-true? x {is authorized to execute} software))
            (setq cond2 (if tmp t nil)))))
    (if (or cond1 cond2) (print "t") (print "nil")))

;;; Check whether the user has the required CPU according to specific task requirement
;;; Example (task_check_user_CPU {user 6} {VR Game Development})
(defun task_check_user_CPU (user task)
  (setq CPUcond nil)
  ;;; check what hardware the user installed
  (setq userProcessorList (list-all-x-of-y {processor of user} user))
  (setq userCPUbrand "")
  (setq userCPUversion "")
  (loop for x in userProcessorList do 
    (setq cpuBoolean (is-x-a-y? x {CPU}))
    (if (string= cpuBoolean "YES") 
    (progn
      (setq userCPUbrand (node-value (the-x-of-y {brand of hardware resources} x)))
      (setq userCPUversion (node-value (the-x-of-y {version of hardware resources} x))))
    ()))

  ;;; check what hardware the task requires
  (setq CPUbrand (task_CPU_brand task))
  (setq CPUversion (task_CPU_version task))  
  ;;;Compare CPU brand with the required CPU brand
  
  (if (string= userCPUbrand CPUbrand) (if(>= (parse-integer userCPUversion) (parse-integer CPUversion))(setq CPUcond t)()) ())
  CPUcond)

;;; Check whether the user has the required GPU according to specific task requirement
;;; Example (task_check_user_GPU {user 6} {VR Game Development})
(defun task_check_user_GPU (user task)
  (setq GPUcond nil)
  ;;; check what hardware the user installed
  (setq userProcessorList (list-all-x-of-y {processor of user} user))
  (setq userGPUbrand "")
  (setq userGPUversion "")
  (loop for x in userProcessorList do 
    (setq gpuBoolean (is-x-a-y? x {GPU}))
    (if (string= gpuBoolean "YES") 
      (progn
        (setq userGPUBrand (node-value (the-x-of-y {brand of hardware resources} x)))
        (setq userGPUVersion (node-value (the-x-of-y {version of hardware resources} x))))
    ()))
  ;;; check what hardware the task requires
  (setq GPUbrand (task_GPU_brand task))
  (setq GPUversion (task_GPU_version task))
  
  ;;;Compare GPU brand with the required GPU brand
  (if (string= userGPUbrand GPUbrand) (if(>= (parse-integer userGPUversion) (parse-integer GPUversion))(setq GPUcond t)()) ())
  GPUcond)


(defun task_GPU_brand (task)
    (with-markers (m1 m2 m3)
      (progn
        (mark-rel {requires processor} task m1)
        (mark-instances {GPU} m2)
        (mark-boolean m3 (list m1 m2) ())
        (do-marked (x m3)
          (setq brand (node-value (the-x-of-y {brand of hardware resources} x))))))
  brand)    

(defun task_CPU_brand (task)
    (with-markers (m1 m2 m3)
      (progn
        (mark-rel {requires processor} task m1)
        (mark-instances {CPU} m2)
        (mark-boolean m3 (list m1 m2) ())
        (do-marked (x m3)
          (setq brand (node-value (the-x-of-y {brand of hardware resources} x))))))
  brand)
   
(defun task_GPU_version (task)
    (with-markers (m1 m2 m3)
      (progn
        (mark-rel {requires processor} task m1)
        (mark-instances {GPU} m2)
        (mark-boolean m3 (list m1 m2) ())
        (do-marked (x m3)
          (setq brand (node-value (the-x-of-y {version of hardware resources} x))))))
  brand)    

(defun task_CPU_version (task)
    (with-markers (m1 m2 m3)
      (progn
        (mark-rel {requires processor} task m1)
        (mark-instances {CPU} m2)
        (mark-boolean m3 (list m1 m2) ())
        (do-marked (x m3)
          (setq brand (node-value (the-x-of-y {version of hardware resources} x))))))
  brand)

;;; Given a user and a task, returns t if the user's operating system meets the task's os requirements
;;; returns nil if it doesn't

;;; test case: (task_check_os {user 3} {VR Game Development})
(defun task_check_user_OS (user task)
    (setq result nil)
    ;;; Check what kind of os the user has
    (setq user_os (the-x-of-y {os of user} user))
    ;;; Check what version it is
    (setq user_os_version (node-value (the-x-of-y {version of operating system} user_os)))
    (setq task_os_list (list-rel {requires operating system} task))
    (loop for x in task_os_list do 
      (progn
        (setq task_os_version (node-value (the-x-of-y {version of operating system} x)))
        (if (is-x-a-y? user_os x) (if (string< task_os_version user_os_version) (setq result t) ()) ())
      )
    )
    result
  )
