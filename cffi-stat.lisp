;;
;;  cffi-stat  -  Common Lisp wrapper for stat.h
;;
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :cffi-stat)

(defmacro stat-mode (stat)
  `(foreign-slot-value ,stat '(:struct stat) 'st-mode))

(defmacro stat-dev (stat)
  `(foreign-slot-value ,stat '(:struct stat) 'st-dev))

(defmacro stat-ino (stat)
  `(foreign-slot-value ,stat '(:struct stat) 'st-ino))

(defmacro stat-nlink (stat)
  `(foreign-slot-value ,stat '(:struct stat) 'st-nlink))

(defmacro stat-uid (stat)
  `(foreign-slot-value ,stat '(:struct stat) 'st-uid))

(defmacro stat-gid (stat)
  `(foreign-slot-value ,stat '(:struct stat) 'st-gid))

(defmacro stat-rdev (stat)
  `(foreign-slot-value ,stat '(:struct stat) 'st-rdev))

(defmacro stat-atim (stat)
  `(foreign-slot-value ,stat '(:struct stat) 'st-atim))

(defmacro stat-mtim (stat)
  `(foreign-slot-value ,stat '(:struct stat) 'st-mtim))

(defmacro stat-ctim (stat)
  `(foreign-slot-value ,stat '(:struct stat) 'st-ctim))

(defmacro stat-size (stat)
  `(foreign-slot-value ,stat '(:struct stat) 'st-size))

(defmacro stat-blocks (stat)
  `(foreign-slot-value ,stat '(:struct stat) 'st-blocks))

(defmacro stat-blksize (stat)
  `(foreign-slot-value ,stat '(:struct stat) 'st-blksize))

(defmacro stat-flags (stat)
  `(foreign-slot-value ,stat '(:struct stat) 'st-flags))

(defmacro stat-gen (stat)
  `(foreign-slot-value ,stat '(:struct stat) 'st-gen))

(defun s-isdir  (m) (= #o040000 (logand #o170000 (the fixnum m))))
(defun s-ischr  (m) (= #o020000 (logand #o170000 (the fixnum m))))
(defun s-isblk  (m) (= #o060000 (logand #o170000 (the fixnum m))))
(defun s-isreg  (m) (= #o100000 (logand #o170000 (the fixnum m))))
(defun s-isfifo (m) (= #o010000 (logand #o170000 (the fixnum m))))
(defun s-islnk  (m) (= #o120000 (logand #o170000 (the fixnum m))))
(defun s-issock (m) (= #o140000 (logand #o170000 (the fixnum m))))

(defcfun ("fstat" c-fstat) :int
  (fd :int)
  (sb (:pointer (:struct stat))))

(defmacro with-fstat ((var &optional (error-p t)) fd &body body)
  `(with-foreign-object (,var '(:struct stat))
     (cond ((= 0 (c-fstat ,fd (mem-aptr ,var '(:struct stat))))
            (locally ,@body))
           ,@(when error-p
               `((t (error-errno "fstat")))))))

(defcfun ("stat" c-stat) :int
  (path :string)
  (sb (:pointer (:struct stat))))

(defmacro with-stat ((var &optional (error-p t)) path &body body)
  `(with-foreign-object (,var '(:struct stat))
     (cond ((= 0 (the fixnum (c-stat ,path ,var)))
            (locally ,@body))
           ,@(when error-p
               `((t (error-errno "stat")))))))

(defcfun ("lstat" c-lstat) :int
  (path :string)
  (sb (:pointer (:struct stat))))

(defmacro with-lstat ((var &optional (error-p t)) path &body body)
  `(with-foreign-object (,var '(:struct stat))
     (cond ((= 0 (c-lstat ,path ,var))
            (locally ,@body))
           ,@(when error-p
               `((t (error-errno "lstat")))))))
