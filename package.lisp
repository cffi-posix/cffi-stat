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

(in-package :common-lisp)

(defpackage :cffi-stat
  (:nicknames :stat)
  (:use
   :cffi
   :common-lisp
   :errno)
  (:export
   #:+accessperms+
   #:+allperms+
   #:+deffilemode+
   #:+s-blksize+
   #:+s-ifblk+
   #:+s-ifchr+
   #:+s-ifdir+
   #:+s-ififo+
   #:+s-iflnk+
   #:+s-ifmt+
   #:+s-ifreg+
   #:+s-ifsock+
   #:+s-irgrp+
   #:+s-iroth+
   #:+s-irusr+
   #:+s-irwxg+
   #:+s-irwxo+
   #:+s-irwxu+
   #:+s-isgid+
   #:+s-istxt+
   #:+s-isuid+
   #:+s-isvtx+
   #:+s-iwgrp+
   #:+s-iwoth+
   #:+s-iwusr+
   #:+s-ixgrp+
   #:+s-ixoth+
   #:+s-ixusr+
   #:+sf-append+
   #:+sf-archived+
   #:+sf-immutable+
   #:+sf-settable+
   #:+uf-append+
   #:+uf-immutable+
   #:+uf-nodump+
   #:+uf-opaque+
   #:+uf-settable+
   #:c-chmod
   #:c-fstat
   #:c-lstat
   #:c-mkdir
   #:c-mknod
   #:c-stat
   #:chmod
   #:fstat
   #:lstat
   #:mknod
   #:s-isblk
   #:s-ischr
   #:s-isdir
   #:s-isfifo
   #:s-islnk
   #:s-isreg
   #:s-issock
   #:stat
   #:stat-atim
   #:stat-blksize
   #:stat-blocks
   #:stat-ctim
   #:stat-dev
   #:stat-flags
   #:stat-gen
   #:stat-gid
   #:stat-ino
   #:stat-mode
   #:stat-mtim
   #:stat-nlink
   #:stat-rdev
   #:stat-size
   #:stat-uid
   #:with-fstat
   #:with-lstat
   #:with-stat
   ))
