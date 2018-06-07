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

(include "sys/stat.h")

(ctype blkcnt-t "blkcnt_t")
(ctype blksize-t "blksize_t")
(ctype dev-t "dev_t")
(ctype gid-t "gid_t")
(ctype ino-t "ino_t")
(ctype mode-t "mode_t")
(ctype nlink-t "nlink_t")
(ctype off-t "off_t")
(ctype time-t "time_t")
(ctype u-int32-t "u_int32_t")
(ctype uid-t "uid_t")

;; from time.h
(cstruct timespec "struct timespec"
         (tv-sec "tv_sec" :type time-t)
         (tv-nsec "tv_nsec" :type :long))

(cstruct stat "struct stat"
         (st-mode "st_mode" :type mode-t)
         (st-dev "st_dev" :type dev-t)
         (st-ino "st_ino" :type ino-t)
         (st-nlink "st_nlink" :type nlink-t)
         (st-uid "st_uid" :type uid-t)
         (st-gid "st_gid" :type gid-t)
         (st-rdev "st_rdev" :type dev-t)
         (st-atim "st_atim" :type (:struct timespec))
         (st-mtim "st_mtim" :type (:struct timespec))
         (st-ctim "st_ctim" :type (:struct timespec))
         (st-size "st_size" :type off-t)
         (st-blocks "st_blocks" :type blkcnt-t)
         (st-blksize "st_blksize" :type blksize-t)
         (st-flags "st_flags" :type u-int32-t)
         (st-gen "st_gen" :type u-int32-t))

(constant (+s-isuid+ "S_ISUID"))
(constant (+s-isgid+ "S_ISGID"))
(constant (+s-irwxu+ "S_IRWXU"))
(constant (+s-irusr+ "S_IRUSR"))
(constant (+s-iwusr+ "S_IWUSR"))
(constant (+s-ixusr+ "S_IXUSR"))
(constant (+s-irwxg+ "S_IRWXG"))
(constant (+s-irgrp+ "S_IRGRP"))
(constant (+s-iwgrp+ "S_IWGRP"))
(constant (+s-ixgrp+ "S_IXGRP"))
(constant (+s-irwxo+ "S_IRWXO"))
(constant (+s-iroth+ "S_IROTH"))
(constant (+s-iwoth+ "S_IWOTH"))
(constant (+s-ixoth+ "S_IXOTH"))

(constant (+accessperms+ "ACCESSPERMS"))
(constant (+allperms+    "ALLPERMS"))
(constant (+deffilemode+ "DEFFILEMODE"))
(constant (+s-blksize+   "S_BLKSIZE"))

(constant (+uf-settable+  "UF_SETTABLE"))
(constant (+uf-nodump+    "UF_NODUMP"))
(constant (+uf-immutable+ "UF_IMMUTABLE"))
(constant (+uf-append+    "UF_APPEND"))
(constant (+uf-opaque+    "UF_OPAQUE"))

(constant (+sf-settable+  "SF_SETTABLE"))
(constant (+sf-archived+  "SF_ARCHIVED"))
(constant (+sf-immutable+ "SF_IMMUTABLE"))
(constant (+sf-append+    "SF_APPEND"))
