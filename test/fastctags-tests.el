;; fastctags-tests.el --- unit tests for fastctags -*- coding: utf-8 -*-

;; Author: Chen Bin

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'fastctags nil t)

(defun get-full-path (filename)
  "Get full path of FILENAME."
  (concat
   (if load-file-name (file-name-directory load-file-name) default-directory)
   filename))

(defun fastctags-test-load-tags-file-internal (file-name)
  "Run test by FILE-NAME of tags file."
  (let* (cands
         (tags-file-name (get-full-path file-name))
         file-info
         (file-size (nth 7 (file-attributes tags-file-name)))
         dict)
    (should (file-exists-p tags-file-name))
    (setq fastctags-tags-file-cache nil)
    (fastctags-load-tags-file tags-file-name nil t t)
    (fastctags-debug-info)
    (setq file-info (gethash tags-file-name fastctags-tags-file-cache))
    (should file-info)
    ;; check the file meta data
    (should (eq (plist-get file-info :filesize) file-size))
    (should (eq (length (plist-get file-info :raw-content)) file-size))
    (setq dict (plist-get file-info :tagname-dict))
    (should dict)))

(ert-deftest fastctags-test-load-tags-file ()
  ;; one hello function, one hello method and one test method in hello.js
  (fastctags-test-load-tags-file-internal "tags-in-vim-format"))

(defun fastctags-test-completion-internal (file-name)
  "Run test by FILE-NAME of tags file."
  (let* ((tags-file-name (get-full-path file-name))
         file-info
         dict
         cands)
    (setq fastctags-tags-file-cache nil)
    (should (fastctags-load-tags-file tags-file-name nil t t))
    (should fastctags-tags-file-cache)

    (setq file-info (gethash tags-file-name fastctags-tags-file-cache))
    (setq dict (plist-get file-info :tagname-dict))
    ;; completion with prefix
    (setq cands (fastctags-all-candidates "get" dict))
    (should (eq (length cands) 1))
    (should (string= (car cands) "get-full-path"))
    (setq cands (fastctags-all-candidates "fastctags" dict))
    (should (eq (length cands) 3))))

(ert-deftest fastctags-test-find-tag ()
  (let* ((tags-file (get-full-path "tags-in-vim-format")))
    ;; fuzzy match
    (should (eq (length (fastctags-nav-extract-cands tags-file "fastctags" t)) 3))

    ;; strict match
    (should (eq (length (fastctags-nav-extract-cands tags-file "get-full-path" nil)) 1))
    (should (eq (length (fastctags-nav-extract-cands tags-file "get-" nil)) 0))))

(ert-deftest fastctags-test-sort-cands-by-filename ()
  (let* (cands
         (tags-file (get-full-path "tags-in-vim-format")))
    (setq cands (fastctags-nav-extract-cands tags-file "fastctags" t))
    (should (eq (length cands) 3))
    ;; sort the candidate by string-distance from "hello.js"
    (let* ((f (get-full-path "fastctags-tests.el")))
      (should (string-match "fastctags-tests.el"
                            (car (nth 0 (fastctags-nav-sort-candidates-maybe cands 3 f))))))))

(ert-deftest fastctags-test-tags-file-cache ()
  (let* (cands
         (tags-file (get-full-path "tags-in-vim-format")))
    ;; clear cache
    (setq fastctags-tags-file-cache nil)
    (setq cands (fastctags-nav-extract-cands tags-file "fastctags" t))
    (should (eq (length cands) 3))
    ;; cache is filled
    (should fastctags-tags-file-cache)
    (should (plist-get (fastctags-get-tags-file-info tags-file) :raw-content))))

(ert-deftest fastctags-test-tag-history ()
  (let* (cands
         (tags-file (get-full-path "tags-in-vim-format"))
         (dir (get-full-path "")))
    ;; clear history
    (setq fastctags-nav-tag-history nil)
    (setq cands (fastctags-nav-extract-cands tags-file "fastctags" t))
    (should (eq (length cands) 3))
    ;; only add tag when it's accessed by user manually
    (should (not fastctags-nav-tag-history))
    (dolist (c cands) (fastctags-nav-remember c dir))
    (should fastctags-nav-tag-history)
    (should (eq (length fastctags-nav-tag-history) 3))))

(ert-run-tests-batch-and-exit)
