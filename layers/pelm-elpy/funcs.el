;; -*- lexical-binding: t -*-
;;
;; Author: eggcaker <eggcaker@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:


(defun elpy/insert-codecell-above ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert "# <codecell>\n")))

(defun elpy/insert-markdowncell-above ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert "# <markdowncell>\n")))
