;;; packages.el --- pelm-contact layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Egg Caker <eggcaker@Eggs-MacBook-Pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst pelm-contact-packages
	'(ebdb counsel-ebdb ebdb-company))

(defun pelm-contact/init-ebdb()
	(use-package ebdb
		:defer
		:commands (ebdb ebdb-create-record )
		:init
		(progn
			(spacemacs/set-leader-keys "a C" 'ebdb)
			(global-set-key (kbd "C-x c") 'ebdb)
			(with-eval-after-load 'ebdb

			(defvar ebdb-wechat-label-list '("wechat")
				"List of known wechat type.")

			(defclass ebdb-field-wechat (ebdb-field-labeled
																		ebdb-field-user)
				((label-list
					 :initform ebdb-wechat-label-list)
					(wechat
						:type string
						:initarg :wechat
						:custom string
						:initform ""))
				:human-readable "wechat"
				:documentation "A field holding weichat information about this record.")


			(cl-defmethod ebdb-read ((class (subclass ebdb-field-wechat)) &optional slots obj)
				(let ((wechat (ebdb-read-string "Wechat id: "
												(when obj (slot-value obj 'wechat)))))
					(cl-call-next-method class (plist-put slots :wechat wechat) obj)))

			(cl-defmethod ebdb-parse ((class (subclass ebdb-field-wechat)) str &optional slots)
				(when (and (null (plist-get slots :wechat))
								(string-trim str "none"))
					(setq slots (plist-put slots :wechat (string-trim str))))
				(cl-call-next-method class str slots))

			(cl-defmethod ebdb-string ((field ebdb-field-wechat))
				(slot-value field 'wechat))

				)
			)))

(defun pelm-contact/init-counsel-ebdb()
	(use-package counsel-ebdb
		:defer t
		:init (spacemacs/set-leader-keys
						"cs" 'counsel-ebdb)))

(defun pelm-contact/init-ebdb-company()
	(use-package company-ebdb
		:defer t
		:init (spacemacs|add-company-backends
						:backends company-ebdb
						:modes ebdb-mode
						)))





;;; packages.el ends here
