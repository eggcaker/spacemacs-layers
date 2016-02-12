

;; Org non-standard faces.
(defface pelm-org-created-kwd
  '((t (:weight normal :box (:line-width 1 :color "#EEE9C3")
                :foreground "#1A1A1A" :background "#FDFCD8")))
  "Face used to display state NEW.")

(defface pelm-org-in-progress-kwd
  '((t (:weight bold :box (:line-width 1 :color "#D9D14A")
                :foreground "#D9D14A" :background "#FCFCDC")))
  "Face used to display state INPROGRESS.")

(defface pelm-org-waiting-for-kwd
  '((t (:weight bold :box (:line-width 1 :color "#89C58F")
                :foreground "#89C58F" :background "#E2FEDE")))
  "Face used to display state WAIT.")
(defface pelm-org-someday-kwd
  '((t (:weight bold :box (:line-width 1 :color "#9EB6D4")
                :foreground "#9EB6D4" :background "#E0EFFF")))
  "Face used to display state SOMEDAY.")

(defface pelm-org-quote-kwd
  '((t (:weight bold :box (:line-width 1 :color "#FC5158")
                :foreground "#FC5158" :background "#FED5D7")))
  "Face used to display .")

(defface pelm-org-quoted-kwd
  '((t (:weight bold :box (:line-width 1 :color "#55BA80")
                :foreground "#55BA80" :background "#DFFFDF")))
  "Face used to display .")

(defface pelm-org-approved-kwd
  '((t (:weight bold :box (:line-width 1 :color "#969696")
                :foreground "#969696" :background "#F2F2EE")))
  "Face used to display .")

(defface pelm-org-rejected-kwd
  '((t (:weight bold :box (:line-width 1 :color "#42B5FF")
                :foreground "#42B5FF" :background "#D3EEFF")))
  "Face used to display state REJECTED.")

(defface pelm-org-openpo-kwd
  '((t (:weight bold :box (:line-width 1 :color "#FC5158")
                :foreground "#FC5158" :background "#FED5D7")))
  "Face used to display OPEN purchase order.")

(defface pelm-org-closedpo-kwd
  '((t (:weight bold :box (:line-width 1 :color "#969696")
                :foreground "#969696" :background "#F2F2EE")))
  "Face used to display CLOSED purchase order.")
