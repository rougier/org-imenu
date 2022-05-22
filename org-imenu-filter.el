;;; org-imenu-filter.el --- org imenu filter -*- lexical-binding: t -*-

;; Copyright (C) 2022 Nicolas P. Rougier

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/org-imenu-filter
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  org-imenu-filter mode allows to filter imenu when in org-mode. Once mode is
;;  active, user can select a specific filter using the interactive
;;  org-imenu-filter-select to specify a filter pattern.
;;
;;  When used in combination with imenu-list, this allows to have a sidebar
;;  with filtered entries.
;;
;;  Usage:
;;
;;  M-x : org-imenu-filter-mode
;;
;;; NEWS:
;;
;; Version 0.1
;; - First version
;;

;;; Code
(require 'org)

(defgroup org-imenu-filter nil
  "Org imenu filter"
  :group 'org-structure)

(defvar org-imenu-filter-history '()
  "Filter history list.")

(defcustom org-imenu-filter-prompt "FILTER: "
  "Prompt for the filter interactive method."
  :group 'org-imenu-filter
  :type 'string)

(defcustom org-imenu-filter-todo nil
  "Wheter to show todo in Imenu."
  :group 'org-imenu-filter
  :type 'boolean)

(defcustom org-imenu-filter-tags t
  "Whether to show tags in Imenu."
  :group 'org-imenu-filter
  :type 'boolean)

(defvar org-imenu-filter-current
  (cdr (org-make-tags-matcher "*"))
  "Filter (function) to decide if a headline is kept. You can provide your own function (filter (todo tags level) ...).")

(defun org-imenu-filter-select ()
  "Select a new filter using org-mode search syntax."
  
  (interactive)
  (let* ((match (completing-read-multiple
                 org-imenu-filter-prompt
                 org-imenu-filter-history
                 nil nil nil
                 'org-imenu-filter-history))
         (match (mapconcat #'identity match " ")))
    (when (string= "" match)
      (setq match "*"))
    (setq org-imenu-filter-current
          (cdr (org-make-tags-matcher match)))
    (when (fboundp 'imenu-list-refresh)
          (imenu-list-refresh))))

(defun org-imenu-filter-format (element todo tags marker)
  "Format ELEMENT, TODO and TAGS as a string in order to insert it the index with the MARKER reference indicating where the element is in the document."
  
  (let* ((node (org-element-property :raw-value element))
         (node (org-link-display-format (substring-no-properties node)))
         (node (concat
                (when (and org-imenu-filter-todo todo)
                  (format "%s " todo))
                node
                (when (and org-imenu-filter-tags tags)
                  (propertize
                   (format " :%s:" (mapconcat 'identity  tags ":"))
                   'face 'org-tag))))
         (node (propertize node 'marker marker
                                'org-imenu-marker marker
                                'org-imenu t)))
    node))


(defun org-imenu-filter-get-tree (&optional bound parent-match)
  "Produce a filtered index for Imenu. Index is build from point to BOUND taking into account PARENT-MATCH that indicate if the parent node has been filtered out or not. Filtering is done using org-imenu-filter-current."
  
  (let* ((imenu '()))
    (save-excursion
      (org-with-wide-buffer
       (unless bound
         (setq bound (point-max))
         (goto-char (point-min)))
       (while (re-search-forward org-heading-regexp bound t)
         (let* ((element (org-element-at-point))
                (begin (org-element-property :begin element))
                (end (org-element-property :end element))
                (marker (copy-marker begin))
                (level (org-element-property :level element))
                (todo (org-element-property :todo-keyword element))
                (tags (save-excursion
                         (goto-char begin)
                          (org-get-tags)))
                (match (save-excursion
                         (goto-char begin)
                         (funcall org-imenu-filter-current
                                  nil (org-get-tags) level)))
                (node (org-imenu-filter-format element todo tags marker))
                (children (org-imenu-filter-get-tree end match)))
           (goto-char end)
           (cond ((> level org-imenu-depth)
                  nil)
                 ((> (length children) 0)
                  (add-to-list 'imenu (append (list node) children) t))
                 ((or match parent-match)
                  (add-to-list 'imenu (cons node marker) t)))))))
    imenu))


(define-minor-mode org-imenu-filter-mode
  "Minor mode for filtering org imenu entries."

  :global t

  (if org-imenu-filter-mode
      (advice-add #'org-imenu-get-tree :override #'org-imenu-filter-get-tree)
    (advice-remove #'org-imenu-get-tree #'org-imenu-filter-get-tree)))


(provide 'org-imenu-filter)
;;; org-imenu-filter.el ends here
