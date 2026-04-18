;;; jiraid.el --- Overlay the ticket summary by JIRA ticket ID -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Masayuki Ataka <masayuki.ataka@gmail.com>

;; Author: Masayuki Ataka <masayuki.ataka@gmail.com>
;; URL: https://github.com/ataka/jiraid
;; Keywords: web
;; Version: 0.1

;;; Commentary:

;; jiraid.el provides functionality to fetch JIRA ticket summaries
;; and display them inline in Emacs buffers.  When a buffer contains
;; JIRA ticket IDs (e.g., "PROJ-123"), this package can look up the
;; corresponding ticket summary via the JIRA REST API and display it
;; as an overlay next to the ticket ID, or insert it directly into
;; the buffer text.

;;; Setup:

;; Set the following variables before use:
;;
;;   (setq jiraid-base-url "https://your-domain.atlassian.net")
;;   (setq jiraid-user-email "you@example.com")
;;   (setq jiraid-api-token "your-api-token")
;;   (setq jiraid-ticket-prefix-list '("PROJ" "TEAM"))

;;; Usage:

;; - `jiraid-add-overlay-buffer': Add overlays for all ticket IDs in
;;   the current buffer.
;; - `jiraid-add-overlay': Add overlays for ticket IDs in the region.
;; - `jiraid-insert-summary': Insert ticket summaries as text in the
;;   region.
;; - `jiraid-clear-overlays': Remove all jiraid overlays.
;; - `jiraid-toggle-title-overlays': Toggle overlay visibility.
;; - `jiraid-get-ticket-summary': Interactively look up a ticket
;;   summary by number.
;; - `jiraid-test-connection': Test JIRA API connectivity.

;;; Code:

(require 'url)
(require 'json)

;;; Custom Variables

(defgroup jiraid nil
  "Jiraid configuration."
  :group 'text)

(defcustom jiraid-base-url ""
  "JIRA Base URL such as https://sample.atlassian.net"
  :type 'string
  :group 'jiraid)

(defcustom jiraid-user-email ""
  "JIRA login email address"
  :type 'string
  :group 'jiraid)

(defcustom jiraid-api-token ""
  "JIRA API token"
  :type 'string
  :group 'jiraid)

(defvar jiraid-ticket-prefix-list nil)

(defvar jiraid-overlay-function 
  (lambda (id title)
    (format " %s" title))
  "overlayに表示するタイトルを作成する関数

JIRA ID とタイトルを引数として受け取り、タイトル文字列を返します。")

;;; System Variables

(defvar jiraid-cache nil)

(defvar jiraid-overlay-list nil
  "作成されたoverlayのリスト")

(defun jiraid-insert-summary (start end)
  (interactive "r")
  (jiraid--make-cache start end)
  (jiraid-insert-title start end))

(defun jiraid-add-overlay (start end)
  (interactive "r")
  (jiraid--make-cache start end)
  (jiraid-add-title-overlays))

;;;###autoload
(defun jiraid-add-overlay-buffer ()
  (interactive)
  (jiraid-add-overlay (point-min) (point-max)))

(defun jiraid--make-cache (start end)
  (save-excursion
    (save-restriction
      (goto-char start)
      (narrow-to-region start end)
      (let ((pattern (jiraid--ticket-prefix-pattern))
            ids)
        (while (re-search-forward pattern nil t)
          (let ((id (match-string 0)))
            (unless (member id ids)
              (setq ids (cons id ids)))))
        (mapc (lambda (id)
                (unless (assoc id jiraid-cache)
                  (setq jiraid-cache (cons (cons id (jiraid-get-ticket-fields id)) jiraid-cache))))
              ids)
        ))))


(defun jiraid-get-ticket-summary-nointeractive (ticket-number)
  "JIRAのチケット番号からサマリを取得する

TICKET-NUMBER は 'PROJ-123' のような形式のチケット番号。
戻り値はチケットのサマリ文字列、エラーの場合は nil。"
  (let* ((url (format "%s/rest/api/3/issue/%s" jiraid-base-url ticket-number))
         (auth-string (base64-encode-string 
                       (format "%s:%s" jiraid-user-email jiraid-api-token) t))
         (url-request-method "GET")
         (url-request-extra-headers 
          `(("Authorization" . ,(format "Basic %s" auth-string))
            ("Accept" . "application/json")
            ("Content-Type" . "application/json"))))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url t t 30)
          (goto-char (point-min))
          (re-search-forward "\n\n" nil t)

          (let* ((json-str (buffer-substring-no-properties (point) (point-max)))
                 (json-data (json-parse-string json-str :object-type 'alist))
                 (fields (cdr (assoc 'fields json-data)))
                 (summary (cdr (assoc 'summary fields))))
            (kill-buffer (current-buffer))
            summary))
      (error 
       (message "JIRA チケット取得エラー: %s" (error-message-string err))
       nil))))

(defun jiraid-get-ticket-fields (ticket-number)
  "JIRAのチケット番号からフィールドを取得する

TICKET-NUMBER は 'PROJ-123' のような形式のチケット番号。
戻り値はチケットのフィールド、エラーの場合は nil。"
  (let* ((url (format "%s/rest/api/3/issue/%s" jiraid-base-url ticket-number))
         (auth-string (base64-encode-string 
                       (format "%s:%s" jiraid-user-email jiraid-api-token) t))
         (url-request-method "GET")
         (url-request-extra-headers 
          `(("Authorization" . ,(format "Basic %s" auth-string))
            ("Accept" . "application/json")
            ("Content-Type" . "application/json"))))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url t t 30)
          (goto-char (point-min))
          (re-search-forward "\n\n" nil t)

          (let* ((json-str (buffer-substring-no-properties (point) (point-max)))
                 (json-data (json-parse-string json-str :object-type 'alist))
                 (fields (cdr (assoc 'fields json-data))))
            (kill-buffer (current-buffer))
            fields))
      (error
       (message "JIRA チケット取得エラー: %s" (error-message-string err))
       nil))))

(defun jiraid-get-ticket-summary-internal (ticket-number)
  (let ((fields (jiraid-get-ticket-fields ticket-number)))
    (cdr (assoc 'summary fields))))

(defun jiraid-get-ticket-summary ()
  "インタラクティブにチケット番号を入力してサマリを取得する."
  (interactive)
  (let* ((ticket-number (read-string "チケット番号を入力してください (例: PROJ-123): "))
         (summary (jiraid-get-ticket-summary-nointeractive ticket-number)))
    (if summary
        (message "チケット %s のサマリ: %s" ticket-number summary)
      (message "チケット %s の取得に失敗しました" ticket-number))))

;; 使用例とテスト関数
(defun jiraid-test-connection ()
  "JIRA接続をテストする関数."
  (interactive)
  (let ((test-ticket (read-string "テスト用チケット番号を入力: ")))
    (message "接続テスト中...")
    (let ((summary (jiraid-get-ticket-summary-nointeractive test-ticket)))
      (if summary
          (message "✓ 接続成功! チケット: %s, サマリ: %s" test-ticket summary)
        (message "✗ 接続失敗。設定を確認してください。")))))

;; overlay

(defun jiraid-add-title-overlays ()
  "バッファ内のJIRAID IDを検索し、対応するタイトルをoverlayで表示する"
  (interactive)
  ;; 既存のoverlayをクリア
  (jiraid-clear-overlays)
  (save-excursion
    (goto-char (point-min))
    ;; JIRA-数字のパターンを検索
    (let ((pattern (jiraid--ticket-prefix-pattern)))
      (while (re-search-forward pattern nil t)
        (let* ((jiraid-id (match-string 0))
               (fields (cdr (assoc jiraid-id jiraid-cache)))
               (title (cdr (assoc 'summary fields)))
               (beg-pos (match-beginning 0))
               (end-pos (match-end 0)))
          (when title
            ;; overlayを作成して表示
            (let ((ov (make-overlay beg-pos end-pos)))
              (overlay-put ov 'after-string 
                           (propertize (apply jiraid-overlay-function `(,jiraid-id ,title))
                                       ;; 'face 'font-lock-comment-face))
                                       'face 'markdown-markup-face))
              (overlay-put ov 'jiraid-title-overlay t)
              (overlay-put ov 'evaporate t)
              (push ov jiraid-overlay-list))))))))

(defun jiraid-insert-title (start end)
  (save-restriction
    (narrow-to-region start end)
    (save-excursion
      (goto-char end)
      ;; JIRA-数字のパターンを検索
      (let ((pattern (jiraid--ticket-prefix-pattern)))
        (while (re-search-backward pattern nil t)
          (let* ((jiraid-id (match-string 0))
                 (fields (cdr (assoc jiraid-id jiraid-cache)))
                 (title (cdr (assoc 'summary fields)))
                 (end-pos (match-end 0)))
            (when title
              ;; overlayを作成して表示
              (save-excursion
                (goto-char end-pos)
                (insert (format " %s" title))))))))))

(defun jiraid-clear-overlays ()
  "すべてのJIRAIDタイトルoverlayを削除する"
  (interactive)
  (dolist (ov jiraid-overlay-list)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq jiraid-overlay-list nil))

(defun jiraid-toggle-title-overlays ()
  "JIRAIDタイトルoverlayの表示/非表示を切り替える"
  (interactive)
  (if jiraid-overlay-list
      (jiraid-clear-overlays)
    (jiraid-add-title-overlays)))

(defun jiraid-remove-id-from-cache (id)
  (interactive (list (jiraid--completing-read)))
  (setq jiraid-cache (assoc-delete-all id jiraid-cache)))

;; misc

(defun jiraid--get-summary-by-id (id)
  (or (cdr (assoc 'summary (assoc id jiraid-cache))) ""))
  
(defun jiraid--affixation (candidates)
  (mapcar
   (lambda (id)
     (let ((summary (jiraid--get-summary-by-id id)))
       (list
        id
        "" ; prefix
        (concat
         "  "
         (propertize summary 'face 'completions-annotations)))))
   candidates))

(defun jiraid--collection (string pred action)
  (let ((alist (mapcar
                (lambda (ticket)
                  (let* ((id (substring-no-properties (car ticket)))
                         (summary (jiraid--get-summary-by-id id)))
                    (cons id summary)))
                jiraid-cache)))
    (if (eq action 'metadata)
        '(metadata
          (category . jiraid)
          (affixation-function . jiraid--affixation))
      (complete-with-action action alist string pred))))

(defun jiraid--completing-read ()
  (completing-read "JIRA Ticket: " #'jiraid--collection nil t))

(defun jiraid--ticket-prefix-pattern ()
  (format "\\b%s-[0-9]+\\b" (regexp-opt jiraid-ticket-prefix-list)))


(provide 'jiraid)
;;; jiraid.el ends here
