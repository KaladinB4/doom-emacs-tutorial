;; -*- lexical-binding: t; -*-
;; doom-chronicles.el --- Phase 1: The Spark

;; Copyright (C) 2023 Your Name
;; Author: Your Name <your.email@example.com>
;; Maintainer: Your Name <your.email@example.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: MIT
;;; Commentary:
;; Basic interactive tutorial for DOOM Emacs - Phase 1.
;; Focuses on core mechanics: modes, basic movement/editing, SPC, feedback.

;;; Code:

(require 'ovarray) ; For overlays
(require 'subr-x)  ; For when-let, etc.

;;----------------------------------------------------------------------------
;; Customization Group & Variables
;;----------------------------------------------------------------------------
(defgroup doom-chronicles nil
  "Interactive DOOM Emacs tutorial game."
  :group 'applications
  :prefix "doom-chronicles-")

(defcustom doom-chronicles-progress-file (expand-file-name "doom-chronicles-progress.el" doom-private-dir)
  "File to store minimal progress."
  :type 'string
  :group 'doom-chronicles)

;;----------------------------------------------------------------------------
;; Faces for Feedback
;;----------------------------------------------------------------------------
(defface doom-chronicles-success-face
  '((t :foreground "green" :weight bold))
  "Face for success feedback."
  :group 'doom-chronicles)

(defface doom-chronicles-error-face
  '((t :foreground "red" :weight bold))
  "Face for error/hint feedback."
  :group 'doom-chronicles)

(defface doom-chronicles-prompt-face
  '((t :foreground "cyan" :weight bold))
  "Face for challenge prompts."
  :group 'doom-chronicles)

(defface doom-chronicles-mode-indicator-face
  '((t :foreground "orange" :weight bold :box (:line-width 1 :color "orange")))
  "Face for the special mode line indicator."
  :group 'doom-chronicles)


;;----------------------------------------------------------------------------
;; Buffer-Local State
;;----------------------------------------------------------------------------
(defvar-local doom-chronicles--current-challenge-index 0)
(defvar-local doom-chronicles--challenges nil) ; Will be populated
(defvar-local doom-chronicles--expected-keys nil)
(defvar-local doom-chronicles--keys-pressed '())
(defvar-local doom-chronicles--original-mode-line-format nil)

;;----------------------------------------------------------------------------
;; Challenge Data (Phase 1)
;;----------------------------------------------------------------------------
;; Structure: (:prompt "..." :goal-keys ("j" "j" "j") :success "..." :setup (lambda ()) :teardown (lambda ()))
;; Goal Keys: A list of key descriptions (like "C-c C-c") or specific chars.
;;            For Phase 1, we primarily use single chars or simple sequences.
(defun doom-chronicles--setup-phase1-challenges ()
  (setq doom-chronicles--challenges
        '(;; 1. Basic Movement Down
          {:prompt "Welcome, Apprentice! The Text Dimension awaits. Move the cursor DOWN 3 times. Press 'j' three times."
           :goal-keys ("j" "j" "j")
           :success "Excellent! You feel the flow of text."
           :setup (lambda () (goto-char (point-min)))}

         ;; 2. Basic Movement Up
         {:prompt "Now, return UP 2 times. Press 'k' twice."
          :goal-keys ("k" "k")
          :success "Precise control!"
          :setup (lambda () (goto-char (+ (point-min) 40)))} ; Ensure cursor isn't at top

         ;; 3. Enter Insert Mode
         {:prompt "Time to shape the void. Enter INSERT mode. Press 'i'."
          :goal-keys ("i")
          :success "You are now in INSERT mode! Notice the mode line change."}

         ;; 4. Type Text & Exit Insert Mode
         {:prompt "In INSERT mode, type the word 'hello'. Then return to NORMAL mode (press ESC or 'f' then 'd')."
          :goal-keys ("h" "e" "l" "l" "o" "ESC") ; We'll handle fd alternative later
          :success "Text manifested! And you're safely back in NORMAL mode."
          :setup (lambda () (insert " "))} ; Ensure space after cursor

         ;; 5. Delete Character
         {:prompt "An anomaly! Delete the character under the cursor. Press 'x'."
          :goal-keys ("x")
          :success "Anomaly neutralized!"
          :setup (lambda () (insert "X") (backward-char))}

         ;; 6. Delete Line
         {:prompt "This entire line is redundant. Delete it. Press 'd' twice ('dd')."
          :goal-keys ("d" "d")
          :success "Line banished!"
          :setup (lambda () (goto-char (line-beginning-position)))}

         ;; 7. Undo
         {:prompt "Wait, perhaps that was hasty. Undo the last action. Press 'u'."
          :goal-keys ("u")
          :success "Reality restored! Undo is powerful."}

         ;; 8. Press SPC
         {:prompt "The SPACE bar holds many secrets in DOOM. Press 'SPC'."
          :goal-keys ("SPC")
          :success "Behold! The which-key menu. A map to power! Press ESC to close it for now."
          :teardown (lambda () (message "Press ESC to dismiss which-key if it's still open."))}

         ;; 9. End of Phase 1
         {:prompt "You've grasped the basics, Apprentice! Phase 1 complete. More awaits..."
          :goal-keys () ; No keys needed
          :success "You feel a surge of potential!"}
          )))

;;----------------------------------------------------------------------------
;; Core Game Logic Functions
;;----------------------------------------------------------------------------
(defun doom-chronicles--get-current-challenge ()
  (nth doom-chronicles--current-challenge-index doom-chronicles--challenges))

(defun doom-chronicles--display-challenge ()
  "Display the current challenge prompt."
  (interactive)
  (when-let ((challenge (doom-chronicles--get-current-challenge)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize (plist-get challenge :prompt) 'face 'doom-chronicles-prompt-face))
      (insert "\n\n--- Start Typing Below ---\n")
      (goto-char (point-max))
      (when-let ((setup-fn (plist-get challenge :setup)))
        (funcall setup-fn))
      (setq doom-chronicles--expected-keys (copy-sequence (plist-get challenge :goal-keys)))
      (setq doom-chronicles--keys-pressed '()))))

(defun doom-chronicles--show-feedback (message &optional face position duration)
  "Display feedback message and optional overlay."
  (message "Chronicles: %s" message)
  (when (and face position)
    (let ((overlay (make-overlay (max (point-min) (1- position)) position)))
      (overlay-put overlay 'face face)
      (overlay-put overlay 'doom-chronicles-feedback t)
      (run-with-timer (or duration 0.5) nil #'delete-overlay overlay))))

(defun doom-chronicles--advance-challenge ()
  "Move to the next challenge."
  (when-let ((challenge (doom-chronicles--get-current-challenge)))
     (when-let ((teardown-fn (plist-get challenge :teardown)))
       (funcall teardown-fn)))
  (setq doom-chronicles--current-challenge-index (1+ doom-chronicles--current-challenge-index))
  (doom-chronicles--save-progress)
  (if (< doom-chronicles--current-challenge-index (length doom-chronicles--challenges))
      (doom-chronicles--display-challenge)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "Congratulations! You have completed all available challenges in this phase!" 'face 'doom-chronicles-success-face)))))

(defun doom-chronicles--validate-key (key-sequence)
  "Validate the pressed key/sequence against the current challenge goal."
  (interactive (list last-input-event)) ; Capture the raw event

  ;; --- Basic Event to String Conversion (Simplify for Phase 1) ---
  ;; This needs refinement for complex keys (C-c, M-x etc.) but works for chars & SPC/ESC
  (let ((key-str
         (cond
          ((integerp key-sequence) (char-to-string key-sequence)) ; Simple character
          ((symbolp key-sequence) (symbol-name key-sequence)) ; Symbols like 'escape, 'space
          ((vectorp key-sequence) ; Handle sequences like `fd` for ESC
           (mapconcat #'event-description key-sequence ""))
          (t (event-description key-sequence))))) ; Fallback

    ;; Special handling for ESC alternative 'fd' in evil-mode
    (when (and evil-local-mode (string= key-str "f"))
      (setq-local unread-command-events (list (read-event)))
      (if (eq (car unread-command-events) ?d)
          (progn
            (setq key-str "ESC") ; Treat 'fd' as ESC for validation
            (setq unread-command-events nil)) ; Consume the 'd'
        ;; If 'f' wasn't followed by 'd', put the next event back
        (setq unread-command-events (list (car unread-command-events)))))

    (if (not doom-chronicles--expected-keys)
        (progn
          (doom-chronicles--show-feedback "No challenge active or challenge completed." 'doom-chronicles-error-face (point))
          (call-interactively (key-binding (this-command-keys)))) ; Try to execute normally if no challenge
      (let ((expected (car doom-chronicles--expected-keys)))
        (if (string= key-str expected)
            ;; Correct key pressed
            (progn
              (setq doom-chronicles--keys-pressed (cons key-str doom-chronicles--keys-pressed))
              (setq doom-chronicles--expected-keys (cdr doom-chronicles--expected-keys))
              (doom-chronicles--show-feedback (format "Correct! '%s' pressed." key-str) 'doom-chronicles-success-face (point))

              ;; --- Execute the actual command ---
              ;; This allows the user to SEE the effect (j moves down, i enters insert mode)
              ;; We need to be careful here for commands that expect further input.
              ;; For Phase 1, we call the command corresponding to the key.
              (let ((cmd (key-binding (vector (read-kbd-macro key-str)))))
                 (when cmd
                   ;; Special case: Entering insert mode requires different handling
                   (if (string= key-str "i")
                       (progn (evil-insert-state) (message "Entered INSERT mode"))
                     ;; Special case: ESC needs to trigger evil-normal-state
                     (if (string= key-str "ESC")
                         (progn (evil-normal-state) (message "Returned to NORMAL mode"))
                       ;; Default: Try to execute the bound command
                       (progn
                         (setq this-command cmd) ; Make sure the command knows itself
                         (call-interactively cmd))))))


              (if (null doom-chronicles--expected-keys)
                  ;; Challenge Complete!
                  (progn
                    (doom-chronicles--show-feedback (plist-get (doom-chronicles--get-current-challenge) :success) 'doom-chronicles-success-face (point) 1.5)
                    (run-with-timer 1.6 nil #'doom-chronicles--advance-challenge))
                (doom-chronicles--show-feedback (format "Next: '%s'" (car doom-chronicles--expected-keys)) nil (point))))
          ;; Incorrect key pressed
          (progn
            (doom-chronicles--show-feedback (format "Incorrect. Expected '%s', but got '%s'." expected key-str) 'doom-chronicles-error-face (point))
            ;; Maybe reset sequence? For Phase 1, just give feedback.
            ))))))


;;----------------------------------------------------------------------------
;; Persistence Functions
;;----------------------------------------------------------------------------
(defun doom-chronicles--save-progress ()
  "Save the current challenge index."
  (condition-case err
      (with-temp-file doom-chronicles-progress-file
        (print `(:current-challenge-index ,doom-chronicles--current-challenge-index)
               (current-buffer)))
    (error (message "Chronicles Error saving progress: %s" (error-message-string err)))))

(defun doom-chronicles--load-progress ()
  "Load the last saved challenge index."
  (if (file-exists-p doom-chronicles-progress-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents doom-chronicles-progress-file)
            (let ((data (read (current-buffer))))
              (plist-get data :current-challenge-index)))
        (error (message "Chronicles Error loading progress: %s. Starting from scratch." (error-message-string err))
               0))
    0))

;;----------------------------------------------------------------------------
;; Mode Setup & Activation
;;----------------------------------------------------------------------------
(defun doom-chronicles--setup-buffer ()
  "Set up the buffer for the tutorial."
  (setq buffer-read-only nil) ; Make sure we can modify
  (doom-chronicles--setup-phase1-challenges)
  (setq doom-chronicles--current-challenge-index (doom-chronicles--load-progress))
  (doom-chronicles--display-challenge)
  (setq buffer-read-only t) ; Make read-only to prevent accidental edits outside challenges
  ;; --- Enhance Mode Line for Clarity ---
  (setq-local doom-chronicles--original-mode-line-format mode-line-format)
  (setq-local mode-line-format
              (cons '(:eval (propertize " [DOOM CHRONICLES] " 'face 'doom-chronicles-mode-indicator-face))
                    mode-line-format)))

(defun doom-chronicles--cleanup-buffer ()
  "Restore buffer state when mode is turned off."
  (when doom-chronicles--original-mode-line-format
    (setq mode-line-format doom-chronicles--original-mode-line-format)))

;; Define the minor mode
(define-minor-mode doom-chronicles-mode
  "Minor mode for the Doom Chronicles interactive tutorial."
  :init-value nil
  :lighter " Chronicles"
  :keymap (let ((map (make-sparse-keymap)))
            ;; --- Override Keys We Want to Teach/Validate ---
            (mapc (lambda (key)
                    (define-key map (kbd key) #'doom-chronicles--validate-key))
                  ;; Keys validated in Phase 1 challenges:
                  '("j" "k" "i" "h" "e" "l" "o" "x" "d" "u" "SPC" "<escape>" "f")) ;; Include 'f' for 'fd' combo
            ;; Add a basic help key
            (define-key map (kbd "C-c C-h") #'doom-chronicles-help)
            map)
  :global nil ; Make it buffer-local
  (if doom-chronicles-mode
      (progn
        (setq-local evil-local-mode t) ; Ensure evil-mode is active locally
        (evil-normal-state) ; Start in normal mode
        (doom-chronicles--setup-buffer))
    ;; Cleanup when mode is turned off
    (doom-chronicles--cleanup-buffer)))

;;----------------------------------------------------------------------------
;; User-Facing Commands
;;----------------------------------------------------------------------------
(defun doom-chronicles-start ()
  "Start the Doom Chronicles tutorial."
  (interactive)
  (let ((buffer (get-buffer-create "*Doom Chronicles*")))
    (with-current-buffer buffer
      (if (not doom-chronicles-mode) ; Only initialize if not already running
          (doom-chronicles-mode 1)
        (doom-chronicles--display-challenge)) ; Redisplay current challenge if mode already on
      (switch-to-buffer buffer))))

(defun doom-chronicles-help ()
  "Show help for the current challenge."
  (interactive)
  (when-let ((challenge (doom-chronicles--get-current-challenge)))
    (message "Current Goal: %s" (plist-get challenge :prompt))
    (message "Expected Keys Remaining: %s" doom-chronicles--expected-keys)))


;;----------------------------------------------------------------------------
;; Provide Feature
;;----------------------------------------------------------------------------
(provide 'doom-chronicles)

;;; doom-chronicles.el ends here
