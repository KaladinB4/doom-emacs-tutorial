;; -*- lexical-binding: t; -*-
;; doom-chronicles.el --- An interactive tutorial game for mastering DOOM Emacs

;; Copyright (C) 2025 KaladinB4
;; Author: KaladinB4 <kaladinb4>
;; Maintainer: KaladinB4
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (evil "1.14.0") (which-key "3.0.0"))
;; SPDX-License-Identifier: MIT
;; Keywords: convenience, games, learning
;; URL: https://github.com/kaladinb4/doom-chronicles

;;; Commentary:
;; Doom Chronicles is an interactive tutorial game designed to teach DOOM Emacs
;; through progressive challenges and immediate feedback.  It guides users from
;; zero knowledge to intermediate proficiency through engaging exercises that
;; focus on building muscle memory and mental models.
;;
;; Phase 1: The Spark - Focuses on core mechanics: modal editing, basic
;; movement/editing, and the essential leader key (SPC).

;;; Code:

(require 'evil)
(require 'which-key)
(require 'subr-x)  ; For when-let, etc.

;;----------------------------------------------------------------------------
;; Customization Group & Variables
;;----------------------------------------------------------------------------
(defgroup doom-chronicles nil
  "Interactive DOOM Emacs tutorial game."
  :group 'applications
  :prefix "doom-chronicles-")

(defcustom doom-chronicles-progress-file (expand-file-name "doom-chronicles-progress.el" doom-private-dir)
  "File to store progress data."
  :type 'string
  :group 'doom-chronicles)

(defcustom doom-chronicles-use-animations t
  "Whether to use animations in feedback and visualizations."
  :type 'boolean
  :group 'doom-chronicles)

(defcustom doom-chronicles-show-hints-automatically nil
  "Whether to show hints automatically after multiple failed attempts."
  :type 'boolean
  :group 'doom-chronicles)

(defcustom doom-chronicles-hint-delay 3
  "Number of failed attempts before showing a hint automatically."
  :type 'integer
  :group 'doom-chronicles)

;;----------------------------------------------------------------------------
;; Faces for Feedback
;;----------------------------------------------------------------------------
(defface doom-chronicles-success-face
  '((((class color) (background light))
     :foreground "forest green" :weight bold)
    (((class color) (background dark))
     :foreground "green" :weight bold))
  "Face for success feedback."
  :group 'doom-chronicles)

(defface doom-chronicles-error-face
  '((((class color) (background light))
     :foreground "firebrick" :weight bold)
    (((class color) (background dark))
     :foreground "red" :weight bold))
  "Face for error/hint feedback."
  :group 'doom-chronicles)

(defface doom-chronicles-prompt-face
  '((((class color) (background light))
     :foreground "royal blue" :weight bold)
    (((class color) (background dark))
     :foreground "cyan" :weight bold))
  "Face for challenge prompts."
  :group 'doom-chronicles)

(defface doom-chronicles-hint-face
  '((((class color) (background light))
     :foreground "dark orange" :slant italic)
    (((class color) (background dark))
     :foreground "gold" :slant italic))
  "Face for hints."
  :group 'doom-chronicles)

(defface doom-chronicles-header-face
  '((t :inherit variable-pitch :height 1.2 :weight bold :underline t))
  "Face for headers and titles."
  :group 'doom-chronicles)

(defface doom-chronicles-mode-indicator-face
  '((((class color) (background light))
     :foreground "black" :background "orange" 
     :weight bold :box (:line-width 1 :color "orange"))
    (((class color) (background dark))
     :foreground "black" :background "orange"
     :weight bold :box (:line-width 1 :color "orange")))
  "Face for the special mode line indicator."
  :group 'doom-chronicles)

(defface doom-chronicles-progress-bar-face
  '((t :inherit doom-chronicles-success-face))
  "Face for the progress bar."
  :group 'doom-chronicles)

(defface doom-chronicles-progress-empty-face
  '((t :inherit shadow))
  "Face for the empty part of the progress bar."
  :group 'doom-chronicles)

(defface doom-chronicles-stats-face
  '((t :inherit variable-pitch :height 0.9))
  "Face for stats and metrics."
  :group 'doom-chronicles)

;;----------------------------------------------------------------------------
;; Buffer-Local State
;;----------------------------------------------------------------------------
(defvar-local doom-chronicles--current-challenge-index 0
  "Index of the current challenge.")

(defvar-local doom-chronicles--challenges nil
  "List of challenge data structures.")

(defvar-local doom-chronicles--expected-keys nil
  "List of expected key inputs for the current challenge.")

(defvar-local doom-chronicles--keys-pressed '()
  "List of keys pressed so far in the current challenge.")

(defvar-local doom-chronicles--original-mode-line-format nil
  "Original mode line format before entering doom-chronicles-mode.")

(defvar-local doom-chronicles--phase 1
  "Current tutorial phase (1-5).")

(defvar-local doom-chronicles--failed-attempts 0
  "Counter for failed attempts on the current challenge.")

(defvar-local doom-chronicles--challenge-start-time nil
  "Time when the current challenge was started.")

(defvar-local doom-chronicles--total-time 0
  "Total time spent on the current phase.")

(defvar-local doom-chronicles--statistics '()
  "Statistics about user performance for analytics.")

;;----------------------------------------------------------------------------
;; Challenge Data (Phase 1)
;;----------------------------------------------------------------------------
;; Challenge data structure:
;; (:prompt "..." 
;;  :goal-keys ("..." ...) 
;;  :success "..." 
;;  :hint "..."
;;  :setup (lambda () ...) 
;;  :teardown (lambda () ...)
;;  :explanation "..."
;;  :difficulty N)

(defun doom-chronicles--setup-phase1-challenges ()
  "Set up challenges for Phase 1: The Spark."
  (setq doom-chronicles--challenges
        '(;; 1. Basic Movement Down
          {:prompt "Welcome, Apprentice! The Text Dimension awaits. Move the cursor DOWN 3 times. Press 'j' three times."
           :goal-keys ("j" "j" "j")
           :success "Excellent! You feel the flow of text."
           :hint "In DOOM Emacs, navigation is primarily done using the h, j, k, l keys (←↓↑→)."
           :explanation "The 'j' key moves down in normal mode, inspired by vi/vim."
           :difficulty 1
           :setup (lambda () (goto-char (point-min)))}

         ;; 2. Basic Movement Up
         {:prompt "Now, return UP 2 times. Press 'k' twice."
          :goal-keys ("k" "k")
          :success "Precise control!"
          :hint "The 'k' key moves your cursor up in normal mode."
          :explanation "The 'k' key moves up, continuing the h, j, k, l navigation scheme."
          :difficulty 1
          :setup (lambda () (goto-char (+ (point-min) 40)))} ; Ensure cursor isn't at top

         ;; 3. Basic Movement Left/Right
         {:prompt "Let's practice horizontal movement. Press 'h' twice to move LEFT, then 'l' three times to move RIGHT."
          :goal-keys ("h" "h" "l" "l" "l")
          :success "Smooth navigation! You're getting the hang of movement."
          :hint "The 'h' key moves left, and 'l' key moves right in normal mode."
          :explanation "The h and l keys complete the basic movement keys in DOOM/Vim."
          :difficulty 1
          :setup (lambda () (insert "      Test text for horizontal movement      ")
                             (goto-char (+ (point-min) 20)))}

         ;; 4. Enter Insert Mode
         {:prompt "Time to shape the void. Enter INSERT mode. Press 'i'."
          :goal-keys ("i")
          :success "You are now in INSERT mode! Notice the mode line change."
          :hint "The 'i' key enters insert mode, allowing you to type text."
          :explanation "Insert mode is where you can type text normally, like in most editors."
          :difficulty 1}

         ;; 5. Type Text & Exit Insert Mode
         {:prompt "In INSERT mode, type the word 'hello'. Then return to NORMAL mode (press ESC or 'f' then 'd')."
          :goal-keys ("h" "e" "l" "l" "o" "ESC") ; We'll handle fd alternative later
          :success "Text manifested! And you're safely back in NORMAL mode."
          :hint "Type each letter of 'hello', then press ESC (or fd quickly) to return to normal mode."
          :explanation "The escape key returns you to normal mode from insert mode."
          :difficulty 2
          :setup (lambda () (insert " "))} ; Ensure space after cursor

         ;; 6. Delete Character
         {:prompt "An anomaly! Delete the character under the cursor. Press 'x'."
          :goal-keys ("x")
          :success "Anomaly neutralized!"
          :hint "The 'x' key in normal mode deletes the character under the cursor."
          :explanation "The 'x' key is a quick way to delete a single character."
          :difficulty 1
          :setup (lambda () (insert "X") (backward-char))}

         ;; 7. Delete Line
         {:prompt "This entire line is redundant. Delete it. Press 'd' twice ('dd')."
          :goal-keys ("d" "d")
          :success "Line banished!"
          :hint "The 'dd' sequence in normal mode deletes the entire current line."
          :explanation "The 'd' key is a powerful deletion command that works with motions or itself."
          :difficulty 2
          :setup (lambda () (goto-char (line-beginning-position)))}

         ;; 8. Undo
         {:prompt "Wait, perhaps that was hasty. Undo the last action. Press 'u'."
          :goal-keys ("u")
          :success "Reality restored! Undo is powerful."
          :hint "The 'u' key in normal mode undoes the last action."
          :explanation "Undo is crucial for recovering from mistakes."
          :difficulty 1}

         ;; 9. Word Movement
         {:prompt "Let's move by words now. Press 'w' three times to move forward by words."
          :goal-keys ("w" "w" "w")
          :success "Leaping across words! Much faster than character movement."
          :hint "The 'w' key in normal mode jumps to the start of the next word."
          :explanation "Word movement is much more efficient than character movement."
          :difficulty 2
          :setup (lambda () (insert "\nThis is a sentence with multiple words to practice word movement.\n")
                            (goto-char (point-max))
                            (search-backward "This" nil t))}

         ;; 10. Back Word Movement
         {:prompt "Now move backwards by words. Press 'b' twice to jump back."
          :goal-keys ("b" "b")
          :success "Backwards mastery! You're becoming efficient."
          :hint "The 'b' key in normal mode jumps back to the start of the previous word."
          :explanation "Combining 'w' and 'b' lets you navigate text quickly."
          :difficulty 2}

         ;; 11. End of Word Movement
         {:prompt "Let's jump to the end of words. Press 'e' twice."
          :goal-keys ("e" "e")
          :success "Precise ending! You can target the end of words."
          :hint "The 'e' key jumps to the end of the current or next word."
          :explanation "The 'e' key complements 'w' by targeting word endings."
          :difficulty 2}

         ;; 12. Beginning/End of Line
         {:prompt "Jump to the beginning of line with '0', then to the end with '$'."
          :goal-keys ("0" "$")
          :success "Line mastery! You can now target entire lines efficiently."
          :hint "The '0' key jumps to the start of a line, '$' to the end."
          :explanation "These keys give you quick access to line boundaries."
          :difficulty 2
          :setup (lambda () (insert "\nThis is a long line that you can use to practice jumping to the beginning and end.\n")
                            (goto-char (point-max))
                            (search-backward "This is a long" nil t)
                            (forward-char 5))}

         ;; 13. Visual Mode
         {:prompt "Enter VISUAL mode to select text. Press 'v'."
          :goal-keys ("v")
          :success "You've entered VISUAL mode! Notice the mode indicator change."
          :hint "The 'v' key enters character-wise visual selection mode."
          :explanation "Visual mode lets you select text before operating on it."
          :difficulty 2}

         ;; 14. Visual Selection
         {:prompt "In VISUAL mode, extend your selection by pressing 'e' to select to the end of a word."
          :goal-keys ("e")
          :success "Selection extended! Visual feedback shows what you're targeting."
          :hint "Use motion commands like 'e' to extend your visual selection."
          :explanation "All the movement commands work to extend visual selections."
          :difficulty 2
          :setup (lambda () (evil-visual-char) (insert "\nSelect this text using visual mode commands.\n")
                            (goto-char (point-max))
                            (search-backward "Select" nil t))}

         ;; 15. Press SPC for Leader Menu
         {:prompt "The SPACE bar holds many secrets in DOOM. Press 'SPC' to open the leader key menu."
          :goal-keys ("SPC")
          :success "Behold! The which-key menu reveals all the power of DOOM Emacs! Press ESC to close it for now."
          :hint "Simply press the spacebar while in normal mode."
          :explanation "The leader key (SPC) is how you access most of DOOM's commands."
          :difficulty 1
          :teardown (lambda () (message "Press ESC to dismiss which-key if it's still open."))}

         ;; 16. Find File via Leader
         {:prompt "Try opening the leader menu again with SPC, then press 'f' to see file commands, then press 'ESC' to cancel."
          :goal-keys ("SPC" "f" "ESC")
          :success "Explorer revealed! You've discovered the file navigation menu."
          :hint "Press SPC to open the leader menu, then 'f' for the file submenu."
          :explanation "The leader key menus are organized by category - 'f' is for file operations."
          :difficulty 2}

         ;; 17. Buffer Menu
         {:prompt "Access the buffer menu by pressing 'SPC' then 'b', then press 'ESC' to cancel."
          :goal-keys ("SPC" "b" "ESC")
          :success "Buffer mastery begins! You can manage your open files from here."
          :hint "Press SPC to open the leader menu, then 'b' for buffer operations."
          :explanation "Buffers are Emacs' way of managing open files and content."
          :difficulty 2}

         ;; 18. End of Phase 1
         {:prompt "You've grasped the basics, Apprentice! Phase 1 complete. You now understand modal editing, basic movement, and the leader key system."
          :goal-keys () ; No keys needed
          :success "You feel a surge of potential! You're ready to continue your journey."
          :explanation "You've completed the foundation of DOOM Emacs mastery."
          :difficulty 1}
          )))

;;----------------------------------------------------------------------------
;; Visualization & Feedback Functions
;;----------------------------------------------------------------------------
(defun doom-chronicles--display-progress-header ()
  "Display a visual progress bar and stats."
  (let* ((total-challenges (length doom-chronicles--challenges))
         (current (1+ doom-chronicles--current-challenge-index))
         (progress (/ (* 100.0 doom-chronicles--current-challenge-index) total-challenges))
         (bar-width 30)
         (completed (round (/ (* bar-width progress) 100.0)))
         (progress-bar (concat "["
                              (propertize (make-string completed ?█) 
                                         'face 'doom-chronicles-progress-bar-face)
                              (propertize (make-string (- bar-width completed) ?░)
                                         'face 'doom-chronicles-progress-empty-face)
                              "]")))
    (concat 
     (propertize (format " DOOM Chronicles - Phase %d " doom-chronicles--phase)
                'face 'doom-chronicles-header-face)
     "\n"
     (propertize (format " Progress: %s %.1f%% " progress-bar progress)
                'face 'doom-chronicles-stats-face)
     "  "
     (propertize (format "Challenge: %d/%d " current total-challenges)
                'face 'doom-chronicles-stats-face))))

(defun doom-chronicles--show-enhanced-feedback (message face &optional position duration)
  "Display rich feedback with animations and sound cues."
  (let ((pos (or position (point)))
        (dur (or duration 1.5)))
    ;; Display a message in the echo area
    (message "Chronicles: %s" message)
    
    ;; Create a visual feedback overlay
    (when face
      (let ((overlay (make-overlay (max (point-min) (1- pos)) pos)))
        (overlay-put overlay 'face face)
        (overlay-put overlay 'after-string 
                     (propertize (concat " → " message) 'face face))
        (overlay-put overlay 'doom-chronicles-feedback t)
        
        ;; Pulse effect if animations are enabled
        (when (and doom-chronicles-use-animations
                   (fboundp 'pulse-momentary-highlight-overlay))
          (pulse-momentary-highlight-overlay overlay face))
        
        ;; Remove overlay after duration
        (run-with-timer dur nil #'delete-overlay overlay)))))

(defun doom-chronicles--pulse-region (start end face)
  "Pulse the region from START to END with FACE."
  (when (and doom-chronicles-use-animations
             (fboundp 'pulse-momentary-highlight-region))
    (pulse-momentary-highlight-region start end face)))

(defun doom-chronicles--show-hint ()
  "Show a hint for the current challenge."
  (when-let* ((challenge (doom-chronicles--get-current-challenge))
              (hint (plist-get challenge :hint)))
    (doom-chronicles--show-enhanced-feedback 
     (format "Hint: %s" hint)
     'doom-chronicles-hint-face
     (point)
     3.0)))

;;----------------------------------------------------------------------------
;; Core Game Logic Functions
;;----------------------------------------------------------------------------
(defun doom-chronicles--get-current-challenge ()
  "Get the current challenge data structure."
  (nth doom-chronicles--current-challenge-index doom-chronicles--challenges))

(defun doom-chronicles--display-challenge ()
  "Display the current challenge prompt."
  (interactive)
  (when-let ((challenge (doom-chronicles--get-current-challenge)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Show progress header
      (insert (doom-chronicles--display-progress-header))
      (insert "\n\n")
      
      ;; Show challenge prompt
      (insert (propertize (plist-get challenge :prompt) 'face 'doom-chronicles-prompt-face))
      (insert "\n\n")
      
      ;; Show difficulty indicator
      (when-let ((difficulty (plist-get challenge :difficulty)))
        (insert (format "Difficulty: %s\n\n" 
                        (make-string difficulty ?★))))
      
      ;; Interface separator
      (insert "--- Start Typing Below ---\n")
      
      ;; Position cursor and prepare challenge
      (goto-char (point-max))
      (when-let ((setup-fn (plist-get challenge :setup)))
        (funcall setup-fn))
      
      ;; Reset challenge state
      (setq doom-chronicles--expected-keys (copy-sequence (plist-get challenge :goal-keys)))
      (setq doom-chronicles--keys-pressed '())
      (setq doom-chronicles--failed-attempts 0)
      (setq doom-chronicles--challenge-start-time (current-time)))))

(defun doom-chronicles--advance-challenge ()
  "Move to the next challenge."
  ;; Record completion time for the current challenge
  (when doom-chronicles--challenge-start-time
    (let* ((elapsed (float-time (time-subtract (current-time) 
                                              doom-chronicles--challenge-start-time)))
           (challenge (doom-chronicles--get-current-challenge))
           (stats (list :challenge doom-chronicles--current-challenge-index
                        :time-spent elapsed
                        :attempts (1+ doom-chronicles--failed-attempts))))
      (push stats doom-chronicles--statistics)
      (setq doom-chronicles--total-time (+ doom-chronicles--total-time elapsed))))
  
  ;; Run any teardown function for the current challenge
  (when-let ((challenge (doom-chronicles--get-current-challenge))
             (teardown-fn (plist-get challenge :teardown)))
    (funcall teardown-fn))
  
  ;; Move to the next challenge and save progress
  (setq doom-chronicles--current-challenge-index (1+ doom-chronicles--current-challenge-index))
  (doom-chronicles--save-progress)
  
  ;; Display the next challenge or completion message
  (if (< doom-chronicles--current-challenge-index (length doom-chronicles--challenges))
      (doom-chronicles--display-challenge)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (doom-chronicles--display-progress-header))
      (insert "\n\n")
      (insert (propertize "Congratulations! You have completed all available challenges in Phase 1!" 
                         'face 'doom-chronicles-success-face))
      (insert "\n\n")
      (insert "You've mastered the basics of DOOM Emacs! You now understand:\n\n")
      (insert "• Modal editing (Normal, Insert, Visual modes)\n")
      (insert "• Basic movement (hjkl, word movement, line navigation)\n")
      (insert "• Text manipulation (delete, undo)\n")
      (insert "• The Leader key system (SPC menus)\n\n")
      (insert "Total time spent: " (format-seconds "%h:%m:%s" doom-chronicles--total-time))
      (insert "\n\n")
      (insert "Press 'q' to exit, or check out Phase 2 challenges when they become available!"))))

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
          (doom-chronicles--show-enhanced-feedback "No challenge active or challenge completed." 
                                                'doom-chronicles-error-face (point))
          (call-interactively (key-binding (this-command-keys)))) ; Try to execute normally if no challenge
      (let ((expected (car doom-chronicles--expected-keys)))
        (if (string= key-str expected)
            ;; Correct key pressed
            (progn
              (setq doom-chronicles--keys-pressed (cons key-str doom-chronicles--keys-pressed))
              (setq doom-chronicles--expected-keys (cdr doom-chronicles--expected-keys))
              (doom-chronicles--show-enhanced-feedback (format "Correct! '%s' pressed." key-str) 
                                                    'doom-chronicles-success-face (point))

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
                    (let ((challenge (doom-chronicles--get-current-challenge)))
                      (doom-chronicles--show-enhanced-feedback 
                       (plist-get challenge :success) 
                       'doom-chronicles-success-face (point) 2.0)
                      
                      ;; Show explanation if available
                      (when-let ((explanation (plist-get challenge :explanation)))
                        (run-with-timer 2.1 nil
                                        (lambda ()
                                          (doom-chronicles--show-enhanced-feedback
                                           (format "Note: %s" explanation)
                                           'doom-chronicles-prompt-face (point) 3.0)))))
                    
                    (run-with-timer 2.5 nil #'doom-chronicles--advance-challenge))
                  
                ;; More keys to press - show next expected
                (doom-chronicles--show-enhanced-feedback 
                 (format "Next: '%s'" (car doom-chronicles--expected-keys)) 
                 nil (point))))
          
          ;; Incorrect key pressed
          (progn
            (doom-chronicles--show-enhanced-feedback 
             (format "Incorrect. Expected '%s', but got '%s'." expected key-str) 
             'doom-chronicles-error-face (point))
            
            ;; Track failed attempts
            (setq doom-chronicles--failed-attempts (1+ doom-chronicles--failed-attempts))
            
            ;; Show hint after repeated failures if automatic hints are enabled
            (when (and doom-chronicles-show-hints-automatically
                       (= (mod doom-chronicles--failed-attempts doom-chronicles-hint-delay) 0))
              (run-with-timer 1.0 nil #'doom-chronicles--show-hint))))))))

;;----------------------------------------------------------------------------
;; Persistence Functions
;;----------------------------------------------------------------------------
(defun doom-chronicles--save-progress ()
  "Save the current challenge index and statistics."
  (condition-case err
      (with-temp-file doom-chronicles-progress-file
        (print `(:current-challenge-index ,doom-chronicles--current-challenge-index
                 :phase ,doom-chronicles--phase
                 :total-time ,doom-chronicles--total-time
                 :statistics ,doom-chronicles--statistics
                 :last-update ,(format-time-string "%Y-%m-%d %H:%M:%S"))
               (current-buffer)))
    (error (message "Chronicles Error saving progress: %s" (error-message-string err)))))

(defun doom-chronicles--load-progress ()
  "Load the last saved challenge index and statistics."
  (if (file-exists-p doom-chronicles-progress-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents doom-chronicles-progress-file)
            (let ((data (read (current-buffer))))
              ;; Return a plist of all saved data
              data))
        (error 
         (message "Chronicles Error loading progress: %s. Starting from scratch." 
                 (error-message-string err))
         ;; Return default values
         '(:current-challenge-index 0 :phase 1 :total-time 0 :statistics nil)))
    ;; No file exists yet
    '(:current-challenge-index 0 :phase 1 :total-time 0 :statistics nil)))

;;----------------------------------------------------------------------------
;; Mode Setup & Activation
;;----------------------------------------------------------------------------
(defun doom-chronicles--setup-buffer ()
  "Set up the buffer for the tutorial."
  (setq buffer-read-only nil) ; Make sure we can modify
  
  ;; Load progress from file
  (let ((progress-data (doom-chronicles--load-progress)))
    (setq doom-chronicles--current-challenge-index 
          (plist-get progress-data :current-challenge-index))
    (setq doom-chronicles--phase 
          (plist-get progress-data :phase))
    (setq doom-chronicles--total-time 
          (plist-get progress-data :total-time))
    (setq doom-chronicles--statistics 
          (plist-get progress-data :statistics)))
  
  ;; Set up the challenges for Phase 1
  (doom-chronicles--setup-phase1-challenges)
  
  ;; Display current challenge
  (doom-chronicles--display-challenge)
  
  ;; Make buffer read-only to prevent accidental edits
  (setq buffer-read-only t)
  
  ;; Enhance mode line for clarity
  (setq-local doom-chronicles--original-mode-line-format mode-line-format)
  (setq-local mode-line-format
              (cons '(:eval (propertize 
                             (if evil-state
                                 (format " [DOOM CHRONICLES:%s] " 
                                        (upcase (symbol-name evil-state)))
                               " [DOOM CHRONICLES] ")
                            'face 'doom-chronicles-mode-indicator-face))
                    mode-line-format)))

(defun doom-chronicles--cleanup-buffer ()
  "Restore buffer state when mode is turned off."
  (when doom-chronicles--original-mode-line-format
    (setq mode-line-format doom-chronicles--original-mode-line-format)))

;; Menu for common actions
(defvar doom-chronicles-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Add keybindings for common actions
    (define-key map (kbd "C-c d n") #'doom-chronicles-next-challenge)
    (define-key map (kbd "C-c d p") #'doom-chronicles-prev-challenge)
    (define-key map (kbd "C-c d r") #'doom-chronicles-retry-challenge)
    (define-key map (kbd "C-c d h") #'doom-chronicles-show-hint)
    (define-key map (kbd "C-c d q") #'doom-chronicles-quit)
    (define-key map (kbd "C-c d s") #'doom-chronicles-show-stats)
    ;; q for quitting when all challenges complete
    (define-key map (kbd "q") #'doom-chronicles-quit)
    map)
  "Keymap for Doom Chronicles mode.")

;; Define the minor mode with enhanced keymap
(define-minor-mode doom-chronicles-mode
  "Minor mode for the Doom Chronicles interactive tutorial."
  :init-value nil
  :lighter " Chronicles"
  :keymap doom-chronicles-mode-map
  (if doom-chronicles-mode
      (progn
        ;; Setup key overrides dynamically
        (let ((key-override-map (make-sparse-keymap)))
          ;; Override keys we want to teach/validate
          (mapc (lambda (key)
                  (define-key key-override-map (kbd key) #'doom-chronicles--validate-key))
                ;; Keys validated in Phase 1 challenges (expanded)
                '("j" "k" "h" "l" "i" "e" "b" "w" "x" "d" "u" "v" "SPC" 
                  "<escape>" "f" "0" "$"))
          ;; Add the key overrides with higher priority
          (set-keymap-parent key-override-map doom-chronicles-mode-map)
          (setq minor-mode-overriding-map-alist
                (cons (cons 'doom-chronicles-mode key-override-map)
                      minor-mode-overriding-map-alist)))
        
        ;; Ensure evil-mode is active locally
        (setq-local evil-local-mode t)
        (evil-normal-state)
        
        ;; Setup the buffer
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

(defun doom-chronicles-next-challenge ()
  "Advance to the next challenge."
  (interactive)
  (when (and doom-chronicles-mode 
             (< (1+ doom-chronicles--current-challenge-index)
                (length doom-chronicles--challenges)))
    (doom-chronicles--advance-challenge)))

(defun doom-chronicles-prev-challenge ()
  "Go to the previous challenge."
  (interactive)
  (when (and doom-chronicles-mode 
             (> doom-chronicles--current-challenge-index 0))
    (when-let ((challenge (doom-chronicles--get-current-challenge))
               (teardown-fn (plist-get challenge :teardown)))
      (funcall teardown-fn))
    (setq doom-chronicles--current-challenge-index 
          (1- doom-chronicles--current-challenge-index))
    (doom-chronicles--display-challenge)))

(defun doom-chronicles-retry-challenge ()
  "Restart the current challenge."
  (interactive)
  (when doom-chronicles-mode
    (doom-chronicles--display-challenge)))

(defun doom-chronicles-show-hint ()
  "Show a hint for the current challenge."
  (interactive)
  (when doom-chronicles-mode
    (doom-chronicles--show-hint)))

(defun doom-chronicles-quit ()
  "Exit Doom Chronicles mode."
  (interactive)
  (when doom-chronicles-mode
    (doom-chronicles--save-progress)
    (doom-chronicles-mode -1)
    (message "Doom Chronicles session ended. Your progress has been saved.")))

(defun doom-chronicles-show-stats ()
  "Display statistics about the current session."
  (interactive)
  (when doom-chronicles-mode
    (let ((stats-buffer (get-buffer-create "*Doom Chronicles Stats*")))
      (with-current-buffer stats-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (org-mode)
          (insert "#+TITLE: Doom Chronicles Statistics\n\n")
          
          ;; Overall progress
          (insert "* Overall Progress\n\n")
          (insert (format "- Current Phase: %d\n" doom-chronicles--phase))
          (insert (format "- Challenge: %d/%d\n" 
                         (1+ doom-chronicles--current-challenge-index)
                         (length doom-chronicles--challenges)))
          (insert (format "- Total Time: %s\n\n" 
                         (format-seconds "%h:%m:%s" doom-chronicles--total-time)))
          
          ;; Challenge statistics
          (insert "* Challenge Statistics\n\n")
          (insert "| Challenge | Time Spent | Attempts |\n")
          (insert "|-----------|------------|----------|\n")
          (dolist (stat (reverse doom-chronicles--statistics))
            (insert (format "| %d | %s | %d |\n"
                           (1+ (plist-get stat :challenge))
                           (format-seconds "%m:%s" (plist-get stat :time-spent))
                           (plist-get stat :attempts))))
          
          ;; Set buffer to read-only
          (setq buffer-read-only t)))
      
      ;; Display the stats buffer
      (pop-to-buffer stats-buffer))))

(defun doom-chronicles-help ()
  "Show help for the current challenge and available commands."
  (interactive)
  (when doom-chronicles-mode
    (let ((help-buffer (get-buffer-create "*Doom Chronicles Help*")))
      (with-current-buffer help-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (org-mode)
          (insert "#+TITLE: Doom Chronicles Help\n\n")
          
          ;; Current challenge info
          (when-let ((challenge (doom-chronicles--get-current-challenge)))
            (insert "* Current Challenge\n\n")
            (insert (format "Goal: %s\n\n" (plist-get challenge :prompt)))
            (insert (format "Expected Keys: %s\n\n" 
                           (mapconcat #'identity doom-chronicles--expected-keys " ")))
            (when-let ((hint (plist-get challenge :hint)))
              (insert (format "Hint: %s\n\n" hint))))
          
          ;; Available commands
          (insert "* Available Commands\n\n")
          (insert "| Keybinding | Description |\n")
          (insert "|------------|--------------|\n")
          (insert "| C-c d n | Next challenge |\n")
          (insert "| C-c d p | Previous challenge |\n")
          (insert "| C-c d r | Retry current challenge |\n")
          (insert "| C-c d h | Show hint |\n")
          (insert "| C-c d q | Quit tutorial |\n")
          (insert "| C-c d s | Show statistics |\n")
          
          ;; Set buffer to read-only
          (setq buffer-read-only t)))
      
      ;; Display the help buffer
      (pop-to-buffer help-buffer))))

;;----------------------------------------------------------------------------
;; Ensure Feature Dependencies
;;----------------------------------------------------------------------------
(defun doom-chronicles--ensure-deps ()
  "Ensure dependencies are available."
  (unless (featurep 'evil)
    (user-error "Doom Chronicles requires Evil mode to be installed"))
  (unless (featurep 'which-key)
    (user-error "Doom Chronicles requires which-key to be installed")))

;; Check dependencies when the package is loaded
(doom-chronicles--ensure-deps)

;;----------------------------------------------------------------------------
;; Provide Feature
;;----------------------------------------------------------------------------
(provide 'doom-chronicles)

;;; doom-chronicles.el ends here
