;; In ~/.doom.d/config.el or ~/.config/doom/config.el

;; If you placed doom-chronicles.el directly in ~/.doom.d/
;; (add-to-list 'load-path "~/.doom.d/")
;; (require 'doom-chronicles)

;; If you placed it in ~/.doom.d/modules/private/learning-game/
;; Add this near the top with other use-package! declarations
(use-package! doom-chronicles
  :load-path "~/.doom.d/modules/private/learning-game/" ; Adjust if needed
  :commands (doom-chronicles-start))

;; Ensure load-path is correct. If using the private module structure,
;; DOOM might handle the load-path automatically after a 'doom sync'.
;; If not, uncomment and adjust the (add-to-list ...) line above the require/use-package.
