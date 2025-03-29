# doom-emacs-tutorial

# Doom Chronicles - Phase 1: The Spark ✨

**An interactive tutorial game to master DOOM Emacs on NixOS, built on cognitive science principles.**

Progressive difficulty 0 to hero interactive game within Doom Emacs

[![Status](https://img.shields.io/badge/Status-Phase%201%20Alpha-orange)](https://github.com/your-username/doom-chronicles) <!-- Replace your-username -->
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

## Vision

Doom Chronicles aims to be the definitive, most engaging, and effective interactive learning experience for mastering DOOM Emacs, particularly within a NixOS environment. Forget dry manuals; learn by *doing* in an adventure designed to make learning addictive, leveraging principles like dopamine rewards, spaced repetition, flow state, and active recall.

## 🔥 Current Status: Phase 1 - The Spark (Early Alpha) 🔥

This repository currently contains **Phase 1** of Doom Chronicles. It is a **minimal viable product** focused *only* on the absolute core mechanics to get you started and address the most common initial frustrations.

**What Phase 1 Covers:**

*   ✅ **Modal Mastery:** Confidently switching between Normal, Insert, and Visual modes. Clear mode indication!
*   ✅ **Fundamental Movement:** `h j k l` navigation.
*   ✅ **Basic Editing:** `x`, `d`, `c`, `y`, `p`, `u`.
*   ✅ **The Leader Key:** Introducing `SPC` and the `which-key` menu.
*   ✅ **Core Feedback Loop:** Instant validation and clear feedback for your actions.
*   ❌ **NOT YET IMPLEMENTED:** Window/buffer management, file finding, Magit, NixOS config, Org mode, Spaced Repetition System (SRS), full gamification, etc. (These are planned for future phases!)

## Why This Approach?

Learning Emacs, especially DOOM Emacs, has a notoriously steep initial learning curve. Key frustrations include modal confusion, keybinding overload, and discovering essential commands. Doom Chronicles tackles these head-on through:

*   **Interactive Challenges:** Learn by *doing*, not just reading.
*   **Immediate Feedback:** Understand instantly if you did something right or wrong.
*   **Targeted Practice:** Focus on the most crucial initial bindings and concepts.
*   **Gamified Motivation:** Making practice feel less like a chore and more like progress.

## Installation (Requires DOOM Emacs)

1.  **Clone the Repository:**
    Clone this repository into your DOOM Emacs private modules directory. The standard location is `~/.doom.d/modules/private/` or `~/.config/doom/modules/private/`.

    ```bash
    # Example using the default ~/.doom.d location
    cd ~/.doom.d/modules/private/
    git clone https://github.com/your-username/doom-chronicles.git learning-game # Or choose your preferred dir name
    ```
    *(Replace `your-username` with your actual GitHub username)*

2.  **Enable the Module in `config.el`:**
    Add the following `use-package!` block to your `~/.doom.d/config.el` (or `~/.config/doom/config.el`):

    ```elisp
    ;; In ~/.doom.d/config.el or ~/.config/doom/config.el

    (use-package! doom-chronicles
      ;; If you cloned into a different subdirectory name, update the path:
      :load-path "~/.doom.d/modules/private/learning-game/" 
      :commands (doom-chronicles-start))
    ```
    *(Adjust `:load-path` if you named the cloned directory differently or used `~/.config/doom`)*

3.  **Sync DOOM:**
    Run `doom sync` from your command line:
    ```bash
    # If using default ~/.emacs.d location for DOOM
    ~/.emacs.d/bin/doom sync

    # If using ~/.config/emacs location for DOOM
    ~/.config/emacs/bin/doom sync
    ```

4.  **Restart Emacs:**
    Close and reopen Emacs completely.

## Usage

1.  Start Emacs.
2.  Press `SPC :` (or `M-x`).
3.  Type `doom-chronicles-start` and press `Enter`.
4.  A new buffer named `*Doom Chronicles*` will appear, presenting your first challenge!
5.  Follow the prompts. Pay attention to the mode line indicator and feedback messages.

## Features (Phase 1)

*   **Interactive Minor Mode (`doom-chronicles-mode`):** A dedicated environment for learning.
*   **Real-time Command Validation:** Checks if you pressed the key(s) required by the current challenge.
*   **Clear Feedback:** Visual (overlays) and textual (messages) feedback for correct/incorrect actions.
*   **Prominent Mode Indicator:** Helps combat modal confusion.
*   **Simple Challenge Progression:** Linear sequence of basic tasks.
*   **Minimal Persistence:** Remembers which challenge you were on (`doom-chronicles-progress.el`).

## Roadmap (Future Phases)

*   **Phase 2: Expanding Horizons:** Buffers, windows, files, basic Org mode, progress visualization, SRS data capture.
*   **Phase 3: Core Tooling & Config:** Magit basics, NixOS/DOOM config intro, active SRS reviews.
*   **Phase 4: Mastery & Ecosystem:** Advanced topics (Org-roam, Org-babel, Nix dev env), polish, full gamification (achievements, skill trees).

## Debugging

If you encounter issues:

1.  Check the `*Messages*` buffer (`SPC b b *Messages* RET`) for errors.
2.  Ensure you ran `doom sync` and restarted Emacs after installation/updates.
3.  Try `M-x toggle-debug-on-error` before running the game to get a backtrace if an error occurs.
4.  Feel free to open an issue on GitHub!

## Contributing

Contributions are very welcome, but please note this is in **early alpha**. Wait for Phase 2 or 3 for more structured contribution opportunities, or open an issue first to discuss ideas for Phase 1 improvements.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

*   Inspired by the need for more engaging ways to learn powerful tools.
*   Built upon the amazing foundation of Emacs and DOOM Emacs.
*   (Add other inspirations or contributors here later)
