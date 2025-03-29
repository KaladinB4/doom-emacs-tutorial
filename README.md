# Doom Chronicles

![Status](https://img.shields.io/badge/Status-Phase%201-green)
![License](https://img.shields.io/badge/License-MIT-blue.svg)

**An interactive tutorial game for mastering DOOM Emacs, built on cognitive science principles.**

## Vision

Doom Chronicles provides the most engaging and effective interactive learning experience for mastering DOOM Emacs. Rather than reading dry manuals, you'll *learn by doing* in a carefully designed adventure that makes learning feel natural and addictive, leveraging principles of flow state, spaced repetition, and immediate feedback.

## Learning Path

```
                    ┌─────────────────┐
                    │   Phase 1       │
                    │   The Spark     │ 
                    │                 │
                    │ • Modal editing │
                    │ • Basic movement│
                    │ • Leader key    │
                    └────────┬────────┘
                             │
                             ▼
┌─────────────────┐   ┌─────────────────┐   ┌─────────────────┐
│    Phase 2      │   │    Phase 3      │   │    Phase 4      │
│  Buffer Mastery │   │  Text Mastery   │   │Knowledge Systems│
│                 │◄─►│                 │◄─►│                 │
│• Buffer/windows │   │• Text objects   │   │• Org mode       │
│• File navigation│   │• Registers      │   │• Notes & capture│
│• Projects       │   │• Macros         │   │• Tags & links   │
└────────┬────────┘   └────────┬────────┘   └────────┬────────┘
         │                     │                     │
         │                     ▼                     │
         │            ┌─────────────────┐            │
         └────────────►   Phase 5      │◄────────────┘
                      │ Dev Environment │
                      │                 │
                      │• Magit          │
                      │• Completion     │
                      │• Projects       │
                      └─────────────────┘
```

## Current Status: Phase 1 - The Spark 🔥

This release contains **Phase 1** of Doom Chronicles, focused on building core mechanics and foundations:

**What Phase 1 Teaches:**

* ✅ **Modal Mastery:** Confidently switching between Normal, Insert, and Visual modes
* ✅ **Movement Fundamentals:** From basic `hjkl` to efficient word-based and line navigation
* ✅ **Text Manipulation:** Quick edits with `x`, `d`, `dd`, and `u`
* ✅ **Leader Key Introduction:** Understanding the DOOM command system via `SPC`
* ✅ **Mental Model Building:** Developing a coherent understanding of how DOOM operates

**Learning Features:**

* ✅ **Progressive Challenges:** 18 carefully sequenced challenges with increasing complexity
* ✅ **Immediate Feedback:** Visual cues and detailed messages for correct/incorrect actions
* ✅ **Hint System:** Optional hints when you're stuck, with contextual explanations
* ✅ **Progress Tracking:** Visual progress bar and challenge completion statistics
* ✅ **Performance Analytics:** Track your improvement and identify areas for practice

## Installation

1. **Clone the Repository:**
   ```bash
   # Using the default ~/.doom.d location
   cd ~/.doom.d/modules/private/
   git clone https://github.com/kaladinb4/doom-chronicles.git learning-game
   ```

2. **Enable in your `config.el`:**
   ```elisp
   ;; In ~/.doom.d/config.el or ~/.config/doom/config.el
   (use-package! doom-chronicles
     :load-path "~/.doom.d/modules/private/learning-game/"
     :commands (doom-chronicles-start))
   ```

3. **Sync DOOM:**
   ```bash
   ~/.emacs.d/bin/doom sync
   ```

4. **Restart Emacs**

## Usage

1. Press `SPC :` (or `M-x`) and type `doom-chronicles-start`
2. Follow the on-screen instructions for each challenge
3. Use `C-c d h` to show hints when you're stuck
4. Use `C-c d s` to view your progress statistics

### Keyboard Shortcuts

While in Doom Chronicles mode:
- `C-c d n` - Next challenge
- `C-c d p` - Previous challenge
- `C-c d r` - Retry current challenge
- `C-c d h` - Show hint
- `C-c d q` - Quit tutorial
- `C-c d s` - Show statistics

## Roadmap

### Phase 2: Buffer Mastery
- Buffer management (`SPC b` commands)
- Window manipulation (`SPC w` commands)
- Project navigation (`SPC p` commands)
- File operations (`SPC f` commands)

### Phase 3: Text Mastery
- Advanced movement with text objects
- Enhanced selection techniques
- Registers and marks
- Macro recording and playback

### Phase 4: Knowledge Systems
- Org-mode fundamentals
- Note-taking workflows
- Task management
- Knowledge organization

### Phase 5: Development Environment
- Version control with Magit
- Completion systems
- Project management
- Language-specific workflows

## Customization

You can customize Doom Chronicles through the Emacs customization interface:
```elisp
M-x customize-group RET doom-chronicles RET
```

Available options include:
- `doom-chronicles-use-animations` - Enable/disable animated feedback
- `doom-chronicles-show-hints-automatically` - Show hints after repeated failures
- `doom-chronicles-hint-delay` - Number of attempts before showing automatic hints

## Debugging

If you encounter issues:

1. Check the `*Messages*` buffer for errors
2. Ensure you ran `doom sync` after installation
3. Try `M-x toggle-debug-on-error` before running the tutorial
4. Make sure all required packages are installed (evil, which-key)

## Contributing

Contributions are welcome! Before submitting pull requests:

1. Ensure your code follows the existing style
2. Add comments for non-trivial code
3. Update documentation as needed
4. Test your changes thoroughly

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Built on the excellent foundation of DOOM Emacs
- Inspired by gamification and cognitive science research
- Thanks to the DOOM Emacs community for feedback and suggestions
