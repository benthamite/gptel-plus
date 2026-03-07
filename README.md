# `gptel-plus`: Enhancements for gptel

## Overview

`gptel-plus` extends [gptel](https://github.com/karthink/gptel), the popular Emacs package for interfacing with large language models. It adds cost awareness, context persistence, and quality-of-life improvements that are useful when working with paid LLM APIs on a regular basis.

If you use gptel daily and want to keep track of how much each request costs, persist your file context across sessions, or manage a large context without losing sight of your spending, `gptel-plus` fills those gaps. It works with Anthropic, OpenAI, and Gemini backends, handles prompt caching for the providers that support it, and integrates transparently into gptel's existing header line and workflow.

The package provides four groups of functionality:

- **Cost estimation and tracking.** Before sending a request, `gptel-plus` estimates its cost and displays it in the header line. After a request completes, it parses the response log to report the exact cost. When a request exceeds a configurable threshold, it asks for confirmation before proceeding.

- **Context persistence.** The gptel context (files added via `gptel-context-add-file`) is ephemeral by default. `gptel-plus` can save and restore context to and from the file itself, as an Org property or a Markdown file-local variable.

- **Context file management.** A dedicated buffer lists all context files sorted by size, with a dired-like interface for flagging and removing files in bulk -- helpful when you need to trim a large context to reduce costs.

- **Automatic mode activation.** `gptel-plus` can automatically enable `gptel-mode` when you open a file that contains gptel data, so you can resume a conversation without manually toggling the mode.

## Installation

`gptel-plus` requires Emacs 29.1 or later and [gptel](https://github.com/karthink/gptel) 0.7.1 or later.

**package-vc** (built-in since Emacs 29):

```emacs-lisp
(package-vc-install "https://github.com/benthamite/gptel-plus")
```

**Elpaca**:

```emacs-lisp
(use-package gptel-plus
  :ensure (gptel-plus :host github :repo "benthamite/gptel-plus"))
```

**straight.el**:

```emacs-lisp
(straight-use-package
 '(gptel-plus :type git :host github :repo "benthamite/gptel-plus"))
```

## Quick start

Once installed, `gptel-plus` activates automatically: it replaces the gptel header line with an extended version that includes a clickable cost indicator, and it hooks into `gptel-send` to warn you when a request is expensive.

A minimal configuration that also enables automatic mode activation:

```emacs-lisp
(use-package gptel-plus
  :ensure (gptel-plus :host github :repo "benthamite/gptel-plus")
  :hook
  (org-mode . gptel-plus-enable-gptel-in-org)
  (markdown-mode . gptel-plus-enable-gptel-in-markdown))
```

Open a gptel buffer, add some context files, and watch the `[Cost: $X.XX]` indicator in the header line update as you type. After sending a request, the exact cost appears in the echo area.

## Documentation

For a comprehensive description of all user options, commands, and functions, see the [manual](README.org).

## Roadmap

- [ ] Session cost accumulation -- track cumulative spend with a running total in the header line
- [ ] Region-based context in cost estimation -- include regions added via `gptel-context--add-region` in the ex ante estimate
- [ ] Restricted and branching conversation awareness -- scope cost estimates to the portion of the buffer actually sent
- [ ] Auto-restore context on mode activation -- restore saved context automatically when `gptel-mode` is enabled via hooks
- [ ] Show actual cost in header line -- display the ex post cost alongside the estimate after a request completes

## License

`gptel-plus` is licensed under the GNU General Public License v3. See [COPYING.txt](COPYING.txt) for details.
