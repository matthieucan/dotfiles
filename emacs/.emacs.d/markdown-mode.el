;;; markdown-mode.el --- Emacs Major mode for Markdown-formatted text files

;; Copyright (C) 2007-2013 Jason R. Blevins <jrblevin@sdf.org>
;; Copyright (C) 2007, 2009 Edward O'Connor <ted@oconnor.cx>
;; Copyright (C) 2007 Conal Elliott <conal@conal.net>
;; Copyright (C) 2008 Greg Bognar <greg_bognar@hms.harvard.edu>
;; Copyright (C) 2008 Dmitry Dzhus <mail@sphinx.net.ru>
;; Copyright (C) 2008 Bryan Kyle <bryan.kyle@gmail.com>
;; Copyright (C) 2008 Ben Voui <intrigeri@boum.org>
;; Copyright (C) 2009 Ankit Solanki <ankit.solanki@gmail.com>
;; Copyright (C) 2009 Hilko Bengen <bengen@debian.org>
;; Copyright (C) 2009 Peter Williams <pezra@barelyenough.org>
;; Copyright (C) 2010 George Ogata <george.ogata@gmail.com>
;; Copyright (C) 2011 Eric Merritt <ericbmerritt@gmail.com>
;; Copyright (C) 2011 Philippe Ivaldi <pivaldi@sfr.fr>
;; Copyright (C) 2011 Jeremiah Dodds <jeremiah.dodds@gmail.com>
;; Copyright (C) 2011 Christopher J. Madsen <cjm@cjmweb.net>
;; Copyright (C) 2011 Shigeru Fukaya <shigeru.fukaya@gmail.com>
;; Copyright (C) 2011 Joost Kremers <joostkremers@fastmail.fm>
;; Copyright (C) 2011-2012 Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Copyright (C) 2012 Akinori Musha <knu@idaemons.org>
;; Copyright (C) 2012 Zhenlei Jia <zhenlei.jia@gmail.com>
;; Copyright (C) 2012 Peter Jones <pjones@pmade.com>
;; Copyright (C) 2013 Matus Goljer <dota.keys@gmail.com>

;; Author: Jason R. Blevins <jrblevin@sdf.org>
;; Maintainer: Jason R. Blevins <jrblevin@sdf.org>
;; Created: May 24, 2007
;; Version: 1.9
;; Keywords: Markdown, GitHub Flavored Markdown, itex
;; URL: http://jblevins.org/projects/markdown-mode/

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; markdown-mode is a major mode for editing [Markdown][]-formatted
;; text files in GNU Emacs.  markdown-mode is free software, licensed
;; under the GNU GPL.
;;
;;  [Markdown]: http://daringfireball.net/projects/markdown/
;;
;; The latest stable version is markdown-mode 1.9, released on January 25, 2013:
;;
;;    * [markdown-mode.el][]
;;    * [Screenshot][][^theme]
;;    * [Release notes][]
;;
;;  [markdown-mode.el]: http://jblevins.org/projects/markdown-mode/markdown-mode.el
;;  [screenshot]: http://jblevins.org/projects/markdown-mode/screenshots/20110812-001.png
;;  [release notes]: http://jblevins.org/projects/markdown-mode/rev-1-9
;;
;; [^theme]: The theme used in the screenshot is
;;   [color-theme-twilight](https://github.com/crafterm/twilight-emacs).
;;
;; markdown-mode is also available in several package managers, including:
;;
;;    * Debian and Ubuntu Linux: [emacs-goodies-el][]
;;    * RedHat and Fedora Linux: [emacs-goodies][]
;;    * OpenBSD: [textproc/markdown-mode][]
;;    * Arch Linux (AUR): [emacs-markdown-mode-git][]
;;    * MacPorts: [markdown-mode.el][macports-package] ([pending][macports-ticket])
;;    * FreeBSD: [textproc/markdown-mode.el][freebsd-port]
;;
;;  [emacs-goodies-el]: http://packages.debian.org/emacs-goodies-el
;;  [emacs-goodies]: https://admin.fedoraproject.org/pkgdb/acls/name/emacs-goodies
;;  [textproc/markdown-mode]: http://pkgsrc.se/textproc/markdown-mode
;;  [emacs-markdown-mode-git]: http://aur.archlinux.org/packages.php?ID=30389
;;  [macports-package]: https://trac.macports.org/browser/trunk/dports/editors/markdown-mode.el/Portfile
;;  [macports-ticket]: http://trac.macports.org/ticket/35716
;;  [freebsd-port]: http://svnweb.freebsd.org/ports/head/textproc/markdown-mode.el
;;
;; The latest development version can be downloaded directly
;; ([markdown-mode.el][devel.el]) or it can be obtained from the
;; (browsable and clonable) Git repository at
;; <http://jblevins.org/git/markdown-mode.git>.  The entire repository,
;; including the full project history, can be cloned via the Git protocol
;; by running
;;
;;     git clone git://jblevins.org/git/markdown-mode.git
;;
;;  [devel.el]: http://jblevins.org/git/markdown-mode.git/plain/markdown-mode.el

;;; Dependencies:

;; markdown-mode requires easymenu, a standard package since GNU Emacs
;; 19 and XEmacs 19, which provides a uniform interface for creating
;; menus in GNU Emacs and XEmacs.

;;; Installation:

;; Make sure to place `markdown-mode.el` somewhere in the load-path and add
;; the following lines to your `.emacs` file to associate markdown-mode
;; with `.text` files:
;;
;;     (autoload 'markdown-mode "markdown-mode"
;;        "Major mode for editing Markdown files" t)
;;     (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;;
;; There is no consensus on an official file extension so change `.text` to
;; `.mdwn`, `.md`, `.mdt`, or whatever you call your markdown files.

;;; Customization:

;; Although no configuration is *necessary* there are a few things
;; that can be customized.  The `M-x customize-mode` command
;; provides an interface to all of the possible customizations:
;;
;;   * `markdown-command' - the command used to run Markdown (default:
;;     `markdown').  This variable may be customized to pass
;;     command-line options to your Markdown processor of choice, but
;;     this command must accept input from `stdin`.  If it does not, a
;;     simple wrapper script can be used to write `stdin` to a file
;;     and then pass that file to your Markdown interpreter.  Ideally,
;;     this command will produce an XHTML fragment around which
;;     markdown-mode will wrap a header and footer (which can be
;;     further customized).  However, it attempts to detect whether
;;     the command produces standalone XHTML output (via
;;     `markdown-xhtml-standalone-regexp'), in which case no header
;;     and footer content will be added.
;;
;;   * `markdown-command-needs-filename' - set to non-nil if
;;     `markdown-command' does not accept input from stdin (default: nil).
;;      Instead, it will be passed a filename as the final command-line
;;      option.  As a result, you will only be able to run Markdown
;;      from buffers which are visiting a file.
;;
;;   * `markdown-open-command' - the command used for calling a standalone
;;     Markdown previewer which is capable of opening Markdown source files
;;     directly (default: `nil').  This command will be called
;;     with a single argument, the filename of the current buffer.
;;     A representative program is the Mac app [Marked][], a
;;     live-updating MultiMarkdown previewer which has a command line
;;     utility at `/usr/local/bin/mark`.
;;
;;   * `markdown-hr-string' - string to use when inserting horizontal
;;     rules (default: `* * * * *').
;;
;;   * `markdown-bold-underscore' - set to a non-nil value to use two
;;     underscores for bold instead of two asterisks (default: `nil').
;;
;;   * `markdown-italic-underscore' - set to a non-nil value to use
;;     underscores for italic instead of asterisks (default: `nil').
;;
;;   * `markdown-indent-function' - the function to use for automatic
;;     indentation (default: `markdown-indent-line').
;;
;;   * `markdown-indent-on-enter' - set to a non-nil value to
;;     automatically indent new lines when the enter key is pressed
;;     (default: `t')
;;
;;   * `markdown-follow-wiki-link-on-enter' - set to a non-nil value
;;     to automatically open a linked document in a new buffer if the
;;     cursor is an wiki link
;;     (default: `t')
;;
;;   * `markdown-wiki-link-alias-first' - set to a non-nil value to
;;     treat aliased wiki links like `[[link text|PageName]]`.
;;     When set to nil, they will be treated as `[[PageName|link text]]'.
;;
;;   * `markdown-uri-types' - a list of protocols for URIs that
;;     `markdown-mode' should highlight.
;;
;;   * `markdown-enable-math' - syntax highlighting for
;;     LaTeX fragments (default: `nil').
;;
;;   * `markdown-css-path' - CSS file to link to in XHTML output.
;;
;;   * `markdown-content-type' - when set to a nonempty string, an
;;     `http-equiv` attribute will be included in the XHTML `<head>`
;;     block.  If needed, the suggested values are
;;     `application/xhtml+xml` or `text/html`.
;;
;;   * `markdown-coding-system' - used for specifying the character
;;     set identifier in the `http-equiv` attribute (see
;;     `markdown-content-type').  When set to `nil',
;;     `buffer-file-coding-system' will be used (and falling back to
;;     `iso-8859-1' when unavailable).  Common settings are `utf-8'
;;     and `iso-latin-1'.
;;
;;   * `markdown-xhtml-header-content' - additional content to include
;;     in the XHTML `<head>` block.
;;
;;   * `markdown-xhtml-standalone-regexp' - a regular expression which
;;     indicates whether the output of `markdown-command' is standalone
;;     XHTML (default: `^\\(\<\?xml\\|\<!DOCTYPE\\|\<html\\)`).  If
;;     this is not matched, we assume this output is a fragment and add
;;     our own header and footer.
;;
;;   * `markdown-link-space-sub-char' - a character to replace spaces
;;     when mapping wiki links to filenames (default: `_`).
;;     For example, use an underscore for compatibility with the
;;     Python Markdown WikiLinks extension or a hyphen for compatibility
;;     with Github wiki links.
;;
;; Additionally, the faces used for syntax highlighting can be modified to
;; your liking by issuing `M-x customize-group RET markdown-faces`
;; or by using the "Markdown Faces" link at the bottom of the mode
;; customization screen.
;;
;; [Marked]: https://itunes.apple.com/us/app/marked/id448925439?ls=1&mt=12&partnerId=30&siteID=GpHp3Acs1Yo

;;; Usage:

;; Keybindings are grouped by prefixes based on their function.  For
;; example, commands dealing with headers begin with `C-c C-t`.  The
;; primary commands in each group will are described below.  You can
;; obtain a list of all keybindings by pressing `C-c C-h`.
;;
;;   * Anchors: `C-c C-a`
;;
;;     `C-c C-a l` inserts inline links of the form `[text](url)`.
;;     `C-c C-a r` inserts reference links of the form `[text][label]`.
;;     The label definition will be placed at the end of the current
;;     block. `C-c C-a w` acts similarly for wiki links of the form
;;     `[[WikiLink]]`. In all cases, if there is an active region, the
;;     text in the region is used as the link text.
;;
;;   * Commands: `C-c C-c`
;;
;;     *Compile:* `C-c C-c m` will run Markdown on the current buffer
;;     and show the output in another buffer.  *Preview*: `C-c C-c p`
;;     runs Markdown on the current buffer and previews, stores the
;;     output in a temporary file, and displays the file in a browser.
;;     *Export:* `C-c C-c e` will run Markdown on the current buffer
;;     and save the result in the file `basename.html`, where
;;     `basename` is the name of the Markdown file with the extension
;;     removed.  *Export and View:* press `C-c C-c v` to export the
;;     file and view it in a browser.  **For both export commands, the
;;     output file will be overwritten without notice.**
;;     *Open:* `C-c C-c o` will open the Markdown source file directly
;;     using `markdown-open-command'.
;;
;;     To summarize:
;;
;;       - `C-c C-c m`: `markdown-command' > `*markdown-output*` buffer.
;;       - `C-c C-c p`: `markdown-command' > temporary file > browser.
;;       - `C-c C-c e`: `markdown-command' > `basename.html`.
;;       - `C-c C-c v`: `markdown-command' > `basename.html` > browser.
;;       - `C-c C-c w`: `markdown-command' > kill ring.
;;       - `C-c C-c o`: `markdown-open-command'.
;;
;;     `C-c C-c c` will check for undefined references.  If there are
;;     any, a small buffer will open with a list of undefined
;;     references and the line numbers on which they appear.  In Emacs
;;     22 and greater, selecting a reference from this list and
;;     pressing `RET` will insert an empty reference definition at the
;;     end of the buffer.  Similarly, selecting the line number will
;;     jump to the corresponding line.
;;
;;     `C-c C-c n` will clean up the numbering of ordered lists.

;;   * Images: `C-c C-i`
;;
;;     `C-c C-i i` inserts an image, using the active region (if any)
;;     as the alt text.
;;
;;   * Physical styles: `C-c C-p`
;;
;;     These commands all act on text in the active region, if any,
;;     and insert empty markup fragments otherwise.  `C-c C-p b` makes
;;     the selected text bold, `C-c C-p f` formats the region as
;;     fixed-width text, and `C-c C-p i` is used for italic text.
;;
;;   * Logical styles: `C-c C-s`
;;
;;     These commands all act on text in the active region, if any,
;;     and insert empty markup fragments otherwise.  Logical styles
;;     include blockquote (`C-c C-s b`), preformatted (`C-c C-s p`),
;;     code (`C-c C-s c`), emphasis (`C-c C-s e`), and strong
;;     (`C-c C-s s`).
;;
;;   * Headers: `C-c C-t`
;;
;;     All header commands use text in the active region, if any, as
;;     the header text.  Otherwise, if the current line is not blank,
;;     use the text on the current line.  Finally, prompt for header
;;     text if there is no active region and the current line is
;;     blank.  To insert an atx or hash style level-n
;;     header, press `C-c C-t n` where n is between 1 and 6.  For a
;;     top-level setext or underline style header press `C-c C-t t`
;;     (mnemonic: title) and for a second-level underline-style header
;;     press `C-c C-t s` (mnemonic: section).
;;
;;     If the point is at a header, these commands will replace the
;;     existing markup in order to update the level and/or type of the
;;     header.  To remove the markup of the header at the point, press
;;     `C-c C-t 0`.
;;
;;   * Footnotes: `C-c C-f`
;;
;;     To create a new footnote at the point, press `C-c C-f n`.
;;     Press `C-c C-f g` with the point at a footnote to jump to the
;;     location where the footnote text is defined.  Then, press
;;     `C-c C-f b` to return to the footnote marker in the main text.
;;     When the point is at a footnote marker or in the body of a
;;     footnote, press `C-c C-f k` to kill the footnote and add the
;;     text to the kill ring.
;;
;;   * Other elements:
;;
;;     `C-c -` inserts a horizontal rule.
;;
;;   * Links:
;;
;;     Press `C-c C-o` when the point is on an inline or reference
;;     link to open the URL in a browser.
;;
;;   * Wiki-Link Navigation:
;;
;;     Use `M-p` and `M-n` to quickly jump to the previous and next
;;     wiki links, respectively.
;;
;;   * Outline Navigation:
;;
;;     Navigation between headings is possible using `outline-mode'.
;;     Use `C-M-n` and `C-M-p` to move between the next and previous
;;     visible headings.  Similarly, `C-M-f` and `C-M-b` move to the
;;     next and previous visible headings at the same level as the one
;;     at the point.  Finally, `C-M-u` will move up to a lower-level
;;     (more inclusive) visible heading.
;;
;; Many of the commands described above behave differently depending on
;; whether Transient Mark mode is enabled or not.  When it makes sense,
;; if Transient Mark mode is on and a region is active, the command
;; applies to the text in the region (e.g., `C-c C-p b` makes the region
;; bold).  For users who prefer to work outside of Transient Mark mode,
;; in Emacs 22 it can be enabled temporarily by pressing `C-SPC C-SPC`.
;;
;; When applicable, commands that specifically act on the region even
;; outside of Transient Mark mode have the same keybinding as the with
;; the exception of an additional `C-` prefix.  For example,
;; `markdown-insert-blockquote' is bound to `C-c C-s b` and only acts on
;; the region in Transient Mark mode while `markdown-blockquote-region'
;; is bound to `C-c C-s C-b` and always applies to the region (when
;; nonempty).
;;
;; markdown-mode attempts to be flexible in how it handles
;; indentation.  When you press `TAB` repeatedly, the point will cycle
;; through several possible indentation levels corresponding to things
;; you might have in mind when you press `RET` at the end of a line or
;; `TAB`.  For example, you may want to start a new list item,
;; continue a list item with hanging indentation, indent for a nested
;; pre block, and so on.
;;
;; markdown-mode supports outline-minor-mode as well as org-mode-style
;; visibility cycling for atx- or hash-style headers.  There are two
;; types of visibility cycling: Pressing `S-TAB` cycles globally between
;; the table of contents view (headers only), outline view (top-level
;; headers only), and the full document view.  Pressing `TAB` while the
;; point is at a header will cycle through levels of visibility for the
;; subtree: completely folded, visible children, and fully visible.
;; Note that mixing hash and underline style headers will give undesired
;; results.

;;; Extensions:

;; Besides supporting the basic Markdown syntax, markdown-mode also
;; includes syntax highlighting for `[[Wiki Links]]` by default. Wiki
;; links may be followed automatically by hitting the enter key when
;; your curser is on a wiki link or by hitting `C-c C-w`. The
;; autofollowing on enter key may be controlled with the
;; `markdown-follow-wiki-link-on-enter' customization.  Use `M-p` and
;; `M-n` to quickly jump to the previous and next wiki links,
;; respectively.  Aliased or piped wiki links of the form
;; `[[link text|PageName]]` are also supported.  Since some wikis
;; reverse these components, set `markdown-wiki-link-alias-first'
;; to nil to treat them as `[[PageName|link text]]`.
;;
;; [SmartyPants][] support is possible by customizing `markdown-command'.
;; If you install `SmartyPants.pl` at, say, `/usr/local/bin/smartypants`,
;; then you can set `markdown-command' to `"markdown | smartypants"`.
;; You can do this either by using `M-x customize-group markdown`
;; or by placing the following in your `.emacs` file:
;;
;;     (defun markdown-custom ()
;;       "markdown-mode-hook"
;;       (setq markdown-command "markdown | smartypants"))
;;     (add-hook 'markdown-mode-hook '(lambda() (markdown-custom)))
;;
;; [SmartyPants]: http://daringfireball.net/projects/smartypants/
;;
;; Experimental syntax highlighting for mathematical expressions written
;; in LaTeX (only expressions denoted by `$..$`, `$$..$$`, or `\[..\]`)
;; can be enabled by setting `markdown-enable-math' to a non-nil value,
;; either via customize or by placing `(setq markdown-enable-itex t)`
;; in `.emacs`, and restarting Emacs.
;;
;; A [GitHub Flavored Markdown](http://github.github.com/github-flavored-markdown/)
;; mode, `gfm-mode', is also available.  The GitHub implementation of
;; differs slightly from standard Markdown.  Most importantly, newlines are
;; significant and trigger hard line breaks.  As such, `gfm-mode' turns off
;; `auto-fill-mode' and turns on `visual-line-mode' (or `longlines-mode' if
;; `visual-line-mode' is not available).  Wiki links in this mode will be
;; treated as on GitHub, with hyphens replacing spaces in filenames and
;; where the first letter of the filename capitalized.  For example,
;; `[[wiki link]]' will map to a file named `Wiki-link` with the same
;; extension as the current file.  GFM code blocks, with optional
;; programming language keywords, will be highlighted.  They can be inserted
;;  with `C-c C-s l`.  If there is an active region, the text in the region
;; will be placed inside the code block.  You will be prompted for the name
;; of the language, but may press enter to continue without naming a language.

;;; Acknowledgments:

;; markdown-mode has benefited greatly from the efforts of the
;; following people:
;;
;;   * Cyril Brulebois <cyril.brulebois@enst-bretagne.fr> for Debian packaging.
;;   * Conal Elliott <conal@conal.net> for a font-lock regexp patch.
;;   * Edward O'Connor <hober0@gmail.com> for a font-lock regexp fix and
;;     GitHub Flavored Markdown mode (`gfm-mode').
;;   * Greg Bognar <greg_bognar@hms.harvard.edu> for menus and running
;;     `markdown' with an active region.
;;   * Daniel Burrows <dburrows@debian.org> for filing Debian bug #456592.
;;   * Peter S. Galbraith <psg@debian.org> for maintaining emacs-goodies-el.
;;   * Dmitry Dzhus <mail@sphinx.net.ru> for reference checking functions.
;;   * Bryan Kyle <bryan.kyle@gmail.com> for indentation code.
;;   * Ben Voui <intrigeri@boum.org> for font-lock face customizations.
;;   * Ankit Solanki <ankit.solanki@gmail.com> for longlines.el
;;     compatibility and custom CSS.
;;   * Hilko Bengen <bengen@debian.org> for proper XHTML output.
;;   * Jose A. Ortega Ruiz <jao@gnu.org> for Emacs 23 fixes.
;;   * Alec Resnick <alec@sproutward.org> for bug reports.
;;   * Joost Kremers <joostkremers@fastmail.fm> for footnote-handling
;;     functions, bug reports regarding indentation, and
;;     fixes for byte-compilation warnings.
;;   * Peter Williams <pezra@barelyenough.org> for fill-paragraph
;;     enhancements.
;;   * George Ogata <george.ogata@gmail.com> for fixing several
;;     byte-compilation warnings.
;;   * Eric Merritt <ericbmerritt@gmail.com> for wiki link features.
;;   * Philippe Ivaldi <pivaldi@sfr.fr> for XHTML preview
;;     customizations and XHTML export.
;;   * Jeremiah Dodds <jeremiah.dodds@gmail.com> for supporting
;;     Markdown processors which do not accept input from stdin.
;;   * Werner Dittmann <werner.dittmann@t-online.de> for bug reports
;;     regarding the cl dependency and auto-fill-mode and indentation.
;;   * Scott Pfister <scott.pfister@gmail.com> for generalizing the space
;;     substitution character for mapping wiki links to filenames.
;;   * Marcin Kasperski <marcin.kasperski@mekk.waw.pl> for a patch to
;;     escape shell commands.
;;   * Christopher J. Madsen <cjm@cjmweb.net> for patches to fix a match
;;     data bug and to prefer `visual-line-mode' in `gfm-mode'.
;;   * Shigeru Fukaya <shigeru.fukaya@gmail.com> for better adherence to
;;     Emacs Lisp coding conventions.
;;   * Donald Ephraim Curtis <dcurtis@milkbox.net> for fixing the `paragraph-fill'
;;     regexp, refactoring the compilation and preview functions,
;;     heading font-lock generalizations, list renumbering,
;;     and kill ring save.
;;   * Kevin Porter <kportertx@gmail.com> for wiki link handling in `gfm-mode'.
;;   * Max Penet <max.penet@gmail.com> and Peter Eisentraut <peter_e@gmx.net>
;;     for an autoload token for `gfm-mode'.
;;   * Ian Yang <me@iany.me> for improving the reference definition regex.
;;   * Akinori Musha <knu@idaemons.org> for an imenu index function.
;;   * Michael Sperber <sperber@deinprogramm.de> for XEmacs fixes.
;;   * Francois Gannaz <francois.gannaz@free.fr> for suggesting charset
;;     declaration in XHTML output.
;;   * Zhenlei Jia <zhenlei.jia@gmail.com> for smart (dedention)
;;     un-indentation function.
;;   * Matus Goljer <dota.keys@gmail.com> for improved wiki link following
;;     and GFM code block insertion.
;;   * Peter Jones <pjones@pmade.com> for link following functions.
;;   * Bryan Fink <bryan.fink@gmail.com> for a bug report regarding
;;     externally modified files.
;;   * Vegard Vesterheim <vegard.vesterheim@uninett.no> and Carsten Dominik
;;     for a bug fix related to orgtbl-mode.
;;   * Makoto Motohashi <mkt.motohashi@gmail.com> for before- and after-
;;     export hooks and unit test improvements.

;;; Bugs:

;; Although markdown-mode is developed and tested primarily using
;; GNU Emacs 24, compatibility with earlier Emacsen is also a
;; priority.
;;
;; If you find any bugs in markdown-mode, please construct a test case
;; or a patch and email me at <jrblevin@sdf.org>.

;;; History:

;; markdown-mode was written and is maintained by Jason Blevins.  The
;; first version was released on May 24, 2007.
;;
;;   * 2007-05-24: Version 1.1
;;   * 2007-05-25: Version 1.2
;;   * 2007-06-05: [Version 1.3][]
;;   * 2007-06-29: Version 1.4
;;   * 2008-05-24: [Version 1.5][]
;;   * 2008-06-04: [Version 1.6][]
;;   * 2009-10-01: [Version 1.7][]
;;   * 2011-08-12: [Version 1.8][]
;;   * 2011-08-15: [Version 1.8.1][]
;;   * 2013-01-25: [Version 1.9][]
;;
;; [Version 1.3]: http://jblevins.org/projects/markdown-mode/rev-1-3
;; [Version 1.5]: http://jblevins.org/projects/markdown-mode/rev-1-5
;; [Version 1.6]: http://jblevins.org/projects/markdown-mode/rev-1-6
;; [Version 1.7]: http://jblevins.org/projects/markdown-mode/rev-1-7
;; [Version 1.8]: http://jblevins.org/projects/markdown-mode/rev-1-8
;; [Version 1.8.1]: http://jblevins.org/projects/markdown-mode/rev-1-8-1
;; [Version 1.9]: http://jblevins.org/projects/markdown-mode/rev-1-9


;;; Code:

(require 'easymenu)
(require 'outline)
(eval-when-compile (require 'cl))

;;; Constants =================================================================

(defconst markdown-mode-version "1.9"
  "Markdown mode version number.")

(defconst markdown-output-buffer-name "*markdown-output*"
  "Name of temporary buffer for markdown command output.")

;;; Customizable variables ====================================================

(defvar markdown-mode-hook nil
  "Hook run when entering Markdown mode.")

(defvar markdown-before-export-hooks nil
  "Hook run before output XHTML.
This hook is abnormal and registered functions are given an argument that is output filename.")

(defvar markdown-after-export-hooks nil
  "Hook run after output XHTML.
This hook is abnormal and registered functions are given an argument that is output filename.")

(defgroup markdown nil
  "Major mode for editing text files in Markdown format."
  :prefix "markdown-"
  :group 'wp
  :link '(url-link "http://jblevins.org/projects/markdown-mode/"))

(defcustom markdown-command "markdown"
  "Command to run markdown."
  :group 'markdown
  :type 'string)

(defcustom markdown-command-needs-filename nil
  "Set to non-nil if `markdown-command' does not accept input from stdin.
Instead, it will be passed a filename as the final command line
option.  As a result, you will only be able to run Markdown from
buffers which are visiting a file."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-open-command nil
  "Command used for opening Markdown files directly.
For example, a standalone Markdown previewer.  This command will
be called with a single argument: the filename of the current
buffer."
  :group 'markdown
  :type 'string)

(defcustom markdown-hr-string "* * * * *"
  "String to use for horizonal rules."
  :group 'markdown
  :type 'string)

(defcustom markdown-bold-underscore nil
  "Use two underscores for bold instead of two asterisks."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-italic-underscore nil
  "Use underscores for italic instead of asterisks."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-indent-function 'markdown-indent-line
  "Function to use to indent."
  :group 'markdown
  :type 'function)

(defcustom markdown-indent-on-enter t
  "Automatically indent new lines when enter key is pressed."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-follow-wiki-link-on-enter t
  "Follow wiki link at point (if any) when the enter key is pressed."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-wiki-link-alias-first t
  "When non-nil, treat aliased wiki links like [[alias text|PageName]].
Otherwise, they will be treated as [[PageName|alias text]]."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-uri-types
  '("acap" "cid" "data" "dav" "fax" "file" "ftp" "gopher" "http" "https"
    "imap" "ldap" "mailto" "mid" "modem" "news" "nfs" "nntp" "pop" "prospero"
    "rtsp" "service" "sip" "tel" "telnet" "tip" "urn" "vemmi" "wais")
  "Link types for syntax highlighting of URIs."
  :group 'markdown
  :type 'list)

(defcustom markdown-enable-math nil
  "Syntax highlighting for inline LaTeX expressions.
This will not take effect until Emacs is restarted."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-css-path ""
  "URL of CSS file to link to in the output XHTML."
  :group 'markdown
  :type 'string)

(defcustom markdown-content-type ""
  "Content type string for the http-equiv header in XHTML output.
When set to a non-empty string, insert the http-equiv attribute.
Otherwise, this attribute is omitted."
  :group 'markdown
  :type 'string)

(defcustom markdown-coding-system nil
  "Character set string for the http-equiv header in XHTML output.
Defaults to `buffer-file-coding-system' (and falling back to
`iso-8859-1' when not available).  Common settings are `utf-8'
and `iso-latin-1'.  Use `list-coding-systems' for more choices."
  :group 'markdown
  :type 'coding-system)

(defcustom markdown-xhtml-header-content ""
  "Additional content to include in the XHTML <head> block."
  :group 'markdown
  :type 'string)

(defcustom markdown-xhtml-standalone-regexp
  "^\\(\<\?xml\\|\<!DOCTYPE\\|\<html\\)"
  "Regexp indicating whether `markdown-command' output is standalone XHTML."
  :group 'markdown
  :type 'regexp)

(defcustom markdown-link-space-sub-char
  "_"
  "Character to use instead of spaces when mapping wiki links to filenames."
  :group 'markdown
  :type 'string)

(defcustom markdown-footnote-location 'end
  "Position where new footnotes are inserted in the document."
  :group 'markdown
  :type '(choice (const :tag "At the end of the document" end)
                 (const :tag "Immediately after the paragraph" immediately)
                 (const :tag "Before next header" header)))

;;; Font lock =================================================================

(require 'font-lock)

(defvar markdown-italic-face 'markdown-italic-face
  "Face name to use for italic text.")

(defvar markdown-bold-face 'markdown-bold-face
  "Face name to use for bold text.")

(defvar markdown-header-delimiter-face 'markdown-header-delimiter-face
  "Face name to use as a base for header delimiters.")

(defvar markdown-header-rule-face 'markdown-header-rule-face
  "Face name to use as a base for header rules.")

(defvar markdown-header-face 'markdown-header-face
  "Face name to use as a base for headers.")

(defvar markdown-header-face-1 'markdown-header-face-1
  "Face name to use for level-1 headers.")

(defvar markdown-header-face-2 'markdown-header-face-2
  "Face name to use for level-2 headers.")

(defvar markdown-header-face-3 'markdown-header-face-3
  "Face name to use for level-3 headers.")

(defvar markdown-header-face-4 'markdown-header-face-4
  "Face name to use for level-4 headers.")

(defvar markdown-header-face-5 'markdown-header-face-5
  "Face name to use for level-5 headers.")

(defvar markdown-header-face-6 'markdown-header-face-6
  "Face name to use for level-6 headers.")

(defvar markdown-inline-code-face 'markdown-inline-code-face
  "Face name to use for inline code.")

(defvar markdown-list-face 'markdown-list-face
  "Face name to use for list markers.")

(defvar markdown-blockquote-face 'markdown-blockquote-face
  "Face name to use for blockquote.")

(defvar markdown-pre-face 'markdown-pre-face
  "Face name to use for preformatted text.")

(defvar markdown-language-keyword-face 'markdown-language-keyword-face
  "Face name to use for programming language identifiers.")

(defvar markdown-link-face 'markdown-link-face
  "Face name to use for links.")

(defvar markdown-missing-link-face 'markdown-missing-link-face
  "Face name to use for links where the linked file does not exist.")

(defvar markdown-reference-face 'markdown-reference-face
  "Face name to use for reference.")

(defvar markdown-footnote-face 'markdown-footnote-face
  "Face name to use for footnote identifiers.")

(defvar markdown-url-face 'markdown-url-face
  "Face name to use for URLs.")

(defvar markdown-link-title-face 'markdown-link-title-face
  "Face name to use for reference link titles.")

(defvar markdown-comment-face 'markdown-comment-face
  "Face name to use for HTML comments.")

(defvar markdown-math-face 'markdown-math-face
  "Face name to use for LaTeX expressions.")

(defgroup markdown-faces nil
  "Faces used in Markdown Mode"
  :group 'markdown
  :group 'faces)

(defface markdown-italic-face
  '((t (:inherit font-lock-variable-name-face :slant italic)))
  "Face for italic text."
  :group 'markdown-faces)

(defface markdown-bold-face
  '((t (:inherit font-lock-variable-name-face :weight bold)))
  "Face for bold text."
  :group 'markdown-faces)

(defface markdown-header-rule-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers rules."
  :group 'markdown-faces)

(defface markdown-header-delimiter-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers hash delimiter."
  :group 'markdown-faces)

(defface markdown-header-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers."
  :group 'markdown-faces)

(defface markdown-header-face-1
  '((t (:inherit markdown-header-face)))
  "Face for level-1 headers."
  :group 'markdown-faces)

(defface markdown-header-face-2
  '((t (:inherit markdown-header-face)))
  "Face for level-2 headers."
  :group 'markdown-faces)

(defface markdown-header-face-3
  '((t (:inherit markdown-header-face)))
  "Face for level-3 headers."
  :group 'markdown-faces)

(defface markdown-header-face-4
  '((t (:inherit markdown-header-face)))
  "Face for level-4 headers."
  :group 'markdown-faces)

(defface markdown-header-face-5
  '((t (:inherit markdown-header-face)))
  "Face for level-5 headers."
  :group 'markdown-faces)

(defface markdown-header-face-6
  '((t (:inherit markdown-header-face)))
  "Face for level-6 headers."
  :group 'markdown-faces)

(defface markdown-inline-code-face
  '((t (:inherit font-lock-constant-face)))
  "Face for inline code."
  :group 'markdown-faces)

(defface markdown-list-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for list item markers."
  :group 'markdown-faces)

(defface markdown-blockquote-face
  '((t (:inherit font-lock-doc-face)))
  "Face for blockquote sections."
  :group 'markdown-faces)

(defface markdown-pre-face
  '((t (:inherit font-lock-constant-face)))
  "Face for preformatted text."
  :group 'markdown-faces)

(defface markdown-language-keyword-face
  '((t (:inherit font-lock-type-face)))
  "Face for programming language identifiers."
  :group 'markdown-faces)

(defface markdown-link-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for links."
  :group 'markdown-faces)

(defface markdown-missing-link-face
  '((t (:inherit font-lock-warning-face)))
  "Face for missing links."
  :group 'markdown-faces)

(defface markdown-reference-face
  '((t (:inherit font-lock-type-face)))
  "Face for link references."
  :group 'markdown-faces)

(defface markdown-footnote-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for footnote markers."
  :group 'markdown-faces)

(defface markdown-url-face
  '((t (:inherit font-lock-string-face)))
  "Face for URLs."
  :group 'markdown-faces)

(defface markdown-link-title-face
  '((t (:inherit font-lock-comment-face)))
  "Face for reference link titles."
  :group 'markdown-faces)

(defface markdown-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for HTML comments."
  :group 'markdown-faces)

(defface markdown-math-face
  '((t (:inherit font-lock-string-face)))
  "Face for LaTeX expressions."
  :group 'markdown-faces)

(defconst markdown-regex-link-inline
  "\\(!?\\[[^]]*?\\]\\)\\(([^\\)]*)\\)"
  "Regular expression for a [text](file) or an image link ![text](file).")

(defconst markdown-regex-link-reference
  "\\(!?\\[[^]]+?\\]\\)[ ]?\\(\\[[^]]*?\\]\\)"
  "Regular expression for a reference link [text][id].")

(defconst markdown-regex-reference-definition
  "^ \\{0,3\\}\\(\\[[^\n]+?\\]\\):\\s *\\(.*?\\)\\s *\\( \"[^\"]*\"$\\|$\\)"
  "Regular expression for a link definition [id]: ...")

(defconst markdown-regex-footnote
  "\\(\\[\\^.+?\\]\\)"
  "Regular expression for a footnote marker [^fn].")

(defconst markdown-regex-header
  "^\\(?:\\(.+\\)\n\\(=+\\)\\|\\(.+\\)\n\\(-+\\)\\|\\(#+\\)\\s-*\\(.*?\\)\\s-*?\\(#*\\)\\)$"
  "Regexp identifying Markdown headers.")

(defconst markdown-regex-header-1-atx
  "^\\(# \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 1 atx-style (hash mark) headers.")

(defconst markdown-regex-header-2-atx
  "^\\(## \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 2 atx-style (hash mark) headers.")

(defconst markdown-regex-header-3-atx
  "^\\(### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 3 atx-style (hash mark) headers.")

(defconst markdown-regex-header-4-atx
  "^\\(#### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 4 atx-style (hash mark) headers.")

(defconst markdown-regex-header-5-atx
  "^\\(##### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 5 atx-style (hash mark) headers.")

(defconst markdown-regex-header-6-atx
  "^\\(###### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 6 atx-style (hash mark) headers.")

(defconst markdown-regex-header-1-setext
  "^\\(.*\\)\n\\(=+\\)$"
  "Regular expression for level 1 setext-style (underline) headers.")

(defconst markdown-regex-header-2-setext
  "^\\(.*\\)\n\\(-+\\)$"
  "Regular expression for level 2 setext-style (underline) headers.")

(defconst markdown-regex-header-setext
  "^\\(.+\\)\n\\(\\(?:=\\|-\\)+\\)$"
  "Regular expression for generic setext-style (underline) headers.")

(defconst markdown-regex-header-atx
  "^\\(#+\\)[ \t]*\\(.*?\\)[ \t]*\\(#*\\)$"
  "Regular expression for generic atx-style (hash mark) headers.")

(defconst markdown-regex-hr
  "^\\(\\*[ ]?\\*[ ]?\\*[ ]?[\\* ]*\\|-[ ]?-[ ]?-[--- ]*\\)$"
  "Regular expression for matching Markdown horizontal rules.")

(defconst markdown-regex-code
  "\\(\\`\\|[^\\]\\)\\(\\(`+\\)\\(\\(.\\|\n[^\n]\\)*?[^`]\\)\\3\\)\\([^`]\\|\\'\\)"
  "Regular expression for matching inline code fragments.

The first group ensures that the leading backquote character
is not escaped.  The group \\(.\\|\n[^\n]\\) matches any
character, including newlines, but not two newlines in a row.
The final group requires that the character following the code
fragment is not a backquote.")

(defconst markdown-regex-pre
  "^\\(    \\|\t\\).*$"
  "Regular expression for matching preformatted text sections.")

(defconst markdown-regex-list
  "^\\([ \t]*\\)\\([0-9]+\\.\\|[\\*\\+-]\\)\\([ \t]+\\)"
  "Regular expression for matching list items.")

(defconst markdown-regex-bold
  "\\(^\\|[^\\]\\)\\(\\([*_]\\{2\\}\\)\\([^ \\]\\|[^ ]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(\\3\\)\\)"
  "Regular expression for matching bold text.
Group 1 matches the character before the opening asterisk or
underscore, if any, ensuring that it is not a backslash escape.
Group 2 matches the entire expression, including delimiters.
Groups 3 and 5 matches the opening and closing delimiters.
Group 4 matches the text inside the delimiters.")

(defconst markdown-regex-italic
  "\\(^\\|[^\\]\\)\\(\\([*_]\\)\\([^ \\]\\|[^ ]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(\\3\\)\\)"
  "Regular expression for matching italic text.
Group 1 matches the character before the opening asterisk or
underscore, if any, ensuring that it is not a backslash escape.
Group 2 matches the entire expression, including delimiters.
Groups 3 and 5 matches the opening and closing delimiters.
Group 4 matches the text inside the delimiters.")

(defconst markdown-regex-blockquote
  "^[ \t]*\\(>\\).*$"
  "Regular expression for matching blockquote lines.")

(defconst markdown-regex-line-break
  "  $"
  "Regular expression for matching line breaks.")

(defconst markdown-regex-wiki-link
  "\\(?:^\\|[^\\]\\)\\(\\[\\[\\([^]|]+\\)\\(|\\([^]]+\\)\\)?\\]\\]\\)"
  "Regular expression for matching wiki links.
This matches typical bracketed [[WikiLinks]] as well as 'aliased'
wiki links of the form [[PageName|link text]].  In this regular
expression, #1 matches the page name and #3 matches the link
text.")

(defconst markdown-regex-uri
  (concat
   "\\(" (mapconcat 'identity markdown-uri-types "\\|")
   "\\):[^]\t\n\r<>,;() ]+")
  "Regular expression for matching inline URIs.")

(defconst markdown-regex-angle-uri
  (concat
   "\\(<\\)\\(\\(?:"
   (mapconcat 'identity markdown-uri-types "\\|")
   "\\):[^]\t\n\r<>,;()]+\\)\\(>\\)")
  "Regular expression for matching inline URIs in angle brackets.")

(defconst markdown-regex-email
  "<\\(\\sw\\|\\s_\\|\\s.\\)+@\\(\\sw\\|\\s_\\|\\s.\\)+>"
  "Regular expression for matching inline email addresses.")

(defconst markdown-regex-latex-expression
  "\\(^\\|[^\\]\\)\\(\\$\\($\\([^\\$]\\|\\\\.\\)*\\$\\|\\([^\\$]\\|\\\\.\\)*\\)\\$\\)"
  "Regular expression for itex $..$ or $$..$$ math mode expressions.")

(defconst markdown-regex-latex-display
  "^\\\\\\[\\(.\\|\n\\)*?\\\\\\]$"
  "Regular expression for itex \[..\] display mode expressions.")

(defvar markdown-mode-font-lock-keywords-basic
  (list
   (cons 'markdown-match-pre-blocks '((0 markdown-pre-face)))
   (cons 'markdown-match-fenced-code-blocks '((0 markdown-pre-face)))
   (cons 'markdown-match-gfm-code-blocks '((1 markdown-pre-face)
                                           (2 markdown-language-keyword-face nil t)
                                           (3 markdown-pre-face)
                                           (4 markdown-pre-face)))
   (cons markdown-regex-blockquote 'markdown-blockquote-face)
   (cons markdown-regex-header-1-setext '((1 markdown-header-face-1)
                                          (2 markdown-header-rule-face)))
   (cons markdown-regex-header-2-setext '((1 markdown-header-face-2)
                                          (2 markdown-header-rule-face)))
   (cons markdown-regex-header-1-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-1)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-header-2-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-2)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-header-3-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-3)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-header-4-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-4)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-header-5-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-5)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-header-6-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-6)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-hr 'markdown-header-face)
   (cons 'markdown-match-comments '((0 markdown-comment-face t t)))
   (cons markdown-regex-code '(2 markdown-inline-code-face))
   (cons markdown-regex-angle-uri 'markdown-link-face)
   (cons markdown-regex-uri 'markdown-link-face)
   (cons markdown-regex-email 'markdown-link-face)
   (cons markdown-regex-list '(2 markdown-list-face))
   (cons markdown-regex-link-inline '((1 markdown-link-face t)
                                      (2 markdown-url-face t)))
   (cons markdown-regex-link-reference '((1 markdown-link-face t)
                                         (2 markdown-reference-face t)))
   (cons markdown-regex-reference-definition '((1 markdown-reference-face t)
                                               (2 markdown-url-face t)
                                               (3 markdown-link-title-face t)))
   (cons markdown-regex-footnote 'markdown-footnote-face)
   (cons markdown-regex-bold '(2 markdown-bold-face))
   (cons markdown-regex-italic '(2 markdown-italic-face))
   )
  "Syntax highlighting for Markdown files.")

(defconst markdown-mode-font-lock-keywords-latex
  (list
   ;; Math mode $..$ or $$..$$
   (cons markdown-regex-latex-expression '(2 markdown-math-face))
   ;; Display mode equations with brackets: \[ \]
   (cons markdown-regex-latex-display 'markdown-math-face)
   ;; Equation reference (eq:foo)
   (cons "(eq:\\w+)" 'markdown-reference-face)
   ;; Equation reference \eqref{foo}
   (cons "\\\\eqref{\\w+}" 'markdown-reference-face))
  "Syntax highlighting for LaTeX fragments.")

(defvar markdown-mode-font-lock-keywords
  (append
   (if markdown-enable-math
       markdown-mode-font-lock-keywords-latex)
   markdown-mode-font-lock-keywords-basic)
  "Default highlighting expressions for Markdown mode.")

;; Footnotes
(defvar markdown-footnote-counter 0
  "Counter for footnote numbers.")
(make-variable-buffer-local 'markdown-footnote-counter)

(defconst markdown-footnote-chars
  "[[:alnum:]-]"
  "Regular expression maching any character that is allowed in a footnote identifier.")



;;; Compatibility =============================================================

;; Handle replace-regexp-in-string in XEmacs 21
(defun markdown-replace-regexp-in-string (regexp rep string)
  "Compatibility wrapper to provide `replace-regexp-in-string'.
Replace all matches for REGEXP with REP in STRING."
  (if (featurep 'xemacs)
      (replace-in-string string regexp rep)
    (replace-regexp-in-string regexp rep string)))

;; `markdown-use-region-p' is a compatibility function which checks
;; for an active region, with fallbacks for older Emacsen and XEmacs.
(eval-and-compile
  (cond
   ;; Emacs 23 and newer
   ((fboundp 'use-region-p)
    (defalias 'markdown-use-region-p 'use-region-p))
   ;; Older Emacsen
   ((and (boundp 'transient-mark-mode) (boundp 'mark-active))
    (defun markdown-use-region-p ()
      "Compatibility wrapper to provide `use-region-p'."
      (and transient-mark-mode mark-active)))
   ;; XEmacs
   ((fboundp 'region-active-p)
    (defalias 'markdown-use-region-p 'region-active-p))))



;;; Markdown parsing functions ================================================

(defun markdown-cur-line-blank-p ()
  "Return t if the current line is blank and nil otherwise."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "^\\s *$" (point-at-eol) t)))

(defun markdown-prev-line-blank-p ()
  "Return t if the previous line is blank and nil otherwise.
If we are at the first line, then consider the previous line to be blank."
  (save-excursion
    (if (= (point-at-bol) (point-min))
        t
      (forward-line -1)
      (markdown-cur-line-blank-p))))

(defun markdown-next-line-blank-p ()
  "Return t if the next line is blank and nil otherwise.
If we are at the last line, then consider the next line to be blank."
  (save-excursion
    (if (= (point-at-bol) (point-max))
        t
      (forward-line 1)
      (markdown-cur-line-blank-p))))

(defun markdown-prev-line-indent-p ()
  "Return t if the previous line is indented and nil otherwise."
  (save-excursion
    (forward-line -1)
    (goto-char (point-at-bol))
    (if (re-search-forward "^\\s " (point-at-eol) t) t)))

(defun markdown-cur-line-indent ()
  "Return the number of leading whitespace characters in the current line."
  (save-match-data
    (save-excursion
      (goto-char (point-at-bol))
      (re-search-forward "^[ \t]+" (point-at-eol) t)
      (current-column))))

(defun markdown-prev-line-indent ()
  "Return the number of leading whitespace characters in the previous line."
  (save-excursion
    (forward-line -1)
    (markdown-cur-line-indent)))

(defun markdown-next-line-indent ()
  "Return the number of leading whitespace characters in the next line."
  (save-excursion
    (forward-line 1)
    (markdown-cur-line-indent)))

(defun markdown-cur-non-list-indent ()
  "Return the number of leading whitespace characters in the current line."
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward markdown-regex-list (point-at-eol) t)
        (current-column)))))

(defun markdown-prev-non-list-indent ()
  "Return position of the first non-list-marker on the previous line."
  (save-excursion
    (forward-line -1)
    (markdown-cur-non-list-indent)))

(defun markdown--next-block ()
  "Move the point to the start of the next text block."
  (forward-line)
  (while (and (or (not (markdown-prev-line-blank-p))
                  (markdown-cur-line-blank-p))
              (not (or (looking-at markdown-regex-list)
                       (looking-at markdown-regex-header)
                       (looking-at markdown-regex-hr)))
              (not (eobp)))
    (forward-line)))

(defun markdown-new-baseline-p ()
  "Determine if the current line begins a new baseline level."
  (save-excursion
    (beginning-of-line)
    (save-match-data
      (or (looking-at markdown-regex-header)
          (looking-at markdown-regex-hr)
          (and (null (markdown-cur-non-list-indent))
               (= (markdown-cur-line-indent) 0)
               (markdown-prev-line-blank-p))))))

(defun markdown-search-backward-baseline ()
  "Search backward baseline point with no indentation and not a list item."
  (end-of-line)
  (let (stop)
    (while (not (or stop (bobp)))
      (re-search-backward "\\(\\`\\|\\(\n[ \t]*\n\\)[^\n \t]\\)" nil t)
      (when (match-end 2)
        (goto-char (match-end 2))
        (cond
         ((markdown-new-baseline-p)
          (setq stop t))
         ((looking-at markdown-regex-list)
          (setq stop nil))
         (t (setq stop t)))))))

(defun markdown-update-list-levels (marker indent levels)
  "Update list levels given list MARKER, block INDENT, and current LEVELS.
Here, MARKER is a string representing the type of list, INDENT is an integer
giving the indentation, in spaces, of the current block, and LEVELS is a
list of the indentation levels of parent list items.  When LEVELS is nil,
it means we are at baseline (not inside of a nested list)."
  (cond
   ;; New list item at baseline.
   ((and marker (null levels))
    (setq levels (list indent)))
   ;; List item with greater indentation (four or more spaces).
   ;; Increase list level.
   ((and marker (>= indent (+ (car levels) 4)))
    (setq levels (cons indent levels)))
   ;; List item with greater or equal indentation (less than four spaces).
   ;; Do not increase list level.
   ((and marker (>= indent (car levels)))
    levels)
   ;; Lesser indentation level.
   ;; Pop appropriate number of elements off LEVELS list (e.g., lesser
   ;; indentation could move back more than one list level).  Note
   ;; that this block need not be the beginning of list item.
   ((< indent (car levels))
    (while (and (> (length levels) 1)
                (< indent (+ (car (cdr levels)) 4)))
      (setq levels (cdr levels)))
    levels)
   ;; Otherwise, do nothing.
   (t levels)))

(defun markdown-prev-list-item (level)
  "Search backward from point for a list item with indentation LEVEL.
Set point to the beginning of the item, and return point, or nil
upon failure."
  (let (bounds indent prev)
    (setq prev (point))
    (forward-line -1)
    (setq indent (markdown-cur-line-indent))
    (while
        (cond
         ;; Stop at beginning of buffer
         ((bobp) (setq prev nil))
         ;; Continue if current line is blank
         ((markdown-cur-line-blank-p) t)
         ;; List item
         ((and (looking-at markdown-regex-list)
               (setq bounds (markdown-cur-list-item-bounds)))
          (cond
           ;; Continue at item with greater indentation
           ((> (nth 3 bounds) level) t)
           ;; Stop and return point at item of equal indentation
           ((= (nth 3 bounds) level)
            (setq prev (point))
            nil)
           ;; Stop and return nil at item with lesser indentation
           ((< (nth 3 bounds) level)
            (setq prev nil)
            nil)))
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the next is blank
         ((and (< indent level)
               (markdown-next-line-blank-p))
          (setq prev nil))
         ;; Stop at a header
         ((looking-at markdown-regex-header) (setq prev nil))
         ;; Stop at a horizontal rule
         ((looking-at markdown-regex-hr) (setq prev nil))
         ;; Otherwise, continue.
         (t t))
      (forward-line -1)
      (setq indent (markdown-cur-line-indent)))
    prev))

(defun markdown-next-list-item (level)
  "Search forward from point for the next list item with indentation LEVEL.
Set point to the beginning of the item, and return point, or nil
upon failure."
  (let (bounds indent prev next)
    (setq next (point))
    (forward-line)
    (setq indent (markdown-cur-line-indent))
    (while
        (cond
         ;; Stop at end of the buffer.
         ((eobp) (setq prev nil))
         ;; Continue if the current line is blank
         ((markdown-cur-line-blank-p) t)
         ;; List item
         ((and (looking-at markdown-regex-list)
               (setq bounds (markdown-cur-list-item-bounds)))
          (cond
           ;; Continue at item with greater indentation
           ((> (nth 3 bounds) level) t)
           ;; Stop and return point at item of equal indentation
           ((= (nth 3 bounds) level)
            (setq next (point))
            nil)
           ;; Stop and return nil at item with lesser indentation
           ((< (nth 3 bounds) level)
            (setq next nil)
            nil)))
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the previous line was blank.
         ((and (< indent level)
               (markdown-prev-line-blank-p))
          (setq next nil))
         ;; Stop at a header
         ((looking-at markdown-regex-header) (setq next nil))
         ;; Stop at a horizontal rule
         ((looking-at markdown-regex-hr) (setq next nil))
         ;; Otherwise, continue.
         (t t))
      (forward-line)
      (setq indent (markdown-cur-line-indent)))
    next))

(defun markdown-cur-list-item-end (level)
  "Move to the end of the current list item with nonlist indentation LEVEL.
If the point is not in a list item, do nothing."
  (let (indent)
    (forward-line)
    (setq indent (markdown-cur-line-indent))
    (while
        (cond
         ;; Stop at end of the buffer.
         ((eobp) nil)
         ;; Continue if the current line is blank
         ((markdown-cur-line-blank-p) t)
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the previous line was blank.
         ((and (< indent level)
               (markdown-prev-line-blank-p))
          nil)
         ;; Stop at a new list item of the same or lesser indentation
         ((looking-at markdown-regex-list) nil)
         ;; Stop at a header
         ((looking-at markdown-regex-header) nil)
         ;; Stop at a horizontal rule
         ((looking-at markdown-regex-hr) nil)
         ;; Otherwise, continue.
         (t t))
      (forward-line)
      (setq indent (markdown-cur-line-indent)))
    ;; Don't skip over whitespace for empty list items (marker and
    ;; whitespace only), just move to end of whitespace.
    (if (looking-back (concat markdown-regex-list "\\s-*"))
          (goto-char (match-end 3))
      (skip-syntax-backward "-"))))

(defun markdown-cur-list-item-bounds ()
  "Return bounds and indentation of the current list item.
Return a list of the form (begin end indent nonlist-indent).
If the point is not inside a list item, return nil.
Leave match data intact for `markdown-regex-list'."
  (let (cur prev-begin prev-end indent nonlist-indent)
    ;; Store current location
    (setq cur (point))
    ;; Verify that cur is between beginning and end of item
    (save-excursion
      (if (looking-at markdown-regex-list)
          (beginning-of-line)
        (end-of-line)
        (re-search-backward markdown-regex-list nil t))
      (save-match-data
        (setq prev-begin (point))
        (setq indent (markdown-cur-line-indent))
        (setq nonlist-indent (markdown-cur-non-list-indent))
        (markdown-cur-list-item-end nonlist-indent)
        (setq prev-end (point)))
      (if (and (>= cur prev-begin)
               (<= cur prev-end)
               nonlist-indent)
          (list prev-begin prev-end indent nonlist-indent)
        nil))))

(defun markdown-bounds-of-thing-at-point (thing)
  "Calls `bounds-of-thing-at-point' for THING with slight modifications.
Does not include trailing newlines when THING is 'line.  Handles the
end of buffer case by setting both endpoints equal to the value of
`point-max', since an empty region will trigger empty markup insertion."
  (let* ((bounds (bounds-of-thing-at-point thing))
         (a (car bounds))
         (b (cdr bounds)))
    (when (eq thing 'line)
      (cond ((and (eobp) (markdown-cur-line-blank-p))
             (setq a b))
            ((char-equal (char-before b) ?\^J)
             (setq b (1- b)))))
  (cons a b)))

;;; Markdown font lock matching functions =====================================

;; From html-helper-mode
(defun markdown-match-comments (last)
  "Match HTML comments from the point to LAST."
  (cond ((search-forward "<!--" last t)
         (backward-char 4)
         (let ((beg (point)))
           (cond ((search-forward-regexp "--[ \t]*>" last t)
                  (set-match-data (list beg (point)))
                  t)
                 (t nil))))
        (t nil)))

(defun markdown-match-pre-blocks (last)
  "Match Markdown pre blocks from point to LAST."
  (let ((first (point)) levels indent pre-regexp end-regexp begin end stop)
    ;; Find a baseline point with zero list indentation
    (markdown-search-backward-baseline)

    ;; Search for all list items between baseline and FIRST
    (while (re-search-forward markdown-regex-list first t)
      (cond
       ;; Make sure this is not a header or hr
       ((markdown-new-baseline-p) (forward-line) (setq levels nil))
       ;; If not, then update levels
       (t
        (setq indent (markdown-cur-line-indent))
        (setq levels (markdown-update-list-levels (match-string 2) indent levels)))))

    ;; Search for pre blocks from FIRST to LAST
    (goto-char first)
    (while (and (< (point) last) (not end))
      ;; Search for a region with sufficient indentation
      (if (null levels)
          (setq indent 1)
        (setq indent (1+ (length levels))))
      (setq pre-regexp (format "^\\(    \\|\t\\)\\{%d\\}" indent))
      (setq end-regexp (format "^\\(    \\|\t\\)\\{0,%d\\}\\([^ \t]\\)" (1- indent)))

      (cond
       ;; If not at the beginning of a line, move forward
       ((not (bolp)) (forward-line))
       ;; Move past blank lines
       ((markdown-cur-line-blank-p) (forward-line))
       ;; At headers and horizontal rules, reset levels
       ((markdown-new-baseline-p) (forward-line) (setq levels nil))
       ;; If the current line has sufficient indentation, mark out pre block
       ((looking-at pre-regexp)
        (setq begin (match-beginning 0))
        (while (and (or (looking-at pre-regexp) (markdown-cur-line-blank-p))
                    (not (eobp)))
          (forward-line))
        (setq end (point)))
       ;; If current line has a list marker, update levels, move to end of block
       ((looking-at markdown-regex-list)
        (setq levels (markdown-update-list-levels
                      (match-string 2) (markdown-cur-line-indent) levels))
        (markdown--next-block))
       ;; If this is the end of the indentation level, adjust levels accordingly.
       ;; Only match end of indentation level if levels is not the empty list.
       ((and (car levels) (looking-at end-regexp))
        (setq levels (markdown-update-list-levels
                      nil (markdown-cur-line-indent) levels))
        (markdown--next-block))
       (t (markdown--next-block))))

    (if (not (and begin end))
        ;; Return nil if no pre block was found
        nil
      ;; Set match data and return t upon success
      (set-match-data (list begin end))
      t)))

(defun markdown-match-fenced-code-blocks (last)
  "Match fenced code blocks from the point to LAST."
  (cond ((search-forward-regexp "^\\([~]\\{3,\\}\\)" last t)
         (beginning-of-line)
         (let ((beg (point)))
           (forward-line)
           (cond ((search-forward-regexp
                   (concat "^" (match-string 1) "~*") last t)
                  (set-match-data (list beg (point)))
                  t)
                 (t nil))))
        (t nil)))

(defun markdown-match-gfm-code-blocks (last)
  "Match GFM quoted code blocks from point to LAST."
  (let (open lang body close all)
    (cond ((and (eq major-mode 'gfm-mode)
                (search-forward-regexp "^\\(```\\)\\(\\w+\\)?$" last t))
           (beginning-of-line)
           (setq open (list (match-beginning 1) (match-end 1))
                 lang (list (match-beginning 2) (match-end 2)))
           (forward-line)
           (setq body (list (point)))
           (cond ((search-forward-regexp "^```$" last t)
                  (setq body (reverse (cons (1- (match-beginning 0)) body))
                        close (list (match-beginning 0) (match-end 0))
                        all (list (car open) (match-end 0)))
                  (set-match-data (append all open lang body close))
                  t)
                 (t nil)))
          (t nil))))

(defun markdown-font-lock-extend-region ()
  "Extend the search region to include an entire block of text.
This helps improve font locking for block constructs such as pre blocks."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (or (re-search-backward "\n\n" nil t) (point-min))))
      (goto-char font-lock-end)
      (when (re-search-forward "\n\n" nil t)
        (beginning-of-line)
        (setq font-lock-end (point)))
      (setq font-lock-beg found))))



;;; Syntax Table ==============================================================

(defvar markdown-mode-syntax-table
  (let ((tab (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\" "." tab)
    tab)
  "Syntax table for `markdown-mode'.")



;;; Element Insertion =========================================================

(defun markdown-ensure-blank-line-before ()
  "If previous line is not already blank, insert a blank line before point."
  (unless (bolp) (insert "\n"))
  (unless (or (bobp) (looking-back "\n\\s-*\n")) (insert "\n")))

(defun markdown-ensure-blank-line-after ()
  "If following line is not already blank, insert a blank line after point.
Return the point where it was originally."
  (save-excursion
    (unless (eolp) (insert "\n"))
    (unless (or (eobp) (looking-at "\n\\s-*\n")) (insert "\n"))))

(defun markdown-wrap-or-insert (s1 s2 &optional thing beg end)
  "Insert the strings S1 and S2, wrapping around region or THING.
If a region is specified by the optional BEG and END arguments,
wrap the strings S1 and S2 around that region.
If there is an active region, wrap the strings S1 and S2 around
the region.  If there is not an active region but the point is at
THING, wrap that thing (which defaults to word).  Otherwise, just
insert S1 and S2 and place the cursor in between.  Return the
bounds of the entire wrapped string, or nil if nothing was wrapped
and S1 and S2 were only inserted."
  (let (a b bounds new-point)
    (cond
     ;; Given region
     ((and beg end)
      (setq a beg
            b end
            new-point (+ (point) (length s1))))
     ;; Active region
     ((markdown-use-region-p)
      (setq a (region-beginning)
            b (region-end)
            new-point (+ (point) (length s1))))
     ;; Thing (word) at point
     ((setq bounds (markdown-bounds-of-thing-at-point (or thing 'word)))
      (setq a (car bounds)
            b (cdr bounds)
            new-point (+ (point) (length s1))))
     ;; No active region and no word
     (t
      (setq a (point)
            b (point))))
    (goto-char b)
    (insert s2)
    (goto-char a)
    (insert s1)
    (when new-point (goto-char new-point))
    (if (= a b)
        nil
      (setq b (+ b (length s1) (length s2)))
      (cons a b))))

(defun markdown-point-after-unwrap (cur prefix suffix)
  "Return desired position of point after an unwrapping operation.
Two cons cells must be provided.  PREFIX gives the bounds of the
prefix string and SUFFIX gives the bounds of the suffix string."
  (cond ((< cur (cdr prefix)) (car prefix))
        ((< cur (car suffix)) (- cur (- (cdr prefix) (car prefix))))
        ((<= cur (cdr suffix))
         (- cur (+ (- (cdr prefix) (car prefix))
                   (- cur (car suffix)))))
        (t cur)))

(defun markdown-unwrap-thing-at-point (regexp all text)
  "Remove prefix and suffix of thing at point and reposition the point.
When the thing at point matches REGEXP, replace the subexpression
ALL with the string in subexpression TEXT.  Reposition the point
in an appropriate location accounting for the removal of prefix
and suffix strings.  Return new bounds of string from group TEXT.
When REGEXP is nil, assumes match data is already set."
  (when (or (null regexp)
            (thing-at-point-looking-at regexp))
    (let ((cur (point))
          (prefix (cons (match-beginning all) (match-beginning text)))
          (suffix (cons (match-end text) (match-end all)))
          (bounds (cons (match-beginning text) (match-end text))))
      ;; Replace the thing at point
      (replace-match (match-string text) t t nil all)
      ;; Reposition the point
      (goto-char (markdown-point-after-unwrap cur prefix suffix))
      ;; Adjust bounds
      (setq bounds (cons (car prefix)
                         (- (cdr bounds) (- (cdr prefix) (car prefix))))))))

(defun markdown-unwrap-things-in-region (beg end regexp all text)
  "Remove prefix and suffix of all things in region.
When a thing in the region matches REGEXP, replace the
subexpression ALL with the string in subexpression TEXT.
Return a cons cell containing updated bounds for the region."
  (save-excursion
    (goto-char beg)
    (let ((removed 0) len-all len-text)
      (while (re-search-forward regexp end t)
        (setq len-all (length (match-string-no-properties all)))
        (setq len-text (length (match-string-no-properties text)))
        (setq removed (+ removed (- len-all len-text)))
        (replace-match (match-string text) t t nil all))
      (cons beg (- end removed)))))

(defun markdown-insert-hr ()
  "Insert a horizonal rule using `markdown-hr-string'."
  (interactive)
  (markdown-ensure-blank-line-before)
  (insert markdown-hr-string)
  (markdown-ensure-blank-line-after))

(defun markdown-insert-bold ()
  "Insert markup to make a region or word bold.
If there is an active region, make the region bold.  If the point
is at a non-bold word, make the word bold.  If the point is at a
bold word or phrase, remove the bold markup.  Otherwise, simply
insert bold delimiters and place the cursor in between them."
  (interactive)
  (let ((delim (if markdown-bold-underscore "__" "**")))
    (if (markdown-use-region-p)
        ;; Active region
        (let ((bounds (markdown-unwrap-things-in-region
                       (region-beginning) (region-end)
                       markdown-regex-bold 2 4)))
          (markdown-wrap-or-insert delim delim nil (car bounds) (cdr bounds)))
      ;; Word at point or markup insert
      (unless (markdown-unwrap-thing-at-point markdown-regex-bold 2 4)
        (markdown-wrap-or-insert delim delim 'word nil nil)))))

(defun markdown-insert-italic ()
  "Insert markup to make a region or word italic.
If there is an active region, make the region italic.  If the point
is at a non-italic word, make the word italic.  If the point is at an
italic word or phrase, remove the italic markup.  Otherwise, simply
insert italic delimiters and place the cursor in between them."
  (interactive)
  (let ((delim (if markdown-italic-underscore "_" "*")))
    (if (markdown-use-region-p)
        ;; Active region
        (let ((bounds (markdown-unwrap-things-in-region
                       (region-beginning) (region-end)
                       markdown-regex-italic 2 4)))
          (markdown-wrap-or-insert delim delim nil (car bounds) (cdr bounds)))
      ;; Word at point or markup insert
      (unless (markdown-unwrap-thing-at-point markdown-regex-italic 2 4)
        (markdown-wrap-or-insert delim delim 'word nil nil)))))

(defun markdown-insert-code ()
  "Insert markup to make a region or word an inline code fragment.
If there is an active region, make the region an inline code
fragment.  If the point is at a word, make the word an inline
code fragment.  Otherwise, simply insert code delimiters and
place the cursor in between them."
  (interactive)
  (if (markdown-use-region-p)
      ;; Active region
      (let ((bounds (markdown-unwrap-things-in-region
                     (region-beginning) (region-end)
                     markdown-regex-code 2 4)))
        (markdown-wrap-or-insert "`" "`" nil (car bounds) (cdr bounds)))
    ;; Word at point or markup insert
    (unless (markdown-unwrap-thing-at-point markdown-regex-code 2 4)
      (markdown-wrap-or-insert "`" "`" 'word nil nil))))

(defun markdown-insert-link ()
  "Insert an inline link, using region or word as link text if possible.
If there is an active region, use the region as the link text.  If the
point is at a word, use the word as the link text.  In these cases, the
point will be left at the position for inserting a URL.  If there is no
active region and the point is not at word, simply insert link markup and
place the point in the position to enter link text."
  (interactive)
  (let ((bounds (markdown-wrap-or-insert "[" "]()")))
    (when bounds
      (goto-char (- (cdr bounds) 1)))))

(defun markdown-insert-reference-link-dwim ()
  "Insert a reference link of the form [text][label] at point.
If Transient Mark mode is on and a region is active, the region
is used as the link text.  Otherwise, the link text will be read
from the minibuffer.  The link URL, label, and title will be read
from the minibuffer.  The link label definition is placed at the
end of the current paragraph."
  (interactive)
  (if (markdown-use-region-p)
      (call-interactively 'markdown-insert-reference-link-region)
    (call-interactively 'markdown-insert-reference-link)))

(defun markdown-insert-reference-link-region (url label title)
  "Insert a reference link at point using the region as the link text.
The link will point to URL, will be referenced as LABEL, and will have the
optional title text given by TITLE."
  (interactive "sLink URL: \nsLink Label (optional): \nsLink Title (optional): ")
  (let ((text (buffer-substring (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (markdown-insert-reference-link text url label title)))

(defun markdown-insert-reference-link (text url label title)
  "Insert a reference link at point.
The link TEXT will point to the given URL and may be referenced using
LABEL.  The link TITLE is optional and will be used to populate the
title attribute when converted to XHTML."
  (interactive "sLink Text: \nsLink URL: \nsLink Label (optional): \nsLink Title (optional): ")
  (let (end)
    (insert (concat "[" text "][" label "]"))
    (setq end (point))
    (forward-paragraph)
    (insert "\n[")
    (if (> (length label) 0)
        (insert label)
      (insert text))
    (insert (concat "]: " url))
    (unless (> (length url) 0)
      (setq end (point)))
    (when (> (length title) 0)
      (insert (concat " \"" title "\"")))
    (insert "\n")
    (unless (looking-at "\n")
      (insert "\n"))
    (goto-char end)))

(defun markdown-insert-wiki-link ()
  "Insert a wiki link of the form [[WikiLink]].
If Transient Mark mode is on and a region is active, it is used
as the link text."
  (interactive)
  (markdown-wrap-or-insert "[[" "]]")
  (backward-char 2))

(defun markdown-insert-image ()
  "Insert inline image markup using region or word as alt text if possible.
If there is an active region, use the region as the alt text.  If the
point is at a word, use the word as the alt text.  In these cases, the
point will be left at the position for inserting a URL.  If there is no
active region and the point is not at word, simply insert image markup and
place the point in the position to enter alt text."
  (interactive)
  (let ((bounds (markdown-wrap-or-insert "![" "]()")))
    (when bounds
      (goto-char (- (cdr bounds) 1)))))

(defun markdown-remove-header ()
  "Remove header markup if point is at a header.
Return bounds of remaining header text if a header was removed
and nil otherwise."
  (interactive "*")
  (or (markdown-unwrap-thing-at-point markdown-regex-header-atx 0 2)
      (markdown-unwrap-thing-at-point markdown-regex-header-setext 0 1)))

(defun markdown-insert-header (&optional level text setext)
  "Insert or replace header markup.
The level of the header is specified by LEVEL and header text is
given by TEXT.  LEVEL must be an integer from 1 and 6, and the
default value is 1.
When TEXT is nil, the header text is obtained as follows.
If there is an active region, it is used as the header text.
Otherwise, the current line will be used as the header text.
If there is not an active region and the point is at a header,
remove the header markup and replace with level N header.
Otherwise, insert empty header markup and place the cursor in
between.
The style of the header will be atx (hash marks) unless
SETEXT is non-nil, in which case a setext-style (underlined)
header will be inserted."
  (interactive "p\nsHeader text: ")
  (setq level (min (max (or level 1) 1) (if setext 2 6)))
  ;; Determine header text if not given
  (when (null text)
    (if (markdown-use-region-p)
        ;; Active region
        (progn
          (setq text (buffer-substring (region-beginning) (region-end)))
          (delete-region (region-beginning) (region-end)))
      ;; No active region
      (markdown-remove-header)
      (setq text (buffer-substring (line-beginning-position) (line-end-position)))
      (delete-region (line-beginning-position) (line-end-position))
      (when (and setext (string-match "^[ \t]*$" text))
        (setq text (read-string "Header text: "))))
    (setq text (markdown-compress-whitespace-string text)))
  ;; Insertion with given text
  (markdown-ensure-blank-line-before)
  (if setext
      ;; setext
      (progn
        (insert text "\n")
        (dotimes (n (length text))
          (insert (if (= level 2) "-" "="))))
    ;; atx
    (dotimes (count level) (insert "#"))
    (insert " " text " ")
    (dotimes (count level) (insert "#")))
  (markdown-ensure-blank-line-after)
  ;; Leave point at end of text
  (if setext
      (backward-char (1+ (length text)))
    (backward-char (1+ level))))

(defun markdown-insert-header-atx-1 ()
  "Insert a first level atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (markdown-insert-header 1 nil nil))

(defun markdown-insert-header-atx-2 ()
  "Insert a level two atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (markdown-insert-header 2 nil nil))

(defun markdown-insert-header-atx-3 ()
  "Insert a level three atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (markdown-insert-header 3 nil nil))

(defun markdown-insert-header-atx-4 ()
  "Insert a level four atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (markdown-insert-header 4 nil nil))

(defun markdown-insert-header-atx-5 ()
  "Insert a level five atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (markdown-insert-header 5 nil nil))

(defun markdown-insert-header-atx-6 ()
  "Insert a sixth level atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (markdown-insert-header 6 nil nil))

(defun markdown-insert-header-setext-1 ()
  "Insert a setext-style (underlined) first-level header.
See `markdown-insert-header'."
  (interactive "*")
  (markdown-insert-header 1 nil t))

(defun markdown-insert-header-setext-2 ()
  "Insert a setext-style (underlined) second-level header.
See `markdown-insert-header'."
  (interactive "*")
  (markdown-insert-header 2 nil t))

(defun markdown-insert-blockquote ()
  "Start a blockquote section (or blockquote the region).
If Transient Mark mode is on and a region is active, it is used as
the blockquote text."
  (interactive)
  (if (markdown-use-region-p)
      (markdown-blockquote-region (region-beginning) (region-end))
    (insert "> ")))

(defun markdown-block-region (beg end prefix)
  "Format the region using a block prefix.
Arguments BEG and END specify the beginning and end of the
region.  The characters PREFIX will appear at the beginning
of each line."
  (if mark-active
      (save-excursion
        ;; Ensure that there is a leading blank line
        (goto-char beg)
        (when (and (>= (point) (+ (point-min) 2))
                   (not (looking-back "\n\n" 2)))
          (insert "\n")
          (setq beg (1+ beg))
          (setq end (1+ end)))
        ;; Move back before any blank lines at the end
        (goto-char end)
        (while (and (looking-back "\n" 1)
                    (not (equal (point) (point-min))))
          (backward-char)
          (setq end (1- end)))
        ;; Ensure that there is a trailing blank line
        (goto-char end)
        (if (not (or (looking-at "\n\n")
                     (and (equal (1+ end) (point-max)) (looking-at "\n"))))
            (insert "\n"))
        ;; Insert PREFIX
        (goto-char beg)
        (beginning-of-line)
        (while (< (point-at-bol) end)
          (insert prefix)
          (setq end (+ (length prefix) end))
          (forward-line)))))

(defun markdown-blockquote-region (beg end)
  "Blockquote the region.
Arguments BEG and END specify the beginning and end of the region."
  (interactive "*r")
  (markdown-block-region beg end "> "))

(defun markdown-insert-pre ()
  "Start a preformatted section (or apply to the region).
If Transient Mark mode is on and a region is active, it is marked
as preformatted text."
  (interactive)
  (if (markdown-use-region-p)
      (markdown-pre-region (region-beginning) (region-end))
    (insert "    ")))

(defun markdown-pre-region (beg end)
  "Format the region as preformatted text.
Arguments BEG and END specify the beginning and end of the region."
  (interactive "*r")
  (markdown-block-region beg end "    "))

(defun markdown-insert-gfm-code-block (&optional lang)
  "Insert GFM code block for language LANG.
If LANG is nil, the language will be queried from user.  If a
region is active, wrap this region with the markup instead.  If
the region boundaries are not on empty lines, these are added
automatically in order to have the correct markup."
  (interactive "sProgramming language: ")
  (if (markdown-use-region-p)
      (let ((b (region-beginning)) (e (region-end)))
        (goto-char b)
        ;; if we're on a blank line, insert the quotes here, otherwise
        ;; add a new line first
        (unless (looking-at "\n")
          (newline)
          (forward-line -1)
          (setq e (1+ e)))
        (insert "```" lang)
        (goto-char (+ e 3 (length lang)))
        ;; if we're on a blank line, don't newline, otherwise the ```
        ;; should go on its own line
        (unless (looking-back "\n")
          (newline))
        (insert "```"))
    (insert "```" lang)
    (newline 2)
    (insert "```")
    (forward-line -1)))

;;; Footnotes ======================================================================

(defun markdown-footnote-counter-inc ()
  "Increment `markdown-footnote-counter' and return the new value."
  (when (= markdown-footnote-counter 0) ; hasn't been updated in this buffer yet.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat "^\\[\\^\\(" markdown-footnote-chars "*?\\)\\]:")
                                (point-max) t)
        (let ((fn (string-to-number (match-string 1))))
          (when (> fn markdown-footnote-counter)
            (setq markdown-footnote-counter fn))))))
  (incf markdown-footnote-counter))

(defun markdown-footnote-new ()
  "Insert footnote with a new number and move point to footnote definition."
  (interactive)
  (let ((fn (markdown-footnote-counter-inc)))
    (insert (format "[^%d]" fn))
    (markdown-footnote-text-find-new-location)
    (insert (format "[^%d]: " fn))))

(defun markdown-footnote-text-find-new-location ()
  "Position the cursor at the proper location for a new footnote text."
  (cond
   ((eq markdown-footnote-location 'end) (goto-char (point-max)))
   ((eq markdown-footnote-location 'immediately) (forward-paragraph))
   ((eq markdown-footnote-location 'header)
    ;; search for a header. if none is found, go to the end of the document.
    (catch 'eof
      (while (progn
               (forward-paragraph)
               (unless (re-search-forward markdown-regex-header nil t)
                 (throw 'eof nil))
               (backward-paragraph)
               (forward-line)
               (not (looking-at markdown-regex-header)))))))
  (forward-line -1)
  ;; make sure we're on an empty line:
  (unless (markdown-cur-line-blank-p)
    (insert "\n"))
  ;; and make sure the previous line is empty:
  (unless (markdown-prev-line-blank-p)
    (insert "\n"))
  ;; then make sure there's an empty line following the footnote:
  (unless (markdown-next-line-blank-p)
    (insert "\n")
    (forward-line -1)))

(defun markdown-footnote-kill ()
  "Kill the footnote at point.
The footnote text is killed (and added to the kill ring), the
footnote marker is deleted.  Point has to be either at the
footnote marker or in the footnote text."
  (interactive)
  (let (return-pos)
    (when (markdown-footnote-text-positions) ; if we're in a footnote text
      (markdown-footnote-return) ; we first move to the marker
      (setq return-pos 'text)) ; and remember our return position
    (let ((marker (markdown-footnote-delete-marker)))
      (unless marker
        (error "Not at a footnote"))
      (let ((text-pos (markdown-footnote-find-text (car marker))))
        (unless text-pos
          (error "No text for footnote `%s'" (car marker)))
        (goto-char text-pos)
        (let ((pos (markdown-footnote-kill-text)))
          (setq return-pos
                (if (and pos (eq return-pos 'text))
                    pos
                  (cadr marker))))))
    (goto-char return-pos)))

(defun markdown-footnote-delete-marker ()
  "Delete a footnote marker at point.
Returns a list (ID START) containing the footnote ID and the
start position of the marker before deletion.  If no footnote
marker was deleted, this function returns NIL."
  (let ((marker (markdown-footnote-marker-positions)))
    (when marker
      (delete-region (second marker) (third marker))
      (butlast marker))))

(defun markdown-footnote-kill-text ()
  "Kill footnote text at point.
Returns the start position of the footnote text before deletion,
or NIL if point was not inside a footnote text.

The killed text is placed in the kill ring (without the footnote
number)."
  (let ((fn (markdown-footnote-text-positions)))
    (when fn
      (let ((text (delete-and-extract-region (second fn) (third fn))))
        (string-match (concat "\\[\\" (first fn) "\\]:[[:space:]]*\\(\\(.*\n?\\)*\\)") text)
        (kill-new (match-string 1 text))
        (second fn)))))

(defun markdown-footnote-goto-text ()
  "Jump to the text of the footnote at point."
  (interactive)
  (let ((fn (car (markdown-footnote-marker-positions))))
    (unless fn
      (error "Not at a footnote marker"))
    (let ((new-pos (markdown-footnote-find-text fn)))
      (unless new-pos
        (error "No definition found for footnote `%s'" fn))
      (goto-char new-pos))))

(defun markdown-footnote-return ()
  "Return from a footnote to its footnote number in the main text."
  (interactive)
  (let ((fn (save-excursion
              (car (markdown-footnote-text-positions)))))
    (unless fn
      (error "Not in a footnote"))
    (let ((new-pos (markdown-footnote-find-marker fn)))
      (unless new-pos
        (error "Footnote marker `%s' not found" fn))
      (goto-char new-pos))))

(defun markdown-footnote-find-marker (id)
  "Find the location of the footnote marker with ID.
The actual buffer position returned is the position directly
following the marker's closing bracket.  If no marker is found,
NIL is returned."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "\\[" id "\\]\\([^:]\\|\\'\\)") nil t)
      (skip-chars-backward "^]")
      (point))))

(defun markdown-footnote-find-text (id)
  "Find the location of the text of footnote ID.
The actual buffer position returned is the position of the first
character of the text, after the footnote's identifier.  If no
footnote text is found, NIL is returned."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^\\[" id "\\]:") nil t)
      (skip-chars-forward "[:space:]")
      (point))))

(defun markdown-footnote-marker-positions ()
  "Return the position and ID of the footnote marker point is on.
The return value is a list (ID START END).  If point is not on a
footnote, NIL is returned."
  ;; first make sure we're at a footnote marker
  (if (or (looking-back (concat "\\[\\^" markdown-footnote-chars "*\\]?") (point-at-bol))
          (looking-at (concat "\\[?\\^" markdown-footnote-chars "*?\\]")))
      (save-excursion
        ;; move point between [ and ^:
        (if (looking-at "\\[")
            (forward-char 1)
          (skip-chars-backward "^["))
        (looking-at (concat "\\(\\^" markdown-footnote-chars "*?\\)\\]"))
        (list (match-string 1) (1- (match-beginning 1)) (1+ (match-end 1))))))

(defun markdown-footnote-text-positions ()
  "Return the start and end positions of the footnote text point is in.
The exact return value is a list of three elements: (ID START END).
The start position is the position of the opening bracket
of the footnote id.  The end position is directly after the
newline that ends the footnote.  If point is not in a footnote,
NIL is returned instead."
  (save-excursion
    (let ((fn (progn
                (backward-paragraph)
                ;; if we're in a multiparagraph footnote, we need to back up further
                (while (>= (markdown-next-line-indent) 4)
                  (backward-paragraph))
                (forward-line)
                (if (looking-at (concat "^\\[\\(\\^" markdown-footnote-chars "*?\\)\\]:"))
                    (list (match-string 1) (point))))))
      (when fn
        (while (progn
                 (forward-paragraph)
                 (>= (markdown-next-line-indent) 4)))
        (append fn (list (point)))))))

;;; Indentation ====================================================================

(defun markdown-indent-find-next-position (cur-pos positions)
  "Return the position after the index of CUR-POS in POSITIONS."
  (while (and positions
              (not (equal cur-pos (car positions))))
    (setq positions (cdr positions)))
  (or (cadr positions) 0))

(defun markdown-indent-line ()
  "Indent the current line using some heuristics.
If the _previous_ command was either `markdown-enter-key' or
`markdown-cycle', then we should cycle to the next
reasonable indentation position.  Otherwise, we could have been
called directly by `markdown-enter-key', by an initial call of
`markdown-cycle', or indirectly by `auto-fill-mode'.  In
these cases, indent to the default position."
  (interactive)
  (let ((positions (markdown-calc-indents))
        (cur-pos (current-column)))
    (if (not (equal this-command 'markdown-cycle))
        (indent-line-to (car positions))
      (setq positions (sort (delete-dups positions) '<))
      (indent-line-to
       (markdown-indent-find-next-position cur-pos positions)))))

(defun markdown-calc-indents ()
  "Return a list of indentation columns to cycle through.
The first element in the returned list should be considered the
default indentation level."
  (let (pos prev-line-pos positions)

    ;; Previous line indent
    (setq prev-line-pos (markdown-prev-line-indent))
    (setq positions (cons prev-line-pos positions))

    ;; Previous non-list-marker indent
    (setq pos (markdown-prev-non-list-indent))
    (when pos
      (setq positions (cons pos positions))
      (setq positions (cons (+ pos tab-width) positions)))

    ;; Indentation of the previous line + tab-width
    (cond
     (prev-line-pos
      (setq positions (cons (+ prev-line-pos tab-width) positions)))
     (t
      (setq positions (cons tab-width positions))))

    ;; Indentation of the previous line - tab-width
    (if (and prev-line-pos
             (> prev-line-pos tab-width))
        (setq positions (cons (- prev-line-pos tab-width) positions)))

    ;; Indentation of preceeding list item
    (setq pos
          (save-excursion
            (forward-line -1)
            (catch 'break
              (while (not (equal (point) (point-min)))
                (forward-line -1)
                (goto-char (point-at-bol))
                (when (re-search-forward markdown-regex-list (point-at-eol) t)
                  (throw 'break (length (match-string 1)))))
              nil)))
    (if (and pos (not (eq pos prev-line-pos)))
        (setq positions (cons pos positions)))

    ;; First column
    (setq positions (cons 0 positions))

    (reverse positions)))

(defun markdown-do-normal-return ()
  "Insert a newline and optionally indent the next line."
  (newline)
  (if markdown-indent-on-enter
      (funcall indent-line-function)))

(defun markdown-enter-key ()
  "Handle RET according to context.
If there is a wiki link at the point, follow it unless
`markdown-follow-wiki-link-on-enter' is nil.  Otherwise, process
it in the usual way."
  (interactive)
  (if (and markdown-follow-wiki-link-on-enter (markdown-wiki-link-p))
      (markdown-follow-wiki-link-at-point)
    (markdown-do-normal-return)))

(defun markdown-dedent-or-delete (arg)
  "Handle BACKSPACE by cycling through indentation points.
When BACKSPACE is pressed, if there is only whitespace
before the current point, then dedent the line one level.
Otherwise, do normal delete by repeating
`backward-delete-char-untabify' ARG times."
  (interactive "*p")
  (let ((cur-pos (current-column))
        (start-of-indention (save-excursion
                              (back-to-indentation)
                              (current-column))))
    (if (and (> cur-pos 0) (= cur-pos start-of-indention))
        (let ((result 0))
          (dolist (i (markdown-calc-indents))
            (when (< i cur-pos)
              (setq result (max result i))))
          (indent-line-to result))
      (backward-delete-char-untabify arg))))



;;; Keymap ====================================================================

(defvar markdown-mode-map
  (let ((map (make-keymap)))
    ;; Element insertion
    (define-key map "\C-c\C-al" 'markdown-insert-link)
    (define-key map "\C-c\C-ar" 'markdown-insert-reference-link-dwim)
    (define-key map "\C-c\C-aw" 'markdown-insert-wiki-link)
    (define-key map "\C-c\C-ii" 'markdown-insert-image)
    (define-key map "\C-c\C-t0" 'markdown-remove-header)
    (define-key map "\C-c\C-t1" 'markdown-insert-header-atx-1)
    (define-key map "\C-c\C-t2" 'markdown-insert-header-atx-2)
    (define-key map "\C-c\C-t3" 'markdown-insert-header-atx-3)
    (define-key map "\C-c\C-t4" 'markdown-insert-header-atx-4)
    (define-key map "\C-c\C-t5" 'markdown-insert-header-atx-5)
    (define-key map "\C-c\C-t6" 'markdown-insert-header-atx-6)
    (define-key map "\C-c\C-pb" 'markdown-insert-bold)
    (define-key map "\C-c\C-ss" 'markdown-insert-bold)
    (define-key map "\C-c\C-pi" 'markdown-insert-italic)
    (define-key map "\C-c\C-se" 'markdown-insert-italic)
    (define-key map "\C-c\C-pf" 'markdown-insert-code)
    (define-key map "\C-c\C-sc" 'markdown-insert-code)
    (define-key map "\C-c\C-sb" 'markdown-insert-blockquote)
    (define-key map "\C-c\C-s\C-b" 'markdown-blockquote-region)
    (define-key map "\C-c\C-sp" 'markdown-insert-pre)
    (define-key map "\C-c\C-s\C-p" 'markdown-pre-region)
    (define-key map "\C-c-" 'markdown-insert-hr)
    (define-key map "\C-c\C-tt" 'markdown-insert-header-setext-1)
    (define-key map "\C-c\C-ts" 'markdown-insert-header-setext-2)
    ;; Footnotes
    (define-key map "\C-c\C-fn" 'markdown-footnote-new)
    (define-key map "\C-c\C-fg" 'markdown-footnote-goto-text)
    (define-key map "\C-c\C-fb" 'markdown-footnote-return)
    (define-key map "\C-c\C-fk" 'markdown-footnote-kill)
    ;; Regular Link Following
    (define-key map "\C-c\C-o" 'markdown-follow-link-at-point)
    ;; WikiLink Following
    (define-key map "\C-c\C-w" 'markdown-follow-wiki-link-at-point)
    (define-key map "\M-n" 'markdown-next-wiki-link)
    (define-key map "\M-p" 'markdown-previous-wiki-link)
    ;; Indentation
    (define-key map "\C-m" 'markdown-enter-key)
    (define-key map (kbd "<backspace>") 'markdown-dedent-or-delete)
    ;; Visibility cycling
    (define-key map (kbd "<tab>") 'markdown-cycle)
    (define-key map (kbd "<S-iso-lefttab>") 'markdown-shifttab)
    (define-key map (kbd "<S-tab>")  'markdown-shifttab)
    (define-key map (kbd "<backtab>") 'markdown-shifttab)
    ;; Header navigation
    (define-key map (kbd "C-M-n") 'outline-next-visible-heading)
    (define-key map (kbd "C-M-p") 'outline-previous-visible-heading)
    (define-key map (kbd "C-M-f") 'outline-forward-same-level)
    (define-key map (kbd "C-M-b") 'outline-backward-same-level)
    (define-key map (kbd "C-M-u") 'outline-up-heading)
    ;; Markdown functions
    (define-key map "\C-c\C-cm" 'markdown-other-window)
    (define-key map "\C-c\C-cp" 'markdown-preview)
    (define-key map "\C-c\C-ce" 'markdown-export)
    (define-key map "\C-c\C-cv" 'markdown-export-and-preview)
    (define-key map "\C-c\C-co" 'markdown-open)
    (define-key map "\C-c\C-cw" 'markdown-kill-ring-save)
    ;; References
    (define-key map "\C-c\C-cc" 'markdown-check-refs)
    ;; Lists
    (define-key map "\C-c\C-cn" 'markdown-cleanup-list-numbers)
    (define-key map (kbd "M-<up>") 'markdown-metaup)
    (define-key map (kbd "M-<down>") 'markdown-metadown)
    (define-key map (kbd "M-<left>") 'markdown-metaleft)
    (define-key map (kbd "M-<right>") 'markdown-metaright)
    (define-key map (kbd "C-c C-u") 'markdown-metaup)
    (define-key map (kbd "C-c C-d") 'markdown-metadown)
    (define-key map (kbd "C-c C-l") 'markdown-metaleft)
    (define-key map (kbd "C-c C-r") 'markdown-metaright)
    (define-key map (kbd "M-<return>") 'markdown-insert-list-item)
    map)
  "Keymap for Markdown major mode.")

(defvar gfm-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map markdown-mode-map)
    (define-key map "\C-c\C-sl" 'markdown-insert-gfm-code-block)
    map)
  "Keymap for `gfm-mode'.
See also `markdown-mode-map'.")

;;; Menu ==================================================================

(easy-menu-define markdown-mode-menu markdown-mode-map
  "Menu for Markdown mode"
  '("Markdown"
    ("Show/Hide"
     ["Cycle visibility" markdown-cycle (outline-on-heading-p)]
     ["Cycle global visibility" markdown-shifttab])
    "---"
    ["Compile" markdown-other-window]
    ["Preview" markdown-preview]
    ["Export" markdown-export]
    ["Export & View" markdown-export-and-preview]
    ["Open" markdown-open]
    ["Kill ring save" markdown-kill-ring-save]
    "---"
    ("Headers (setext)"
     ["First level" markdown-insert-setext-header-1]
     ["Second level" markdown-insert-setext-header-2])
    ("Headers (atx)"
     ["First level" markdown-insert-header-atx-1]
     ["Second level" markdown-insert-header-atx-2]
     ["Third level" markdown-insert-header-atx-3]
     ["Fourth level" markdown-insert-header-atx-4]
     ["Fifth level" markdown-insert-header-atx-5]
     ["Sixth level" markdown-insert-header-atx-6])
    "---"
    ["Bold" markdown-insert-bold]
    ["Italic" markdown-insert-italic]
    ["Blockquote" markdown-insert-blockquote]
    ["Preformatted" markdown-insert-pre]
    ["Code" markdown-insert-code]
    "---"
    ["Insert inline link" markdown-insert-link]
    ["Insert reference link" markdown-insert-reference-link-dwim]
    ["Insert image" markdown-insert-image]
    ["Insert horizontal rule" markdown-insert-hr]
    "---"
    ("Footnotes"
     ["Insert footnote" markdown-footnote-new]
     ["Jump to footnote text" markdown-footnote-goto-text]
     ["Return from footnote" markdown-footnote-return])
    "---"
    ["Check references" markdown-check-refs]
    ["Clean up list numbering" markdown-cleanup-list-numbers]
    "---"
    ["Version" markdown-show-version]
    ))



;;; imenu =====================================================================

(defun markdown-imenu-create-index ()
  "Create and return an imenu index alist for the current buffer.

See `imenu-create-index-function' and `imenu--index-alist' for details."
  (let* ((root '(nil . nil))
         cur-alist
         (cur-level 0)
         (empty-heading "-")
         (self-heading ".")
         hashes pos level heading)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward markdown-regex-header (point-max) t)
        (cond
         ((setq heading (match-string-no-properties 1))
          (setq pos (match-beginning 1)
                level 1))
         ((setq heading (match-string-no-properties 3))
          (setq pos (match-beginning 3)
                level 2))
         ((setq hashes (match-string-no-properties 5))
          (setq heading (match-string-no-properties 6)
                pos (match-beginning 5)
                level (length hashes))))
        (let ((alist (list (cons heading pos))))
          (cond
           ((= cur-level level)         ; new sibling
            (setcdr cur-alist alist)
            (setq cur-alist alist))
           ((< cur-level level)         ; first child
            (dotimes (i (- level cur-level 1))
              (setq alist (list (cons empty-heading alist))))
            (if cur-alist
                (let* ((parent (car cur-alist))
                       (self-pos (cdr parent)))
                  (setcdr parent (cons (cons self-heading self-pos) alist)))
              (setcdr root alist))      ; primogenitor
            (setq cur-alist alist)
            (setq cur-level level))
           (t                           ; new sibling of an ancestor
            (let ((sibling-alist (last (cdr root))))
              (dotimes (i (1- level))
                (setq sibling-alist (last (cdar sibling-alist))))
              (setcdr sibling-alist alist)
              (setq cur-alist alist))
            (setq cur-level level)))))
      (cdr root))))

;;; References ================================================================

;;; Undefined reference checking code by Dmitry Dzhus <mail@sphinx.net.ru>.

(defconst markdown-refcheck-buffer
  "*Undefined references for %buffer%*"
  "Pattern for name of buffer for listing undefined references.
The string %buffer% will be replaced by the corresponding
`markdown-mode' buffer name.")

(defun markdown-has-reference-definition (reference)
  "Find out whether Markdown REFERENCE is defined.

REFERENCE should include the square brackets, like [this]."
  (let ((reference (downcase reference)))
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward markdown-regex-reference-definition nil t)
          (when (string= reference (downcase (match-string-no-properties 1)))
            (throw 'found (match-string-no-properties 2))))))))

(defun markdown-get-undefined-refs ()
  "Return a list of undefined Markdown references.

Result is an alist of pairs (reference . occurencies), where
occurencies is itself another alist of pairs (label .
line-number).

For example, an alist corresponding to [Nice editor][Emacs] at line 12,
\[GNU Emacs][Emacs] at line 45 and [manual][elisp] at line 127 is
\((\"[emacs]\" (\"[Nice editor]\" . 12) (\"[GNU Emacs]\" . 45)) (\"[elisp]\" (\"[manual]\" . 127)))."
  (let ((missing))
    (save-excursion
      (goto-char (point-min))
      (while
          (re-search-forward markdown-regex-link-reference nil t)
        (let* ((label (match-string-no-properties 1))
               (reference (match-string-no-properties 2))
               (target (downcase (if (string= reference "[]") label reference))))
          (unless (markdown-has-reference-definition target)
            (let ((entry (assoc target missing)))
              (if (not entry)
                  (add-to-list 'missing (cons target
                                              (list (cons label (markdown-line-number-at-pos)))) t)
                (setcdr entry
                        (append (cdr entry) (list (cons label (markdown-line-number-at-pos))))))))))
      missing)))

(defun markdown-add-missing-ref-definition (ref buffer &optional recheck)
  "Add blank REF definition to the end of BUFFER.

REF is a Markdown reference in square brackets, like \"[lisp-history]\".

When RECHECK is non-nil, BUFFER gets rechecked for undefined
references so that REF disappears from the list of those links."
  (with-current-buffer buffer
    (when (not (eq major-mode 'markdown-mode))
      (error "Not available in current mode"))
    (goto-char (point-max))
    (indent-new-comment-line)
    (insert (concat ref ": ")))
  (switch-to-buffer-other-window buffer)
  (goto-char (point-max))
  (when recheck
    (markdown-check-refs t)))

;; Button which adds an empty Markdown reference definition to the end
;; of buffer specified as its 'target-buffer property. Reference name
;; is button's label
(when (>= emacs-major-version 22)
  (define-button-type 'markdown-ref-button
    'help-echo "Push to create an empty reference definition"
    'face 'bold
    'action (lambda (b)
              (markdown-add-missing-ref-definition
               (button-label b) (button-get b 'target-buffer) t))))

;; Button jumping to line in buffer specified as its 'target-buffer
;; property. Line number is button's 'line property.
(when (>= emacs-major-version 22)
  (define-button-type 'goto-line-button
    'help-echo "Push to go to this line"
    'face 'italic
    'action (lambda (b)
              (message (button-get b 'buffer))
              (switch-to-buffer-other-window (button-get b 'target-buffer))
              ;; use call-interactively to silence compiler
              (call-interactively 'goto-line (button-get b 'target-line)))))

(defun markdown-check-refs (&optional silent)
  "Show all undefined Markdown references in current `markdown-mode' buffer.

If SILENT is non-nil, do not message anything when no undefined
references found.

Links which have empty reference definitions are considered to be
defined."
  (interactive "P")
  (when (not (eq major-mode 'markdown-mode))
    (error "Not available in current mode"))
  (let ((oldbuf (current-buffer))
        (refs (markdown-get-undefined-refs))
        (refbuf (get-buffer-create (markdown-replace-regexp-in-string
                                 "%buffer%" (buffer-name)
                                 markdown-refcheck-buffer))))
    (if (null refs)
        (progn
          (when (not silent)
            (message "No undefined references found"))
          (kill-buffer refbuf))
      (with-current-buffer refbuf
        (when view-mode
          (View-exit-and-edit))
        (erase-buffer)
        (insert "Following references lack definitions:")
        (newline 2)
        (dolist (ref refs)
          (let ((button-label (format "%s" (car ref))))
            (if (>= emacs-major-version 22)
                ;; Create a reference button in Emacs 22
                (insert-text-button button-label
                                    :type 'markdown-ref-button
                                    'target-buffer oldbuf)
              ;; Insert reference as text in Emacs < 22
              (insert button-label)))
          (insert " (")
          (dolist (occurency (cdr ref))
            (let ((line (cdr occurency)))
              (if (>= emacs-major-version 22)
                  ;; Create a line number button in Emacs 22
                  (insert-button (number-to-string line)
                                 :type 'goto-line-button
                                 'target-buffer oldbuf
                                 'target-line line)
                ;; Insert line number as text in Emacs < 22
                (insert (number-to-string line)))
              (insert " "))) (delete-char -1)
          (insert ")")
          (newline))
        (view-buffer-other-window refbuf)
        (goto-char (point-min))
        (forward-line 2)))))


;;; Lists =====================================================================

(defun markdown-insert-list-item (&optional arg)
  "Insert a new list item.
If the point is inside unordered list, insert a bullet mark.  If
the point is inside ordered list, insert the next number followed
by a period.  Use the previous list item to determine the amount
of whitespace to place before and after list markers.

With a \\[universal-argument] prefix (i.e., when ARG is 4),
decrease the indentation by one level.

With two \\[universal-argument] prefixes (i.e., when ARG is 16),
increase the indentation by one level."
  (interactive "p")
  (let (bounds item-indent marker indent new-indent end)
    (save-match-data
      (setq bounds (markdown-cur-list-item-bounds))
      (if (not bounds)
          ;; When not in a list, start a new unordered one
          (progn
            (unless (markdown-cur-line-blank-p)
              (insert "\n"))
            (insert "* "))
        ;; Compute indentation for a new list item
        (setq item-indent (nth 2 bounds))
        (setq marker (concat (match-string 2) (match-string 3)))
        (setq indent (cond
                      ((= arg 4) (max (- item-indent 4) 0))
                      ((= arg 16) (+ item-indent 4))
                      (t item-indent)))
        (setq new-indent (make-string indent 32))
        (goto-char (nth 1 bounds))
        (newline)
        (cond
         ;; Ordered list
         ((string-match "[0-9]" marker)
          (if (= arg 16) ;; starting a new column indented one more level
              (insert (concat new-indent "1. "))
            ;; travel up to the last item and pick the correct number.  If
            ;; the argument was nil, "new-indent = item-indent" is the same,
            ;; so we don't need special treatment. Neat.
            (save-excursion
              (while (not (looking-at (concat new-indent "\\([0-9]+\\)\\.")))
                (forward-line -1)))
            (insert (concat new-indent
                            (int-to-string (1+ (string-to-number (match-string 1))))
                            ". "))))
         ;; Unordered list
         ((string-match "[\\*\\+-]" marker)
          (insert (concat new-indent marker))))))))

(defun markdown-move-list-item-up ()
  "Move the current list item up in the list when possible."
  (interactive)
  (let (cur prev old)
    (when (setq cur (markdown-cur-list-item-bounds))
      (setq old (point))
      (goto-char (nth 0 cur))
      (if (markdown-prev-list-item (nth 3 cur))
          (progn
            (setq prev (markdown-cur-list-item-bounds))
            (condition-case nil
                (progn
                  (transpose-regions (nth 0 prev) (nth 1 prev)
                                     (nth 0 cur) (nth 1 cur) t)
                  (goto-char (+ (nth 0 prev) (- old (nth 0 cur)))))
              ;; Catch error in case regions overlap.
              (error (goto-char old))))
        (goto-char old)))))

(defun markdown-move-list-item-down ()
  "Move the current list item down in the list when possible."
  (interactive)
  (let (cur next old)
    (when (setq cur (markdown-cur-list-item-bounds))
      (setq old (point))
      (if (markdown-next-list-item (nth 3 cur))
          (progn
            (setq next (markdown-cur-list-item-bounds))
            (condition-case nil
                (progn
                  (transpose-regions (nth 0 cur) (nth 1 cur)
                                     (nth 0 next) (nth 1 next) nil)
                  (goto-char (+ old (- (nth 1 next) (nth 1 cur)))))
              ;; Catch error in case regions overlap.
              (error (goto-char old))))
        (goto-char old)))))

(defun markdown-promote-list-item ()
  "Indent the current list item."
  (interactive)
  (let ((bounds (markdown-cur-list-item-bounds))
        (ins 0))
    (when bounds
      (save-excursion
        (save-match-data
          (goto-char (nth 0 bounds))
          (while (< (point) (+ (nth 1 bounds) ins))
            (unless (markdown-cur-line-blank-p)
              (insert "    ")
              (setq ins (+ ins 4)))
            (forward-line)))))))

(defun markdown-demote-list-item ()
  "Unindent the current list item."
  (interactive)
  (let (bounds num del regexp)
    (when (setq bounds (markdown-cur-list-item-bounds))
      (save-excursion
        (save-match-data
          (goto-char (nth 0 bounds))
          (setq del 0)
          (when (looking-at "^[ ]\\{1,4\\}")
            (setq num (- (match-end 0) (match-beginning 0)))
            (setq regexp (format "^[ ]\\{1,%d\\}" num))
            (while (re-search-forward regexp (- (nth 1 bounds) del) t)
              (replace-match "" nil nil)
              (setq num (- (match-end 0) (match-beginning 0)))
              (setq del (+ del num))
              (forward-line))))))))

(defun markdown--cleanup-list-numbers-level (&optional pfx)
  "Update the numbering for level PFX (as a string of spaces).

Assume that the previously found match was for a numbered item in
a list."
  (let ((cpfx pfx)
        (idx 0)
        (continue t)
        (step t)
        (sep nil))
    (while (and continue (not (eobp)))
      (setq step t)
      (cond
       ((looking-at "^\\([\s-]*\\)[0-9]+\\. ")
        (setq cpfx (match-string-no-properties 1))
        (cond
         ((string= cpfx pfx)
          (replace-match
           (concat pfx (number-to-string  (setq idx (1+ idx))) ". "))
          (setq sep nil))
         ;; indented a level
         ((string< pfx cpfx)
          (setq sep (markdown--cleanup-list-numbers-level cpfx))
          (setq step nil))
         ;; exit the loop
         (t
          (setq step nil)
          (setq continue nil))))

       ((looking-at "^\\([\s-]*\\)[^ \t\n\r].*$")
        (setq cpfx (match-string-no-properties 1))
        (cond
         ;; reset if separated before
         ((string= cpfx pfx) (when sep (setq idx 0)))
         ((string< cpfx pfx)
          (setq step nil)
          (setq continue nil))))
       (t (setq sep t)))

      (when step
        (beginning-of-line)
        (setq continue (= (forward-line) 0))))
    sep))

(defun markdown-cleanup-list-numbers ()
  "Update the numbering of ordered lists."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (markdown--cleanup-list-numbers-level "")))


;;; Outline ===================================================================

;; The following visibility cycling code was taken from org-mode
;; by Carsten Dominik and adapted for markdown-mode.

(defvar markdown-cycle-global-status 1)
(defvar markdown-cycle-subtree-status nil)

;; Based on org-end-of-subtree from org.el
(defun markdown-end-of-subtree (&optional invisible-OK)
  "Move to the end of the current subtree.
Only visible heading lines are considered, unless INVISIBLE-OK is
non-nil."
  (outline-back-to-heading invisible-OK)
  (let ((first t)
        (level (funcall outline-level)))
    (while (and (not (eobp))
                (or first (> (funcall outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (if (memq (preceding-char) '(?\n ?\^M))
        (progn
          ;; Go to end of line before heading
          (forward-char -1)
          (if (memq (preceding-char) '(?\n ?\^M))
              ;; leave blank line before heading
              (forward-char -1)))))
  (point))

;; Based on org-cycle from org.el.
(defun markdown-cycle (&optional arg)
  "Visibility cycling for Markdown mode.
If ARG is t, perform global visibility cycling.  If the point is
at an atx-style header, cycle visibility of the corresponding
subtree.  Otherwise, insert a tab using `indent-relative'."
  (interactive "P")
  (cond
   ((eq arg t) ;; Global cycling
    (cond
     ((and (eq last-command this-command)
           (eq markdown-cycle-global-status 2))
      ;; Move from overview to contents
      (hide-sublevels 1)
      (message "CONTENTS")
      (setq markdown-cycle-global-status 3))

     ((and (eq last-command this-command)
           (eq markdown-cycle-global-status 3))
      ;; Move from contents to all
      (show-all)
      (message "SHOW ALL")
      (setq markdown-cycle-global-status 1))

     (t
      ;; Defaults to overview
      (hide-body)
      (message "OVERVIEW")
      (setq markdown-cycle-global-status 2))))

   ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
    ;; At a heading: rotate between three different views
    (outline-back-to-heading)
    (let ((goal-column 0) eoh eol eos)
      ;; Determine boundaries
      (save-excursion
        (outline-back-to-heading)
        (save-excursion
          (beginning-of-line 2)
          (while (and (not (eobp)) ;; this is like `next-line'
                      (get-char-property (1- (point)) 'invisible))
            (beginning-of-line 2)) (setq eol (point)))
        (outline-end-of-heading)   (setq eoh (point))
        (markdown-end-of-subtree t)
        (skip-chars-forward " \t\n")
        (beginning-of-line 1) ; in case this is an item
        (setq eos (1- (point))))
      ;; Find out what to do next and set `this-command'
      (cond
       ((= eos eoh)
        ;; Nothing is hidden behind this heading
        (message "EMPTY ENTRY")
        (setq markdown-cycle-subtree-status nil))
       ((>= eol eos)
        ;; Entire subtree is hidden in one line: open it
        (show-entry)
        (show-children)
        (message "CHILDREN")
        (setq markdown-cycle-subtree-status 'children))
       ((and (eq last-command this-command)
             (eq markdown-cycle-subtree-status 'children))
        ;; We just showed the children, now show everything.
        (show-subtree)
        (message "SUBTREE")
        (setq markdown-cycle-subtree-status 'subtree))
       (t
        ;; Default action: hide the subtree.
        (hide-subtree)
        (message "FOLDED")
        (setq markdown-cycle-subtree-status 'folded)))))

   (t
    (indent-for-tab-command))))

;; Based on org-shifttab from org.el.
(defun markdown-shifttab ()
  "Global visibility cycling.
Calls `markdown-cycle' with argument t."
  (interactive)
  (markdown-cycle t))

(defun markdown-outline-level ()
  "Return the depth to which a statement is nested in the outline."
  (cond
   ((match-end 1) 1)
   ((match-end 3) 2)
   ((- (match-end 5) (match-beginning 5)))))

;;; Structure Editing =========================================================

(defun markdown-metaup ()
  "Move list item up.
Calls `markdown-move-list-item-up'."
  (interactive)
  (markdown-move-list-item-up))

(defun markdown-metadown ()
  "Move list item down.
Calls `markdown-move-list-item-down'."
  (interactive)
  (markdown-move-list-item-down))

(defun markdown-metaleft ()
  "Unindent list item.
Calls `markdown-demote-list-item'."
  (interactive)
  (markdown-demote-list-item))

(defun markdown-metaright ()
  "Indent list item.
Calls `markdown-promote-list-item'."
  (interactive)
  (markdown-promote-list-item))

;;; Commands ==================================================================

(defun markdown (&optional output-buffer-name)
  "Run `markdown-command' on buffer, sending output to OUTPUT-BUFFER-NAME.
The output buffer name defaults to `markdown-output-buffer-name'.
Return the name of the output buffer used."
  (interactive)
  (save-window-excursion
    (let ((begin-region)
          (end-region))
      (if (markdown-use-region-p)
          (setq begin-region (region-beginning)
                end-region (region-end))
        (setq begin-region (point-min)
              end-region (point-max)))

      (unless output-buffer-name
        (setq output-buffer-name markdown-output-buffer-name))

      (cond
       ;; Handle case when `markdown-command' does not read from stdin
       (markdown-command-needs-filename
        (if (not buffer-file-name)
            (error "Must be visiting a file")
          (shell-command (concat markdown-command " "
                                 (shell-quote-argument buffer-file-name))
                         output-buffer-name)))
       ;; Pass region to `markdown-command' via stdin
       (t
        (shell-command-on-region begin-region end-region markdown-command
                                 output-buffer-name))))
    output-buffer-name))

(defun markdown-standalone (&optional output-buffer-name)
  "Special function to provide standalone HTML output.
Insert the output in the buffer named OUTPUT-BUFFER-NAME."
  (interactive)
  (setq output-buffer-name (markdown output-buffer-name))
  (with-current-buffer output-buffer-name
    (set-buffer output-buffer-name)
    (goto-char (point-min))
    (unless (markdown-output-standalone-p)
      (markdown-add-xhtml-header-and-footer output-buffer-name))
    (html-mode))
  output-buffer-name)

(defun markdown-other-window (&optional output-buffer-name)
  "Run `markdown-command' on current buffer and display in other window.
When OUTPUT-BUFFER-NAME is given, insert the output in the buffer with
that name."
  (interactive)
  (display-buffer (markdown-standalone output-buffer-name)))

(defun markdown-output-standalone-p ()
  "Determine whether `markdown-command' output is standalone XHTML.
Standalone XHTML output is identified by an occurrence of
`markdown-xhtml-standalone-regexp' in the first five lines of output."
  (re-search-forward
   markdown-xhtml-standalone-regexp
   (save-excursion (goto-char (point-min)) (forward-line 4) (point))
   t))

(defun markdown-add-xhtml-header-and-footer (title)
  "Wrap XHTML header and footer with given TITLE around current buffer."
  (insert "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
          "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
          "\t\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\n"
          "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n\n"
          "<head>\n<title>")
  (insert title)
  (insert "</title>\n")
  (when (> (length markdown-content-type) 0)
    (insert
     (format
      "<meta http-equiv=\"Content-Type\" content=\"%s;charset=%s\"/>\n"
      markdown-content-type
      (or (and markdown-coding-system
               (fboundp 'coding-system-get)
               (coding-system-get markdown-coding-system
                                  'mime-charset))
          (and (fboundp 'coding-system-get)
               (coding-system-get buffer-file-coding-system
                                  'mime-charset))
          "iso-8859-1"))))
  (if (> (length markdown-css-path) 0)
      (insert "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\""
              markdown-css-path
              "\"  />\n"))
  (when (> (length markdown-xhtml-header-content) 0)
    (insert markdown-xhtml-header-content))
  (insert "\n</head>\n\n"
          "<body>\n\n")
  (goto-char (point-max))
  (insert "\n"
          "</body>\n"
          "</html>\n"))

(defun markdown-preview (&optional output-buffer-name)
  "Run `markdown-command' on the current buffer and view output in browser.
When OUTPUT-BUFFER-NAME is given, insert the output in the buffer with
that name."
  (interactive)
  (browse-url-of-buffer (markdown markdown-output-buffer-name)))

(defun markdown-export-file-name (&optional extension)
  "Attempt to generate a filename for Markdown output.
The file extension will be EXTENSION if given, or .html by default.
If the current buffer is visiting a file, we construct a new
output filename based on that filename.  Otherwise, return nil."
  (when (buffer-file-name)
    (unless extension
      (setq extension ".html"))
    (concat
     (cond
      ((buffer-file-name)
       (file-name-sans-extension (buffer-file-name)))
      (t (buffer-name)))
     extension)))

(defun markdown-export (&optional output-file)
  "Run Markdown on the current buffer, save to file, and return the filename.
If OUTPUT-FILE is given, use that as the filename.  Otherwise, use the filename
generated by `markdown-export-file-name', which will be constructed using the
current filename, but with the extension removed and replaced with .html."
  (interactive)
  (unless output-file
    (setq output-file (markdown-export-file-name ".html")))
  (when output-file
    (let ((output-buffer-name))
      (setq output-buffer-name (buffer-name (find-file-noselect output-file)))
      (run-hook-with-args 'markdown-before-export-hooks output-file)
      (markdown-standalone output-buffer-name)
      (with-current-buffer output-buffer-name
        (save-buffer))
      (run-hook-with-args 'markdown-after-export-hooks output-file)
      output-file)))

(defun markdown-export-and-preview ()
  "Export to XHTML using `markdown-export' and browse the resulting file."
  (interactive)
  (browse-url (markdown-export)))

(defun markdown-open ()
  "Open file for the current buffer with `markdown-open-command'."
  (interactive)
  (if (not markdown-open-command)
      (error "Variable `markdown-open-command' must be set")
    (if (not buffer-file-name)
        (error "Must be visiting a file")
      (call-process markdown-open-command
                    nil nil nil buffer-file-name))))

(defun markdown-kill-ring-save ()
  "Run Markdown on file and store output in the kill ring."
  (interactive)
  (save-window-excursion
    (markdown)
    (with-current-buffer markdown-output-buffer-name
      (kill-ring-save (point-min) (point-max)))))


;;; Links =====================================================================

(require 'thingatpt)

(defun markdown-link-p ()
  "Return non-nil when `point' is at a non-wiki link.
See `markdown-wiki-link-p' for more information."
  (let ((case-fold-search nil))
    (and (not (markdown-wiki-link-p))
         (or (thing-at-point-looking-at markdown-regex-link-inline)
             (thing-at-point-looking-at markdown-regex-link-reference)
             (thing-at-point-looking-at markdown-regex-uri)
             (thing-at-point-looking-at markdown-regex-angle-uri)))))

(defun markdown-link-link ()
  "Return the link part of the regular (non-wiki) link at point.
Works with both inline and reference style links.  If point is
not at a link or the link reference is not defined returns nil."
  (cond
   ((thing-at-point-looking-at markdown-regex-link-inline)
    (substring-no-properties (match-string 2) 1 -1))
   ((thing-at-point-looking-at markdown-regex-link-reference)
    (let* ((label (match-string-no-properties 1))
           (reference (match-string-no-properties 2))
           (target (downcase (if (string= reference "[]") label reference))))
      (markdown-has-reference-definition target)))
   ((thing-at-point-looking-at markdown-regex-uri)
    (match-string-no-properties 0))
   ((thing-at-point-looking-at markdown-regex-angle-uri)
    (match-string-no-properties 2))
   (t nil)))

(defun markdown-follow-link-at-point ()
  "Open the current non-wiki link in a browser."
  (interactive)
  (if (markdown-link-p) (browse-url (markdown-link-link))
    (error "Point is not at a Markdown link or URI")))


;;; WikiLink Following/Markup =================================================

(defun markdown-wiki-link-p ()
  "Return non-nil when `point' is at a true wiki link.
A true wiki link name matches `markdown-regex-wiki-link' but does not
match the current file name after conversion.  This modifies the data
returned by `match-data'.  Note that the potential wiki link name must
be available via `match-string'."
  (let ((case-fold-search nil))
    (and (thing-at-point-looking-at markdown-regex-wiki-link)
         (or (not buffer-file-name)
             (not (string-equal (buffer-file-name)
                                (markdown-convert-wiki-link-to-filename
                                 (markdown-wiki-link-link)))))
         (not (save-match-data
                (save-excursion))))))

(defun markdown-wiki-link-link ()
  "Return the link part of the wiki link using current match data.
The location of the link component depends on the value of
`markdown-wiki-link-alias-first'."
  (if markdown-wiki-link-alias-first
      (or (match-string-no-properties 4) (match-string-no-properties 2))
    (match-string-no-properties 2)))

(defun markdown-convert-wiki-link-to-filename (name)
  "Generate a filename from the wiki link NAME.
Spaces in NAME are replaced with `markdown-link-space-sub-char'.
When in `gfm-mode', follow GitHub's conventions where [[Test Test]]
and [[test test]] both map to Test-test.ext."
  (let ((basename (markdown-replace-regexp-in-string
                   "[[:space:]\n]" markdown-link-space-sub-char name)))
    (when (eq major-mode 'gfm-mode)
      (setq basename (concat (upcase (substring basename 0 1))
                             (downcase (substring basename 1 nil)))))
    (concat basename
            (if (buffer-file-name)
                (concat "."
                        (file-name-extension (buffer-file-name)))))))

(defun markdown-follow-wiki-link (name &optional other)
  "Follow the wiki link NAME.
Convert the name to a file name and call `find-file'.  Ensure that
the new buffer remains in `markdown-mode'.  Open the link in another
window when OTHER is non-nil."
  (let ((filename (markdown-convert-wiki-link-to-filename name))
        (wp (file-name-directory buffer-file-name)))
    (when other (other-window 1))
    (find-file (concat wp filename)))
  (when (not (eq major-mode 'markdown-mode))
    (markdown-mode)))

(defun markdown-follow-wiki-link-at-point (&optional arg)
  "Find Wiki Link at point.
With prefix argument ARG, open the file in other window.

See `markdown-wiki-link-p' and `markdown-follow-wiki-link'."
  (interactive "P")
  (if (markdown-wiki-link-p)
      (markdown-follow-wiki-link (markdown-wiki-link-link) arg)
    (error "Point is not at a Wiki Link")))

(defun markdown-next-wiki-link ()
  "Jump to next wiki link.
If successful, return point.  Otherwise, return nil.
See `markdown-wiki-link-p' and `markdown-previous-wiki-link'."
  (interactive)
  (let ((opoint (point)))
    (when (markdown-wiki-link-p)
      ;; At a wiki link already, move past it.
      (goto-char (+ (match-end 0) 1)))
      ;; Search for the next wiki link and move to the beginning.
    (if (re-search-forward markdown-regex-wiki-link nil t)
        (goto-char (match-beginning 1))
      (goto-char opoint)
      nil)))

(defun markdown-previous-wiki-link ()
  "Jump to previous wiki link.
If successful, return point.  Otherwise, return nil.
See `markdown-wiki-link-p' and `markdown-next-wiki-link'."
  (interactive)
  (if (re-search-backward markdown-regex-wiki-link nil t)
      (goto-char (match-beginning 1))
    nil))

(defun markdown-highlight-wiki-link (from to face)
  "Highlight the wiki link in the region between FROM and TO using FACE."
  (put-text-property from to 'font-lock-face face))

(defun markdown-unfontify-region-wiki-links (from to)
  "Remove wiki link faces from the region specified by FROM and TO."
  (interactive "nfrom: \nnto: ")
  (remove-text-properties from to '(font-lock-face markdown-link-face))
  (remove-text-properties from to '(font-lock-face markdown-missing-link-face)))

(defun markdown-fontify-region-wiki-links (from to)
  "Search region given by FROM and TO for wiki links and fontify them.
If a wiki link is found check to see if the backing file exists
and highlight accordingly."
  (goto-char from)
  (save-match-data
    (while (re-search-forward markdown-regex-wiki-link to t)
      (let ((highlight-beginning (match-beginning 1))
            (highlight-end (match-end 1))
            (file-name
             (markdown-convert-wiki-link-to-filename
              (markdown-wiki-link-link))))
        (if (file-exists-p file-name)
            (markdown-highlight-wiki-link
             highlight-beginning highlight-end markdown-link-face)
          (markdown-highlight-wiki-link
           highlight-beginning highlight-end markdown-link-face)
          (markdown-highlight-wiki-link
           highlight-beginning highlight-end markdown-missing-link-face))))))

(defun markdown-extend-changed-region (from to)
  "Extend region given by FROM and TO so that we can fontify all links.
The region is extended to the first newline before and the first
newline after."
  ;; start looking for the first new line before 'from
  (goto-char from)
  (re-search-backward "\n" nil t)
  (let ((new-from (point-min))
        (new-to (point-max)))
    (if (not (= (point) from))
        (setq new-from (point)))
    ;; do the same thing for the first new line after 'to
    (goto-char to)
    (re-search-forward "\n" nil t)
    (if (not (= (point) to))
        (setq new-to (point)))
    (values new-from new-to)))

(defun markdown-check-change-for-wiki-link (from to change)
  "Check region between FROM and TO for wiki links and re-fontfy as needed.
Designed to be used with the `after-change-functions' hook.
CHANGE is the number of bytes of pre-change text replaced by the
given range."
  (interactive "nfrom: \nnto: \nnchange: ")
  (let* ((modified (buffer-modified-p))
         (buffer-undo-list t)
         (inhibit-read-only t)
         (inhibit-point-motion-hooks t)
         deactivate-mark
         buffer-file-truename)
     (unwind-protect
         (save-excursion
           (save-match-data
             (save-restriction
               ;; Extend the region to fontify so that it starts
               ;; and ends at safe places.
               (multiple-value-bind (new-from new-to)
                   (markdown-extend-changed-region from to)
                 ;; Unfontify existing fontification (start from scratch)
                 (markdown-unfontify-region-wiki-links new-from new-to)
                 ;; Now do the fontification.
                 (markdown-fontify-region-wiki-links new-from new-to)))))
       (and (not modified)
            (buffer-modified-p)
            (set-buffer-modified-p nil)))))

(defun markdown-fontify-buffer-wiki-links ()
  "Refontify all wiki links in the buffer."
  (interactive)
  (markdown-check-change-for-wiki-link (point-min) (point-max) 0))

;;; Miscellaneous =============================================================

(defun markdown-compress-whitespace-string (str)
  "Compress whitespace in STR and return result.
Leading and trailing whitespace is removed.  Sequences of multiple
spaces, tabs, and newlines are replaced with single spaces."
  (replace-regexp-in-string "\\(^[ \t\n]+\\|[ \t\n]+$\\)" ""
                            (replace-regexp-in-string "[ \t\n]+" " " str)))

(defun markdown-line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location.
This is an exact copy of `line-number-at-pos' for use in emacs21."
  (let ((opoint (or pos (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char opoint)
      (forward-line 0)
      (1+ (count-lines start (point))))))

(defun markdown-nobreak-p ()
  "Return nil if it is acceptable to break the current line at the point."
  ;; inside in square brackets (e.g., link anchor text)
  (looking-back "\\[[^]]*"))



;;; Mode definition  ==========================================================

(defun markdown-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "markdown-mode, version %s" markdown-mode-version))

;;;###autoload
(define-derived-mode markdown-mode text-mode "Markdown"
  "Major mode for editing Markdown files."
  ;; Natural Markdown tab width
  (setq tab-width 4)
  ;; Comments
  (make-local-variable 'comment-start)
  (setq comment-start "<!-- ")
  (make-local-variable 'comment-end)
  (setq comment-end " -->")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "<!--[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 0)
  ;; Font lock.
  (set (make-local-variable 'font-lock-defaults)
       '(markdown-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t)
  ;; For imenu support
  (setq imenu-create-index-function 'markdown-imenu-create-index)
  ;; For menu support in XEmacs
  (easy-menu-add markdown-mode-menu markdown-mode-map)
  ;; Make filling work with lists (unordered, ordered, and definition)
  (set (make-local-variable 'paragraph-start)
       "\f\\|[ \t]*$\\|^[ \t]*[*+-] \\|^[ \t]*[0-9]+\\.\\|^[ \t]*: ")
  ;; Outline mode
  (make-local-variable 'outline-regexp)
  (setq outline-regexp markdown-regex-header)
  (make-local-variable 'outline-level)
  (setq outline-level 'markdown-outline-level)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))
  ;; Indentation and filling
  (make-local-variable 'fill-nobreak-predicate)
  (add-hook 'fill-nobreak-predicate 'markdown-nobreak-p)
  (setq indent-line-function markdown-indent-function)

  ;; Prepare hooks for XEmacs compatibility
  (when (featurep 'xemacs)
    (make-local-hook 'after-change-functions)
    (make-local-hook 'font-lock-extend-region-functions)
    (make-local-hook 'window-configuration-change-hook))

  ;; Multiline font lock
  (add-hook 'font-lock-extend-region-functions
            'markdown-font-lock-extend-region)

  ;; Anytime text changes make sure it gets fontified correctly
  (add-hook 'after-change-functions 'markdown-check-change-for-wiki-link t t)

  ;; If we left the buffer there is a really good chance we were
  ;; creating one of the wiki link documents. Make sure we get
  ;; refontified when we come back.
  (add-hook 'window-configuration-change-hook
            'markdown-fontify-buffer-wiki-links t t)

  ;; do the initial link fontification
  (markdown-fontify-buffer-wiki-links))

;;(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))

;;; GitHub Flavored Markdown Mode  ============================================

;;;###autoload
(define-derived-mode gfm-mode markdown-mode "GFM"
  "Major mode for editing GitHub Flavored Markdown files."
  (setq markdown-link-space-sub-char "-")
  (auto-fill-mode 0)
  ;; Use visual-line-mode if available, fall back to longlines-mode:
  (if (fboundp 'visual-line-mode)
      (visual-line-mode 1)
    (longlines-mode 1))
  ;; do the initial link fontification
  (markdown-fontify-buffer-wiki-links))


(provide 'markdown-mode)

;;; markdown-mode.el ends here
