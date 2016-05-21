;;; init.el --- My Emacs settings

;;; Commentary:

;; Author: Kumpei Shiraishi <kumpeishiraishi@gmail.com>
;; Created: around 2014
;; Last modified: 2016-05-21

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; パッケージ管理
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))
(defvar myPackages
  '(ace-isearch
    auto-complete
    avy
    exec-path-from-shell
    flycheck
    haskell-mode
    helm
    helm-swoop
    markdown-mode
    migemo
    org
    yatex))
(dolist (package myPackages)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 設定
(setq make-backup-files nil)
(fset 'yes-or-no-p 'y-or-n-p)
;; 言語環境・文字コード
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
;; emacs-24.5-inline.patchを当ててHomebrewからインストールして可能になった、日本語関係の設定（起動時、ミニバッファ、isearch/migemoで英数）
;; (setq default-input-method "MacOSX")でIME毎カーソル色変更などは出来なかった（未解決2016-03-28）
;; 下記のIME関係は、インラインパッチをあてたEmacsの全画面表示時に、日本語入力が一文字しか出来ないという問題のため、棚上げ（2016-03-28）
;; (add-hook 'after-init-hook 'mac-change-language-to-us)
;; (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
;; (add-hook 'isearch-mode-hook 'mac-change-language-to-us)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 表示
;; 各種表示/非表示
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode 1)
(show-paren-mode 1)
(setq ring-bell-function 'ignore)
(setq frame-title-format "%f")
;; フォント
;; Macでヒラギノ、それ以外で源ノ角。Source Code Proと源ノ角を統合したSource Han Code JPもあるが、欧文太字潰れや幅が気に入らず、見送り（2016-04-21）
(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :height 150)
(if (equal system-type 'darwin)
    (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic ProN"))
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Source Han Sans")))
(setq face-font-rescale-alist '(("Hiragino.*" . 1)))
;; ウィンドウ
(if window-system
    (progn
      (set-frame-parameter nil 'fullscreen 'maximized)
      (set-background-color "Black")
      (set-foreground-color "White")
      (set-cursor-color "Green")
      (set-frame-parameter nil 'alpha 80)))
;; 起動時画面
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
;; 空白
;;(require 'whitespace)
;;(global-whitespace-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; モードライン
;; 各種表示/非表示
(line-number-mode -1);; 常に行番号を表示しているので、モードラインには不要
(setq display-time-day-and-date t)
(display-time)
(display-battery-mode t)
(set-face-foreground 'mode-line "Yellow")
(set-face-background 'mode-line "DarkSlateBlue")
;; モードを略号表示
(defvar mode-line-cleaner-alist
  '(
    (helm-mode . "")
    (helm-migemo-mode . "")
    (ace-isearch-mode . "")
    (flyspell-mode . "")
    (abbrev-mode . "")
    (auto-complete-mode . "")
    (flycheck-mode . "")
    ;; Major modes
    (lisp-interaction-mode . "Li")
    (python-mode . "Py")
    (haskell-mode . "Hs")
    (emacs-lisp-mode . "El")
;;    (yatex-mode . "TeX")
    (c++-mode . "C++")
    (c-mode . "C")
    (markdown-mode . "Md")))
(defun clean-mode-line ()
  (interactive)
  (loop for (mode . mode-str) in mode-line-cleaner-alist
        do
        (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
          (when old-mode-str
            (setcar old-mode-str mode-str))
          ;; major mode
          (when (eq mode major-mode)
            (setq mode-name mode-str)))))
(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; キーバインド
(global-set-key "\C-u" 'undo)
(global-unset-key "\C-z")
(keyboard-translate ?\C-h ?\C-?)
(windmove-default-keybindings 'super);; 分割ウィンドウ移動をCMDで

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; exec-path-from-shell
(exec-path-from-shell-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Markdown-mode
(setq markdown-command "pandoc --standalone --self-contained --highlight-style=pygments -t html5 --css=/Users/kumpeishiraishi/dotfiles/.pandoc/github.css --mathjax=/Users/kumpeishiraishi/dotfiles/.pandoc/dynoload.js")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; haskell-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; YaTeX
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(add-hook 'yatex-mode-hook '(lambda () (auto-fill-mode -1)));; 自動折り返し無効

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode
;; orgキーバインド
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cr" 'org-remember)
;; org表示
(add-hook 'org-mode-hook 'turn-on-font-lock)
(setq org-hide-leading-stars t);; 見出しの*は最小限に
(setq org-startup-truncated nil);; 画面端で改行
;; org note
;; org-default-notes-fileのディレクトリ
(setq org-directory "~/Dropbox/Emacs/org/")
;; org-default-notes-fileのファイル名
(setq org-default-notes-file "notes.org")
;; TODO状態
(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
;; DONE時刻記録
(setq org-log-done 'time)
;; アジェンダ表示の対象ファイル
(setq org-agenda-files (list org-directory))
;; アジェンダ表示で下線を用いる
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
(setq hl-line-face 'underline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; スペルチェック
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell" '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")));; 日本語混じりでも有効に
(global-set-key [s-return] 'ispell-word)
;; flyspell
(mapc
 (lambda (hook)
   (add-hook hook
	     '(lambda () (flyspell-mode 1))))
 '(yatex-mode-hook
   markdown-mode-hook
   org-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; migemo
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm
;; helm
(require 'helm-config)
(helm-mode 1)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key helm-read-file-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "<tab>") 'helm-execute-persistent-action)
(helm-migemo-mode 1)
;; 検索
(require 'helm-swoop)
(global-ace-isearch-mode 1)
(setq ace-isearch-function 'avy-goto-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-complete
(ac-config-default)
(setq ac-auto-start 1)        ;; n文字で開始
(setq ac-auto-show-menu 0.2)  ;; n秒で開始
(setq ac-candidate-limit nil) ;; 補完候補表示を無制限に
(setq ac-use-menu-map t)
(setq ac-use-comphist nil)
(setq ac-ignore-case t)       ;; 大文字小文字を区別しない
(global-auto-complete-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SKK
;; load-path
(let ((default-directory (expand-file-name "~/.emacs.d/skk/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))
;; SKKを使用
(require 'skk-autoloads)
(require 'skk-study)
(global-set-key "\C-\\" 'skk-mode)
;; SKK辞書・ファイル
(setq skk-jisyo-code 'utf-8)
(setq skk-isearch-start-mode 'utf-8);; migemoではSKK不要
;; (setq skk-user-directory "~/Dropbox/Emacs/skk") これでは以下のように、ファイル群を望んだフォルダ配下に保存できない（2016-05-02）
(setq skk-jisyo "~/Dropbox/Emacs/skk/jisyo"
      skk-backup-jisyo "~/Dropbox/Emacs/skk/jisyo.bak"
      skk-record-file "~/Dropbox/Emacs/skk/record"
      skk-study-file "~/Dropbox/Emacs/skk/study"
      skk-study-backup-file "~/Dropbox/Emacs/skk/study.bak")
(setq skk-large-jisyo "~/Dropbox/Emacs/skk/SKK-JISYO.L")
(setq skk-extra-jisyo-file-list
      (list
       "~/Dropbox/Emacs/skk/SKK-JISYO.geo"
       "~/Dropbox/Emacs/skk/SKK-JISYO.jinmei"
       "~/Dropbox/Emacs/skk/SKK-JISYO.propernoun"
       "~/Dropbox/Emacs/skk/SKK-JISYO.station"))
(setq skk-tut-file "~/.emacs.d/skk/etc/SKK.tut")
;; SKK表示
(setq skk-latin-mode-string "A"
      skk-hiragana-mode-string "あ"
      skk-katakana-mode-string "ア")
(when skk-use-color-cursor
  (setq skk-cursor-default-color "Green"
	skk-cursor-hiragana-color "Magenta"
	skk-cursor-katakana-color "Cyan"
	skk-cursor-abbrev-color "Royalblue"
	skk-cursor-jisx0208-latin-color "Pink";; 全英
	skk-cursor-latin-color "Green"))
;; SKK設定
(setq skk-auto-insert-paren t);; 対応する閉括弧挿入
(setq skk-previous-candidate-key "x");; 前候補に戻るのはxだけ、C-pは使わない
(setq skk-dcomp-activate t);; 動的補完
;;      skk-dcomp-multiple-activate t
;;      skk-dcomp-multiple-rows 5);; 補完候補を複数表示させると表示が崩れるので、止め（2016-05-10）
(defadvice skk-j-mode-on (after skk-settings-for-dcomp activate)
  (define-key skk-j-mode-map "\C-n" 'skk-comp-wrapper)
  (define-key skk-j-mode-map "\C-p" 'skk-previous-comp-maybe))
(setq skk-show-annotation t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-set-key "\M-n" 'flycheck-next-error)
(global-set-key "\M-p" 'flycheck-previous-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash")
(setq dired-recursive-copies 'always
      dired-recursive-deletes 'always)
(setq dired-isearch-filenames t)
(setq dired-dwim-target t);; diredを2画面で開いていれば、片方でコピー/移動先を表示
(setq dired-listing-switches (purecopy "-alh"));; lsオプション

;;; init.el ends here
