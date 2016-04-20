;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el
;;; Created: around 2014
;;; Modified: 2016-04-20
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 設定
(setq make-backup-files nil)
;; 言語環境・文字コード
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

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
(set-face-attribute 'default nil
             :family "Source Code Pro"
             :height 150)
(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic ProN"))
(setq face-font-rescale-alist '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)));; 半角全角比を1:2
;; ウィンドウ
(if window-system (progn
  (setq initial-frame-alist '((width . 132)(height . 39)(top . 0)(left . 0)))
  (set-background-color "Black")
  (set-foreground-color "White")
  (set-cursor-color "Green")
));; 位置色
(add-to-list 'default-frame-alist '(alpha . (0.80 0.80)));; 透明化
;; 起動時画面
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
;; 空白
;;(global-whitespace-mode 1)
;;(setq whitespace-space-regexp "\\(\u3000\\)")
;;(setq whitespace-style '(face tabs tab-mark))
;;(setq whitespace-display-mappings ())
;;(set-face-foreground 'whitespace-tab "yellow")
;;(set-face-underline  'whitespace-tab t)
;;(set-face-foreground 'whitespace-space "yellow")
;;(set-face-background 'whitespace-space "red")
;;(set-face-underline  'whitespace-space t)

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
    ;; Major modes
    (lisp-interaction-mode . "Li")
    (python-mode . "Py")
    (haskell-mode . "Hs")
    (emacs-lisp-mode . "El")
;    (yatex-mode . "TeX")
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
(windmove-default-keybindings 'super);; 分割ウィンドウ移動をCMDで
(keyboard-translate ?\C-h ?\C-?)
;; emacs-24.5-inline.patchを当ててHomebrewからインストールして可能になった、日本語関係の設定（起動時、ミニバッファ、isearch/migemoで英数）
;; (setq default-input-method "MacOSX")でIME毎カーソル色変更などは出来なかった（未解決2016/03/28）
;;下記のIME関係は、インラインパッチをあてたEmacsの全画面表示時に、日本語入力が一文字しか出来ないという問題のため、棚上げ（2016/03/28）
;;(add-hook 'after-init-hook 'mac-change-language-to-us)
;;(add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
;;(add-hook 'isearch-mode-hook 'mac-change-language-to-us)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; パッケージ管理
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; exec-path-from-shell
(exec-path-from-shell-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Markdown-mode

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
;;; helmと検索
;; helm
(require 'helm-config)
(helm-mode 1)
(define-key helm-read-file-map (kbd "<tab>") 'helm-execute-persistent-action)
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

;; 設定
(require 'skk-autoloads)
(require 'skk-study)
(global-set-key "\C-\\" 'skk-mode)
(setq skk-jisyo-code 'utf-8)
(setq skk-large-jisyo "~/.emacs.d/skk/etc/SKK-JISYO.L")
(setq skk-extra-jisyo-file-list
      (list '("~/.emacs.d/skk/etc/SKK-JISYO.geo"
              "~/.emacs.d/skk/etc/SKK-JISYO.jinmei"
              "~/.emacs.d/skk/etc/SKK-JISYO.propernoun"
              "~/.emacs.d/skk/etc/SKK-JISYO.station"))
)
(setq skk-isearch-start-mode 'utf-8);; migemoではSKK不要
(setq skk-tut-file "~/.emacs.d/skk/etc/SKK.tut")
(setq skk-latin-mode-string "A"
      skk-hiragana-mode-string "あ"
      skk-katakana-mode-string "ア")
