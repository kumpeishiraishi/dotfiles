;;;;;;;;;;;;;;;;;;;;;;;; 表示関係 ;;;;;;;;;;;;;;;;;;;;;;;;
; 英語フォント
(set-face-attribute 'default nil
             :family "Source Code Pro" ;; font
             :height 150)              ;; font size
; 日本語フォント
(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic ProN"))
; 半角全角比を1:2
(setq face-font-rescale-alist '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)))
; ウィンドウサイズの位置、サイズ
(if window-system (progn
  (setq initial-frame-alist '((width . 132)(height . 39)(top . 0)(left . 0)))
  (set-background-color "Black")
  (set-foreground-color "White")
  (set-cursor-color "Gray")
))
; ウィンドウ透明化
(add-to-list 'default-frame-alist '(alpha . (0.80 0.80)))
; ツールバー非表示
(tool-bar-mode -1)
; タイトルバーにファイル名表示
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))
; 行番号常に表示
(require 'linum)
(global-linum-mode)
; 釣り合いのとれる括弧をハイライトする
(show-paren-mode 1)
; beep音・画面点滅どっちも消す
(setq ring-bell-function 'ignore)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;表示関係終わり

;;;;;;;;;;;;;;;;;;;;;;;; キーバインド等設定 ;;;;;;;;;;;;;;;;;;;;;;;;
; 常にutf8
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
; ホームディレクトリの件
(setq inhibit-splash-screen t)
(defun cd-to-homedir-all-buffers ()
  "Change every current directory of all buffers to the home directory."
  (mapc
   (lambda (buf) (set-buffer buf) (cd (expand-file-name "~"))) (buffer-list)))
(add-hook 'after-init-hook 'cd-to-homedir-all-buffers)
; C-uを「元に戻す」
(global-set-key "\C-u" 'undo)
; バックアップファイル作らない
(setq make-backup-files nil)
; C-hをバックスペース
(keyboard-translate ?\C-h ?\C-?)
; C-zを無効化
(global-unset-key "\C-z")
; emacs-24.5-inline.patchを当ててHomebrewからインストールして可能になった、日本語関係の設定（起動時、ミニバッファ、isearch/migemoで英数）
; (setq default-input-method "MacOSX")でIME毎カーソル色変更などは出来なかった（未解決2016/03/28）
(add-hook 'after-init-hook 'mac-change-language-to-us)
(add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
(add-hook 'isearch-mode-hook 'mac-change-language-to-us)
; 分割ウィンドウ移動
(windmove-default-keybindings 'super)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;キーバインド等設定終わり

;;;;;;;;;;;;;;;;;;;;;;;; パッケージ管理 ;;;;;;;;;;;;;;;;;;;;;;;;
; パッケージリストを追加
(require 'package)
; MELPA追加
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
; MELPA-stable追加
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
; Marmalade追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
; 初期化
(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;パッケージ管理終わり

;;;;;;;;;;;;;;;;;;;;;;;; exec-path-from-shell ;;;;;;;;;;;;;;;;;;;;;;;;
; PATHをshellからEmacsに引き継ぐ
(exec-path-from-shell-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;exec-path-from-shell終わり

;;;;;;;;;;;;;;;;;;;;;;;; Markdown-mode ;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; haskell-mode ;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; YaTeX ;;;;;;;;;;;;;;;;;;;;;;;;
; yatex-mode使用可能にし、拡張子と紐付け
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
; 自動折り返し無効
(add-hook 'yatex-mode-hook '(lambda () (auto-fill-mode -1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;YaTeX終わり

;;;;;;;;;;;;;;;;;;;;;;;; org-mode ;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
; キーバインド
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cr" 'org-remember)
; org-modeでの強調表示を可能に
(add-hook 'org-mode-hook 'turn-on-font-lock)
; 見出しの*は最小限に
(setq org-hide-leading-stars t)
; 画面端で改行
(setq org-startup-truncated nil)
; org-default-notes-fileのディレクトリ
(setq org-directory "~/Dropbox/Emacs/org/")
; org-default-notes-fileのファイル名
(setq org-default-notes-file "notes.org")
; TODO状態
(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
; DONE時刻記録
(setq org-log-done 'time)
; アジェンダ表示の対象ファイル
(setq org-agenda-files (list org-directory))
; アジェンダ表示で下線を用いる
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
(setq hl-line-face 'underline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;org-mode終わり

;;;;;;;;;;;;;;;;;;;;;;;; スペルチェック ;;;;;;;;;;;;;;;;;;;;;;;;
; aspell有効に
(setq-default ispell-program-name "aspell")
; 日本語混じりでも有効に
(eval-after-load "ispell" '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
; LaTeX, Markdown, org-modeでFlyspell常に有効に
(mapc
   (lambda (hook)
     (add-hook hook
                      '(lambda () (flyspell-mode 1))))
   '(
     yatex-mode-hook
     markdown-mode-hook
     org-mode-hook
     ))
; Flyspellで修正をCMD+RETで
(global-set-key (kbd "<s-return>") 'ispell-word)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;スペルチェック終わり

;;;;;;;;;;;;;;;;;;;;;;;; migemo ;;;;;;;;;;;;;;;;;;;;;;;;
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;migemo終わり

;;;;;;;;;;;;;;;;;;;;;;;; helm ;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm-config)
(helm-mode 1)
; TABで補完
(define-key helm-read-file-map (kbd "<tab>") 'helm-execute-persistent-action)
; migemo有効に
(helm-migemo-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;helm終わり


