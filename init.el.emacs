;; -*- coding: utf-8-unix mode:emacs-lisp -*-
;;
;; C-x F => 関数へジャンプ
;; C-x K => キーバインドに関連付けされた関数へジャンプ
;; C-x V => 変数の定義元へジャンプ
;; C-x C-e => S式評価
;; C-x [,] => ^L へジャンプ
;; M-: => S式評価
;;
;;(setq debug-on-error t)
;;
;;; use-package 要約
;;   - http://rubikitch.com/2014/09/09/use-package/
;;   - http://qiita.com/kai2nenobu/items/5dfae3767514584f5220
;; * use-package => require の代わりに使う
;;    * :if => ライブラリを読み込む条件を指定する
;; * :config => ライブラリ読み込み後に実行される初期化コード
;; * :init - :configより先に実行される初期化コード
;;   * 遅延キーワードなし
;;     1 ライブラリのロード
;;     2 :init キーワードの設定を評価
;;     3 :config キーワードの設定を評価
;;   * 遅延キーワードあり
;;     1 :init キーワードの設定を評価
;;     2 （ autoload された関数が実行されるタイミングで）ライブラリの遅延ロード
;;     3 :config キーワードの設定を評価
;; * 遅延キーワードは以下の５つ
;; ** :commands    => コマンド呼び出し時に評価したい初期化コード
;; ** :bind        => ショートカット押下時に評価したい初期化コード
;; ** :mode        => 拡張子とメジャーモードを関連付けしたい時
;; ** :interpreter => シバンのインタプリタとメジャーモードを関連付けしたい時
;; ** :defer       => ライブラリ側ですでに遅延指定されてるときで初期化コードを評価したい時


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-package ()
  (require 'package)

  (package-initialize)

  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ;;("marmalade" . "https://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")
                           ("org" . "http://orgmode.org/elpa/")))

  ;; パッケージ情報の更新
  ;; (package-refresh-contents)

  ;; インストールするパッケージ
  (setq my-packages
        '(use-package
           edit-server
           ag
           wgrep-ag
           yasnippet
           auto-complete
           company
           smartrep
           simple-screen
           helm
           helm-c-moccur
           helm-c-yasnippet
           helm-git
           helm-gtags
           helm-ag
           helm-open-github
           twittering-mode
           org
           magit
           web-mode
           lispxmp
           eldoc-extension
           paredit
           rainbow-delimiters
           clojure-mode
           queue
           cider
           ac-cider
           js2-mode
           jdee
           ;;origami
           ace-link
           ;;hydra
           smart-mode-line
           ;;powerline
           ;;e2wm
           ))

  ;; インストールしていないパッケージをインストール
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package)))

  (require 'use-package)
  ;;(put 'use-package 'lisp-indent-function 1)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 拡張なしで実現できる最低限の設定をここに。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-init ()
  ;;
  ;; #keybind
  ;;
  (define-key global-map (kbd "C-h") 'delete-backward-char)
  (define-key global-map (kbd "C-M-h") 'backward-kill-word)
  (define-key global-map (kbd "C-j") nil)
  (global-set-key (kbd "C-m")  'newline-and-indent)
  (define-key isearch-mode-map (kbd "C-h") 'isearch-del-char)

  ;; 別ウィンドウの逆スクロール
  (global-set-key (kbd "C-M-y") 'scroll-other-window-down)

  ;; 標準のバッファリストだとカーソルが移動しないので
  ;; カーソルが移動するコマンドを定義
  (global-set-key (kbd "C-x C-b")   'buffer-menu)

  ;;
  ;; #lang
  ;;
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (when (eq window-system 'w32)
    (setq file-name-coding-system 'cp932)
    (setq locale-coding-system 'cp932))

  ;;
  ;; #basic
  ;;

  ;; 白が嫌いなので速攻色を変えておく
  (set-background-color "Black")
  (set-foreground-color "LightGray")
  (set-cursor-color "Gray")

  (setq large-file-warning-threshold (* 25 1024 1024))
  (setq history-length 1000)
  (setq message-log-max 10000)
  (setq gc-cons-threshold (* 10 gc-cons-threshold))

  (savehist-mode 1)
  (setq-default save-place t)

  (setq echo-keystrokes 0.1)
  (setq enable-recursive-minibuffers t)
  (setq use-dialog-box nil)
  (setq scroll-conservatively 1)

  ;; ファイル名補完時の大文字小文字は無視
  (setq read-file-name-completion-ignore-case t)

  ;; バッファ補完時の大文字小文字は無視
  (setq read-buffer-completion-ignore-case t)

  ;; 検索時に大文字小文字を無視
  (setq-default case-fold-search t)

  ;; 補完時の大文字小文字は無視
  (setq completion-ignore-case t)
  (put 'upcase-region 'disabled nil)

  ;; ミニバッファからミニバッファを開けるように
  (setq enable-recurslve-m1nﾔbuffers t)

  ;; lisp再帰呼び出し？などのエラー防止。
  ;; - http://d.hatena.ne.jp/a666666/20100221/1266695355
  (setq max-lisp-eval-depth 5000)
  (setq max-specpdl-size 12000)

  ;; tab4, no tabcode
  (setq default-tab-width 4)
  (setq tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq indent-tabs-mode nil)

  ;; no beep, no bell
  (setq visible-bell t)
  (setq ring-bell-function 'ignore)

  ;; バックアップ、自動保存をしない
  (setq make-backup-files nil)
  (setq auto-save-default nil)

  ;; 新しいバッファを作成するときにいちいち聞いてこない
  (setq iswitchb-propmt-newbuffer nil)

  ;; save時に無駄な行末の空白を削除する
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; 画面分割を抑止する
  (setq pop-up-windows t)

  ;;
  ;; #alias
  ;;
  (defalias 'message-box 'message)
  (defadvice abort-recursive-edit (before minibuffer-save activate)
    (when (eq (selected-window) (active-minibuffer-window))
      (add-to-history minibuffer-history-variable (minibuffer-contents))))
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;;
  ;; #appiarance
  ;; 見た目の設定
  ;;

  ;; カーソル行ハイライト
  ;; http://ayato.hateblo.jp/entry/20120309/1331270088
  (defface hlline-face
    '((((class color)
        (background dark))
       (:background "dark slate gray"))
      (((class color)
        (background light))
       (:background "OliveDrab1"))
      (t
       ()))
    "*Face used by hl-line.")
  ;;(setq hl-line-face 'hlline-face)
  ;;(setq hl-line-face 'underline) ; 下線
  ;;(set-face-background 'hl-line "darkolivegreen")
  ;;(global-hl-line-mode)

  ;; メニューバーは出さない
  (menu-bar-mode 0)

  ;; ミニバッファに時計表示
  ;;(display-time)
  ;;(setq display-time-day-and-date t)

  ;; 行番号
  (setq linum-format "%3d ")
  (column-number-mode 1)
  (line-number-mode 1)

  ;; 最近開いたファイルの保存数
  (setq recentf-max-saved-items 512)

  ;; テキストをカラフルにする
  (global-font-lock-mode t)

  (show-paren-mode 1)
  (transient-mark-mode 1)

  (cond (window-system
         (setq initial-frame-alist '((width . 80) (height . 30)))
         ;;(set-background-color "RoyalBlue4")

         ;; スクロールバーは右に
         (set-scroll-bar-mode 'right)

         ;; ツールバーは出さない
         (tool-bar-mode 0)

         (scroll-bar-mode 0)

         ;; クリップボードを有効
         (setq x-select-enable-clipboard t)
         ))


  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; diredでWindowsに関連付けられたｱﾌﾟﾘを起動する
(defun uenox-dired-winstart ()
    "Type '[uenox-dired-winstart]': win-start the current line's file."
    (interactive)
    (if (eq major-mode 'dired-mode)
        (let ((fname (dired-get-filename)))
          (w32-shell-execute "open" fname)
          (message "win-started %s" fname))))
;; (setq ls-lisp-use-localized-time-format t)
;; (setq ls-lisp-format-time-list (quote ("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")))

(defun my-dired ()
  (add-hook 'dired-mode-hook
            (lambda ()
              (setenv "LC_ALL")
              (setenv "LC_TIME" "POSIX")
              (setq ls-lisp-use-localized-time-format t)
              (setq ls-lisp-format-time-list (quote ("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")))
              (define-key dired-mode-map "z" 'uenox-dired-winstart))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #twitter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-twitter ()
  (use-package twittering-mode
    :config
    (bind-key "C-c C-i" 'twittering-goto-next-uri twittering-mode-map)

    ;;
    ;; 認証
    ;;
    (setq twittering-auth-method 'oauth)
    (setq twittering-use-master-password t)
    (setq twittering-private-info-file (expand-file-name "~/.twittering-mode.gpg"))

    ;;
    ;; key bind
    ;;
    (bind-key "C-i" 'twittering-goto-next-uri twittering-mode-map)
    (bind-key "n" 'twittering-switch-to-next-timeline twittering-mode-map)
    (bind-key "p" 'twittering-switch-to-previous-timeline twittering-mode-map)
    (bind-key "F" 'twittering-favorite twittering-mode-map)
    ;;(bind-key "R" 'twittering-favorite twittering-mode-map)

    ;; サーバ証明書の認証を無効化する
    ;; http://d.hatena.ne.jp/wadakei/20120211/1328968663
    (setq twittering-allow-insecure-server-cert t)


    (setq twittering-timer-interval 75)
    (setq twittering-convert-fix-size 16)
    (setq twittering-update-status-function 'twittering-update-status-from-pop-up-buffer)
    (setq twittering-icon-mode nil)
    (setq twittering-scroll-mode nil)
    ;; いくつかのTLをまとめて名前をつけることができる
    (setq twittering-timeline-spec-alias
          `(("related-to" .
             ,(lambda (username)
                (if username
                    (format ":search/to:%s OR from:%s OR @%s/"
                            username username username)
                  ":home")))
            ))
    ;; 起動時に以下のリストを読みこむ
    (setq twittering-initial-timeline-spec-string
          '("$related-to(kiwanami)" ;; 括弧を変更するとユーザにもとづくおすすめが表示されるぽい
            "mikio_kun/geeks"
            "mikio_kun/friend"
            ":search/clojure/"
            ":search/libgdx/"
            ":search/emacs/"
            ":search/lisp/"
            ":search/vim/"
            ":direct_messages"
            ":home"
            ":favorites"
            ;;"mikio_kun/bot"
            ))

    ;; タイムラインのフォーマット
    ;; Documentation:
    ;; Format string for rendering statuses.
    ;; Ex. "%i %s,  %@:\n%FILL{  %T // from %f%L%r%R}
    ;;  "

    ;; Items:
    ;;  %s - screen_name
    ;;  %S - name
    ;;  %i - profile_image
    ;;  %d - description
    ;;  %l - location
    ;;  %L - " [location]"
    ;;  %r - " sent to user" (use on direct_messages{,_sent})
    ;;  %r - " in reply to user" (use on other standard timeline)
    ;;  %R - " (retweeted by user)"
    ;;  %RT{...} - strings rendered only when the tweet is a retweet.
    ;;             The braced strings are rendered with the information of the
    ;;             retweet itself instead of that of the retweeted original tweet.
    ;;             For example, %s for a retweet means who posted the original
    ;;             tweet, but %RT{%s} means who retweeted it.
    ;;  %u - url
    ;;  %j - user.id
    ;;  %p - protected?
    ;;  %c - created_at (raw UTC string)
    ;;  %C{time-format-str} - created_at (formatted with time-format-str)
    ;;  %@ - X seconds ago
    ;;  %T - raw text
    ;;  %t - text filled as one paragraph
    ;;  %' - truncated
    ;;  %FACE[face-name]{...} - strings decorated with the specified face.
    ;;  %FILL[prefix]{...} - strings filled as a paragraph. The prefix is optional.
    ;;                       You can use any other specifiers in braces.
    ;;  %FOLD[prefix]{...} - strings folded within the frame width.
    ;;                       The prefix is optional. This keeps newlines and does not
    ;;                       squeeze a series of white spaces.
    ;;                       You can use any other specifiers in braces.
    ;;  %f - source
    ;;  %# - id

    (add-hook 'twittering-mode-hook (lambda ()
                                      (set-face-bold-p 'twittering-username-face t)
                                      (set-face-foreground 'twittering-username-face "DeepSkyBlue3")
                                      (set-face-foreground 'twittering-uri-face "gray60")
                                      ;;(setq twittering-status-format "%i %s/%S,  %@: %FILL{  %T [ %f ]%L%r%R}")
                                      (setq twittering-status-format "%i %S [@%s],  %@: %FILL{  %T [ %f ]%L%r%R}")
                                      (setq twittering-retweet-format " RT @%s: %t")
                                      ))
    ;; ;; URL短縮サービスをj.mpに
    ;; ;; YOUR_USER_IDとYOUR_API_KEYを自分のものに置き換えてください
    ;; ;; from http://u.hoso.net/2010/03/twittering-mode-url-jmp-bitly.html
    ;; (add-to-list 'twittering-tinyurl-services-map
    ;; 	     '(jmp . "http://api.j.mp/shorten?version=2.0.1&login=YOUR_USER_ID&apiKey=YOUR_API_KEY&format=text&longUrl="))
    ;; (setq twittering-tinyurl-service 'jmp)

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #bind-key
;; とにかく優先したいキーバインドをここに記述する。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-bind-key ()
  (use-package bind-key
    :config
    (bind-key* "C-h" 'backward-delete-char)
    (bind-key* "M-o" 'switch-to-last-buffer-or-other-window)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #elisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-elisp ()
  (find-function-setup-keys) ;; C-x F, C-x Kなどを使えるようにする。

  (use-package lispxmp
    :config
    (define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp))

  (use-package eldoc-extension
    :config
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-clojure ()
  (use-package cider
    :config
    (add-hook 'clojure-mode-hook 'cider-mode)
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

    ;; 'C-x b' した時に *nrepl-connection* と *nrepl-server* のbufferを一覧に表示しない
    (setq nrepl-hide-special-buffers t)

    ;; RELPのbuffer名を 'project名:nREPLのport番号' と表示する
    ;; project名は project.clj で defproject した名前
    (setq nrepl-buffer-name-show-port t)

    ;; 2013/12/23 の時点では、repl起動時にpretty-printのエラーがでてしまうので
    ;; それを抑制する。
    ;; https://github.com/clojure-emacs/cider/issues/367
    ;;(setq cider-repl-use-pretty-printing nil)


    ;;
    ;; REPL History
    ;;
    ;; To make the REPL history wrap around when its end is reached:
    (setq cider-repl-wrap-history t)
    ;;To adjust the maximum number of items kept in the REPL history:
    (setq cider-repl-history-size 1000) ; the default is 500
    ;;To store the REPL history in a file:
    (setq cider-repl-history-file "~/.emacs.d/cider-repl-history")
    )

  (use-package ac-cider
    :config
    (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
    (add-hook 'cider-mode-hook 'ac-cider-setup)
    (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #lisp
;; 共通のlisp設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-lisp ()
  (use-package paredit
    :config
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    (add-hook 'ielm-mode-hook 'enable-paredit-mode)
    (add-hook 'clojure-mode-hook 'enable-paredit-mode)
    (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
    )

  (use-package rainbow-delimiters
    :config
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun java-lineup-arglist (langelem)
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    (vector (+ (current-column) c-basic-offset))))

(defun java-lineup-annotated-method (langelem)
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    (when (looking-at "@")
      (vector (current-column)))))

(defun java-lineup-enum-entry (langelem)
  (save-excursion
    (if (looking-at "}")
        (vector (- (current-column) c-basic-offset))
      (while (eq (c-langelem-sym langelem) 'statement-cont)
        (goto-char (c-langelem-pos langelem))
        (setq langelem (car (c-guess-basic-syntax))))
      (when (and (eq (c-langelem-sym langelem) 'defun-block-intro)
                 (save-excursion
                   (re-search-backward "\\<enum\\>"
                                       (c-langelem-pos langelem) t)))
        (vector (current-column))))))

(defun my-java ()
  (c-add-style
   "java2"
   '("java"
     (c-offsets-alist
      ;; 引数の調整
      (arglist-intro . +)
      (arglist-close . 0)
      (arglist-cont-nonempty . java-lineup-arglist)
      ;; 無名クラスの調整
      (inexpr-class . 0)
      ;; enumの調整
      (statement-cont . (first java-lineup-annotated-method
                               java-lineup-enum-entry
                               +))
      ;; アノテーションの調整
      (topmost-intro-cont .(first java-lineup-annotated-method
                                  0))
      )))
  (add-hook 'java-mode-hook
            (lambda ()
              (linum-mode)
              (define-key java-mode-map (kbd "M-C-h") 'backward-kill-word)
              (c-set-style "java2")
              (set (make-local-variable 'compile-command)
                   (format "javac %s" (file-name-nondirectory buffer-file-name)))
              (message "jaba-hook")
              ;;(auto-complete 1)
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #jdee
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-jdee ()
  (require 'jdee)
  (setq jdee-server-dir "~/local/src/jdee-server/target")

  ;; JDEEでantビルド時に発生する下記のエラーを抑制
  ;;   No such directory found via CDPATH environment variable
  ;;(setq command-line-default-directory "~/dev/")
  )

;;(defun my-jdee ()
;;  (use-package jdee
;;    :config
;;
;;    ;; jdeeの設定変数は、init.elで読んでも初期化されない。理由はわからん。。
;;    ;;(setq jdee-server-dir "c:/Users/mikio/dev/jdee-server/target")
;;    ;;(setq jdee-import-auto-sort t)      ; import文挿入時に自動でソート
;;
;;    ;; complilationバッファを自動的にスクロールさせる
;;    (setq compilation-ask-about-save nil)
;;    (setq compilation-scroll-output 'first-error)
;;    ))

(defun my-jde-bsh-restart ()
  "BeanShellを再起動してprj.elを読みこむ"
  (interactive)
  (jde-bsh-exit)
  (sleep-for 1)
  (jde-load-project-file)
  (jde-bsh-run)
  (end-of-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-auto-complete ()
  (use-package auto-complete-config
    :config
    (ac-config-default)
    ;;(global-auto-complete-mode t)
    (setq ac-use-menu-map t)
    (define-key ac-menu-map (kbd "C-n") 'ac-next)
    (define-key ac-menu-map (kbd "C-p") 'ac-previous)
    (define-key ac-menu-map (kbd "ESC") 'ac-stop)
    (setq ac-auto-start 1)
    (add-to-list 'ac-modes 'emacs-lisp-mode)
    (add-to-list 'ac-modes 'cider-mode)
    (add-to-list 'ac-modes 'cider-repl-mode)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #simple-screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-simple-screen ()
    (use-package simple-screen
      :config
      (global-set-key (kbd "C-c") 'simple-screen-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-org ()
 (use-package org
   :config

   ;; 見た目だけインデントする(実際にはインデントしない)
   (setq org-startup-indented t)

   ;; 見出しの*を隠す
   (setq org-hide-leading-stars t)

   ;; コードブロックも色付け
   (setq org-src-fontify-natively t)

   ;; t:自動的にTODO項目にdoneの印をつける
   (setq org-log-done t)

   ;;(setq org-agenda-include-diary t)
   (setq org-agenda-include-diary nil)

   ;; 言語は日本語
   (setq org-export-default-language "ja")

   ;; 文字コードはUTF-8
   (setq org-export-html-coding-system 'utf-8)

   ;; 行頭の:は使わない BEGIN_EXAMPLE 〜 END_EXAMPLE で十分
   (setq org-export-with-fixed-width nil)

   ;; ^と_を解釈しない
   (setq org-export-with-sub-superscripts nil)

   ;; --や---をそのまま出力する
   (setq org-export-with-special-strings nil)

   ;; TeX・LaTeXのコードを解釈しない
   (setq org-export-with-TeX-macros nil)
   (setq org-export-with-LaTeX-fragments nil)

   (setq org-agenda-time-grid
         '((daily today require-timed)
           "----------------"
           (900 930 1000 1030 1100 1130 1200 1230
                1300 1330 1400 1430 1500 1530 1600 1630 1700 1730 1800 1830
                1900 1930 2000 2030 2100 2130 2200 2230 2300 2330 )))

   ;;
   ;; agendaの日付フォーマットを日本語表記に変更。
   ;; http://valvallow.blogspot.com/2011/02/org-agenda-weekly-view.html
   ;;
   (defadvice org-agenda (around org-agenda-around)
     (let ((system-time-locale "English"))
       ad-do-it))
   (defadvice org-agenda-redo (around org-agenda-redo-around)
     (let ((system-time-locale "English"))
       ad-do-it))
   (custom-set-variables
    '(org-agenda-format-date "%Y/%m/%d (%a)"))
   (custom-set-faces
    '(org-agenda-date ((t :weight bold))))

   ;;
   ;; orgファイルの場所
   ;;
   (setq hostname (system-name))
   ;;(message hostname)
   (setq org-directory nil)
   (if (or (equal "MADO-PC" hostname)
           (equal "gnudam" hostname))
       (setq org-directory (expand-file-name "~/OneDrive/org")))
   (if (equal "PC-18176" hostname)
       (setq org-directory (expand-file-name "~/docs/org")))
   ;; TODO org-directoryの判定は、現状のnilかどうかの判定から
   ;;      ディレクトリがあるかどうかに書き換える。
   (if (not (null org-directory))
       (progn
         (require 'em-glob)
         (setq org-agenda-files (eshell-extended-glob (concat org-directory "**/*.org")))))



   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-helm ()
  (use-package helm-config
    :bind
    (("M-y" . helm-show-kill-ring) ; 過去のkillリングの内容を表示する。
     )
    :config
    '(
      (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
      (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

      )

    ;;(helm-dired-bindings 1)

    ;; nil:helmのファイル一覧でパスを表示する
    (setq helm-ff-transformer-show-only-basename nil)

    ;;(helm-mode t) ; なんでもhelm
    )

  (use-package helm-gtags
    :config
    (add-hook 'c-mode-hook (lambda () (helm-gtags-mode)))
    (add-hook 'java-mode-hook (lambda () (helm-gtags-mode)))
    (add-hook 'jdee-mode-hook (lambda () (helm-gtags-mode)))

    ;; customize
    (setq helm-c-gtags-path-style 'relative)
    (setq helm-c-gtags-ignore-case t)
    (setq helm-c-gtags-read-only nil)

    )

    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x b") 'helm-buffers-list)
    (global-set-key (kbd "C-x a r") 'helm-recentf)
    ;; (global-set-key (kbd "C-x a h b") 'helm-hatena-bookmark)
    (global-set-key (kbd "C-x a b") 'helm-bookmarks)
    (global-set-key (kbd "C-x a i") 'helm-imenu)
    (global-set-key (kbd "C-x a g") 'helm-do-grep)
    ;;(global-set-key (kbd "C-x a g") 'helm-ack)
    (global-set-key (kbd "C-x a d") 'helm-for-document)
    (global-set-key (kbd "C-x a m") 'helm-man-woman)
    ;;(global-set-key (kbd "C-x C-f") 'helm-find-files)

    ;; 関数の定義元へ移動
    (local-set-key (kbd "C-c t j") 'helm-gtags-find-tag)
    ;; 関数を参照元の一覧を表示．RET で参照元へジャンプできる
    (local-set-key (kbd "C-c t r") 'helm-gtags-find-rtag)
    ;; 変数の定義元と参照元の一覧を表示．RET で該当箇所へジャンプできる．
    (local-set-key (kbd "C-c t s") 'helm-gtags-find-symbol)
    ;; すべてのシンボルから選択する
    (local-set-key (kbd "C-c t S") 'helm-gtags-select)
    ;; すべてのシンボルから選択する
    (local-set-key (kbd "C-c t p") 'helm-gtags-parse-file)
    ;; 元にもどる
    (local-set-key (kbd "C-c t b") 'helm-gtags-pop-stack)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #eww
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ewwを複数起動
;;   http://futurismo.biz/archives/2950
(defun my-eww-mode-hook--rename-buffer ()
  "Rename eww browser's buffer so sites open in new page."
  (rename-buffer "eww" t))
(add-hook 'eww-mode-hook 'my-eww-mode-hook--rename-buffer)

(defun my-eww ()
  (use-package eww
    :config
    ;;:commands (eww)
    (setq browse-url-browser-function 'eww-browse-url) ;; emacsの標準ブラウザとしてewwを指定する。
    ;; (bind-key "H" 'eww-back-url eww-mode-map)
    ;; (bind-key "L" 'eww-forward-url eww-mode-map)
    ;; (bind-key "r" 'eww-reload eww-mode-map)
    (bind-key "<backtab>" 'shr-previous-link eww-mode-map)
    (bind-key "C-c C-l" 'eww eww-mode-map)
    (bind-key "C-c l" 'eww-copy-page-url eww-mode-map)
    ;;(bind-key "&" 'eww-browse-with-external-browser eww-mode-map)
    (bind-key "f" 'ace-link-eww eww-mode-map)
    ;;(bind-key "q" 'quit-window eww-mode-map)
    (bind-key "j" 'next-line eww-mode-map)
    (bind-key "k" 'previous-line eww-mode-map)
    (setq eww-search-prefix "https://www.google.com/search?q=")

    ;; 色味の設定
    (defvar eww-disable-colorize t)
    (defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
      (unless eww-disable-colorize
        (funcall orig start end fg)))
    (advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
    (advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
    (defun eww-disable-color ()
      "eww で文字色を反映させない"
      (interactive)
      (setq-local eww-disable-colorize t)
      (eww-reload))
    (defun eww-enable-color ()
      "eww で文字色を反映させる"
      (interactive)
      (setq-local eww-disable-colorize nil)
      (eww-reload))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #edit-server
;; for Edit With Emacs(chrome)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-edit-server ()
  (use-package edit-server
    :config
    (setq edit-server-new-frame nil)
    (edit-server-start)
    (setq network-coding-system-alist
          '(("nntp" . (junet-unix . junet-unix))
            (9292 . (utf-8 . utf-8))    ; edit-server文字化け対策
            (110 . (no-conversion . no-conversion))
            (25 . (no-conversion . no-conversion))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #hex-region
;; リージョンで選択したURL文字列をデコード/エンコードする
;; http://stackoverflow.com/questions/611831/how-to-url-decode-a-string-in-emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun hex-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #last-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ひとつ前のバッファに戻る
(defun switch-to-last-buffer-or-other-window ()
  (interactive)
  (if (one-window-p t)
      (switch-to-last-buffer)
    (other-window 1)))

(defvar last-buffer-saved nil)
;; last-bufferで選択しないバッファを設定
(defvar last-buffer-exclude-name-regexp
  (rx (or "*mplayer*" "*Completions*" "*Org Export/Publishing Help*"
          (regexp "^ "))))
(defun record-last-buffer ()
  (when (and (one-window-p)
             (not (eq (window-buffer) (car last-buffer-saved)))
             (not (string-match last-buffer-exclude-name-regexp
                                (buffer-name (window-buffer)))))
    (setq last-buffer-saved
          (cons (window-buffer) (car last-buffer-saved)))))
(add-hook 'window-configuration-change-hook 'record-last-buffer)
(defun switch-to-last-buffer ()
  (interactive)
  (condition-case nil
      (switch-to-buffer (cdr last-buffer-saved))
    (error (switch-to-buffer (other-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #w32-init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-w32-init ()
  ;;
  ;; #lang
  ;;
  (prefer-coding-system 'utf-8)
  (setq file-name-coding-system 'sjis)
  (setq locale-coding-system 'sjis)
  ;;(process-coding-system 'sjis)

  ;;-----------------------------------------------------------------
  ;; font
  ;; 01234567890123456789
  ;; あいうえおかきくけこ
  ;; 800heigth == 8PT
  ;; 現在利用できるフォント一覧
  ;; (insert (prin1-to-string (x-list-fonts "*")))
  ;;-----------------------------------------------------------------
  (cond
   ;; windowsの場合
   ((eq system-type 'windows-nt)

    ;; 英字フォントの設定
    (set-face-attribute 'default nil
                        :family "Consolas"
                        :height 105)

    (set-fontset-font "fontset-default"
                      'japanese-jisx0208
                      '("MeiryoKe_Console")
                      )

    (set-fontset-font "fontset-default"
                      'katakana-jisx0201
                      '("MeiryoKe_Console")
                      )
    (setq face-font-rescale-alist
          '(
            (".*Cosolas.*" . 1.0)
            (".*MeiryoKe_Console.*"    . 1.1)
            ))

    )

   ;; cygwinの場合(emacs-w32)
   ((eq system-type 'cygwin)

    ;; 英字フォントの設定
    (set-face-attribute 'default nil
                        :family "Consolas"
                        :height 105)

    (set-fontset-font "fontset-default"
                      'japanese-jisx0208
                      '("MeiryoKe_Console")
                      )

    (set-fontset-font "fontset-default"
                      'katakana-jisx0201
                      '("MeiryoKe_Console")
                      )
    (setq face-font-rescale-alist
          '(
            (".*Cosolas.*" . 1.0)
            (".*MeiryoKe_Console.*"    . 1.1)
            )
          )
    )

   ;; linuxなど
   (t
    ;;

    ))
  )




(defun my-hydra ()
  (global-set-key
   (kbd "C-n")
   (defhydra hydra-move
     (:body-pre (next-line))
     "move"
     ("n" next-line)
     ("p" previous-line)
     ("f" forward-char)
     ("b" backward-char)
     ("a" beginning-of-line)
     ("e" move-end-of-line)
     ("v" scroll-up-command)
     ;; Converting M-v to V here by analogy.
     ("V" scroll-down-command)
     ("l" recenter-top-bottom)))
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(my-init)
(my-package)
(my-clojure)
(my-elisp)
(my-lisp)
(my-java)
(my-jdee)
(my-auto-complete)
(my-simple-screen)
(my-dired)
(my-org)
(my-eww)
(my-twitter)
(my-edit-server)
(my-helm)
(my-bind-key)
(my-w32-init)
;;(my-hydra)

;; customize の出力先
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;;;;;;;  server start for emacs-client
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))
(put 'dired-find-alternate-file 'disabled nil)
