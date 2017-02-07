;; -*- coding: utf-8-unix mode:emacs-lisp -*-
;;
;; C-x F => 関数へジャンプ
;; C-x K => キーバインドに関連付けされた関数へジャンプ
;; C-x V => 変数の定義元へジャンプ
;; C-x C-e => S式評価
;; C-c C-d => 行末に";=>"がある場合にS式評価
;; C-x [,] => ^L へジャンプ
;; M-: => S式評価
;;
;; 下記のようなエラーが発生したら
;;   Reading at buffer position 999
;; 次のコマンドを実行する(goto-char)
;;   M-g c 999 RET
;;
;;(setq debug-on-error t)
;;
;;; ------------------------------------------------------------
;;; use-package 要約
;;;   - http://rubikitch.com/2014/09/09/use-package/
;;;   - http://qiita.com/kai2nenobu/items/5dfae3767514584f5220
;;; autoloadについては下記が詳しい
;;;   - http://yohshiy.blog.fc2.com/blog-entry-270.html
;;; ------------------------------------------------------------
;; * use-package
;;   - require の代わりに使う
;;   - クオートは外す。use-packageを使うと、ライブラリがなくてもエラーにならない。
;;     (require 'hoge) => (use-package hoge)
;;   - :if => ライブラリを読み込む条件を指定する
;; * :config
;;   - ライブラリ読み込み後に実行される初期化コード
;; * :init
;;   - :configより先に実行される初期化コード
;; * 初期化コード実行の流れ
;;   * 遅延キーワードなし
;;     1 ライブラリのロード
;;     2 :init キーワードの設定を評価
;;     3 :config キーワードの設定を評価
;;   * 遅延キーワードあり
;;     1 :init キーワードの設定を評価
;;     2 （ autoload された関数が実行されるタイミングで）ライブラリの遅延ロード
;;     3 :config キーワードの設定を評価
;; * 遅延キーワードは以下の５つ
;;   * :commands    => コマンド呼び出し時に評価したい初期化コード
;;   * :bind        => ショートカット押下時に評価したい初期化コード
;;   * :mode        => 拡張子とメジャーモードを関連付けしたい時
;;   * :interpreter => シバンのインタプリタとメジャーモードを関連付けしたい時
;;   * :defer       => ライブラリ側ですでに遅延指定されてるときで初期化コードを評価したい時
;;; ------------------------------------------------------------
;;; autoloadについて
;;;   - いつも忘れるのでメモ
;;;   - http://yohshiy.blog.fc2.com/blog-entry-270.html
;;; ------------------------------------------------------------
;; (autoload 'winner-undo "winner" "" t)
;;   第1引数のwinner-undoという関数のシンボルだけを登録する。
;;   第2引数の"winner"は、第一引数の関数が呼びだされたら、winner.elを呼び出すよという意味 。
;;   最後のtは、このシンボルがM-xで呼び出せるかどうかという意味。tだと呼び出せる。
;;
;; (eval-after-load 'winner
;;   '(progn
;;      ;; winnerの設定
;;      ))
;;  winner がロードされたらprongnの内部の設定を実行するという意味。
;;

;; カスタム変数保存ファイル
(setq custom-file "~/.emacs.d/custom.el")

;; 使用パッケージ
(defvar my-packages
  '(seq
    edit-server
    ag
    wgrep-ag
    yasnippet
    ;;popwin
    auto-complete
    company
    smartrep
    color-moccur
    hydra
    ;;tabbar
    simple-screen
    ;; e2wm
    anything
    helm
    ;; helm-gtags
    twittering-mode
    org
    magit
    web-mode
    slime
    ac-slime
    lispxmp
    eldoc-extension
    paredit
    rainbow-delimiters
    clojure-mode
    queue
    cider
    ac-cider
    js2-mode
    markdown-mode
    ;;gradle-mode
    groovy-mode
    meghanada ;; ide like mode for java
    flycheck  ;; for meghanada
    go-mode
    ;;go-autocomplete
    company-go
    go-eldoc
    which-key
    origami
    ace-link
    powershell
    ;; mozc
    ;; mozc-im
    ;; mozc-popup
    ddskk
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #package
;; 下記が非常に参考になった。
;; http://ja.stackoverflow.com/questions/2157/emacs%E3%83%91%E3%83%83%E3%82%B1%E3%83%BC%E3%82%B8%E7%AE%A1%E7%90%86%E3%81%AE%E8%89%AF%E3%81%84%E6%96%B9%E6%B3%95%E3%81%AB%E3%81%A4%E3%81%84%E3%81%A6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(package-initialize) ;; package群をload-pathに通す
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ;;("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; packageインストールコマンド
(defun my-package-refresh ()
  "my package install command"
  (interactive)
  (package-refresh-contents)
  (dolist (package my-packages)
    (when (not (package-installed-p package))
    (package-install package))))

;; use-packegeがなければインストール
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; seqライブラリ
;; emacs25よりビルトイン。
;;(use-package seq)

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
;;; 拡張なしで実現できる最低限の設定をここに。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-init ()
  ;;
  ;; #basic
  ;;

  ;; #keybind
  (keyboard-translate ?\C-h ?\C-?) ;; C-h を BACKSPACEにする
  (global-set-key (kbd "C-M-h") 'backward-kill-word)

  ;; 白が嫌いなので速攻色を変えておく
  (set-background-color "Black")
  (set-foreground-color "LightGray")
  (set-cursor-color "Gray")

  ;; モードライン
  (set-face-foreground 'mode-line "Black")
  (set-face-background 'mode-line "White")
  (set-face-foreground 'mode-line-inactive "gray30")
  (set-face-background 'mode-line-inactive "gray85")

  ;; 標準のバッファリストだとカーソルが移動しないので
  ;; カーソルが移動するコマンドを定義
  (global-set-key (kbd "C-x C-b")   'buffer-menu)

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
  ;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; 画面分割を抑止する
  (setq pop-up-windows t)

  ;; 「ん」を n 2回で入力 (デフォは1回)
  (setq quail-japanese-use-double-n t)

  ;;
  ;; #alias
  ;;
  (defalias 'message-box 'message)
  (defadvice abort-recursive-edit (before minibuffer-save activate)
    (when (eq (selected-window) (active-minibuffer-window))
      (add-to-history minibuffer-history-variable (minibuffer-contents))))
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; ;;
  ;; ;; #appiarance
  ;; ;; 見た目の設定
  ;; ;;
  ;; (defface hlline-face
  ;;   '((((class color)
  ;;       (background dark))
  ;;      (:background "brightblue"))
  ;;     (((class color)
  ;;       (background light))
  ;;      (:background "blue"))
  ;;     (t
  ;;      ()))
  ;;   "*Face used by hl-line.")
  ;; (setq hl-line-face 'hlline-face)
  ;; (global-hl-line-mode)

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

  ;; GCを減らして軽くする.
  (setq gc-cons-threshold (* gc-cons-threshold 10))
  (setq garbage-collection-messages t) ;; GC時にメッセージ出力

  ;; ミニバッファで確認せずにすぐにカレントバッファを消す
  (global-set-key (kbd "C-x k") 'kill-this-buffer)

  (cond (window-system
         (setq initial-frame-alist '((width . 120) (height . 80)))
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
;; #linux-init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-linux-init ()
  (message "linux...")

  (setenv "LANG" "ja_JP.UTF-8")
  ;;(prefer-coding-system 'utf-8)
  ;;(set-default-coding-systems 'utf-8)
  (set-coding-system-priority 'utf-8)
  ;;(my-mozc)
  (set-frame-font "ricty-13.5")
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #cygwin-windows-init
;; cygwinとwindows共通の設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-cygwin-windows-init ()
  ;; diredでWindowsに関連付けられたアプリを起動する
  (add-hook
   'dired-mode-hook (lambda ()
                      (define-key dired-mode-map "z" 'uenox-dired-winstart)))
  (if (eq window-system 'w32)
      (progn
        (my-w32-font)
        ))
  ;;(my-mozc)
  ;; (advice-add 'mozc-session-execute-command
  ;;             :after (lambda (&rest args)
  ;;                      (when (eq (nth 0 args) 'CreateSession)
  ;;                        (mozc-session-sendkey '(hiragana)))))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #w32-init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-windows-init ()
  (message "windows-nt...")
  (prefer-coding-system 'sjis-dos)
  ;; (setq file-name-coding-system 'sjis)
  ;; (setq locale-coding-system 'sjis)
  ;; (process-coding-system 'sjis)
  (my-cygwin-windows-init)
  (use-package powershell)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #cygwin-init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-cygwin-init ()
  (message "cygwin...")
  ;;(setenv "LANG" "ja_JP.UTF-8")
  ;; (setq default-buffer-file-coding-system 'utf-8-unix)
  ;; (set-coding-system-priority 'utf-8)
  ;; (set-terminal-coding-system 'utf-8)
  ;; (set-keyboard-coding-system 'utf-8)

  ;;(add-to-list 'process-coding-system-alist '("git" utf-8 . cp932))

  ;;(setq temporary-file-directory "/tmp")
  (setq temporary-file-directory (concat (getenv "HOME") "/AppData/Local/Temp"))

  (my-cygwin-windows-init)
  )

;; ;; process-connection-type が nil で start-process がコールされるけれども、fakecygpty を経由して
;; ;; 起動したいプログラムの名称を列挙する
;; (defvar fakecygpty-program-list '("curl" "gpg" "bash"))

;; ;; fakecygpty を経由するかを判断してプログラムを起動する
;; (advice-add 'start-process
;;             :around (lambda (orig-fun &rest args)
;;                       (when (and (nth 2 args)
;;                                  (or process-connection-type
;;                                      (member (replace-regexp-in-string "\\.exe$" ""
;;                                                                        (file-name-nondirectory (nth 2 args)))
;;                                              fakecygpty-program-list)))
;;                         (push "fakecygpty" (nthcdr 2 args)))
;;                       (apply orig-fun args))
;;             '((depth . 100)))

;; fakecygpty を経由して起動したプロセスに対し、コントロールキーを直接送信する
;; (cl-loop for (func ctrl-key) in '((interrupt-process "C-c")
;;                                   (quit-process      "C-\\")
;;                                   (stop-process      "C-z")
;;                                   (process-send-eof  "C-d"))
;;          do (eval `(advice-add ',func
;;                                :around (lambda (orig-fun &rest args)
;;                                          (let ((process (or (nth 0 args)
;;                                                             (get-buffer-process (current-buffer)))))
;;                                            (if (string= (car (process-command process)) "fakecygpty")
;;                                                (process-send-string (nth 0 args) (kbd ,ctrl-key))
;;                                              (apply orig-fun args)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #my-environment-init
;; 環境ごとの初期化
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-environment-init ()

  ;; 文字コードの設定はできるだけ設定の最後で実施すること。
  ;; 理由は、様々なパッケージが買って言うが脇する可能性があるため。
  (prefer-coding-system 'utf-8-unix)
  (set-coding-system-priority 'utf-8)

  (cond
   ((eq system-type 'gnu/linux)
    (my-linux-init)
    )
   ((eq system-type 'cygwin)
    (my-cygwin-init)
    )
   ((eq system-type 'windows-nt)
    (my-windows-init)
    )
   (t
    (message "default environment...")
    )
   )
  )
;;-----------------------------------------------------------------
;; font
;; 01234567890123456789
;; あいうえおかきくけこ
;; 800heigth == 8PT
;; 現在利用できるフォント一覧
;; (insert (prin1-to-string (x-list-fonts "*")))
;;-----------------------------------------------------------------
(defun my-w32-font ()
  ;; 英字フォントの設定
  (set-face-attribute 'default nil
                      :family "Consolas"
                      :height 100)

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #my-whitespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-whitespace ()
  (use-package whitespace
    :config
    (setq whitespace-style '(face       ; faceで可視化
                             trailing   ; 行末
                             tabs       ; タブ
                             spaces     ; スペース
                             empty      ; 先頭/末尾の空行
                             space-mark ; 表示のマッピング
                             tab-mark
                             ))

    (setq whitespace-display-mappings
          '((space-mark ?\u3000 [?\u25a1])
            ;; WARNING: the mapping below has a problem.
            ;; When a TAB occupies exactly one column, it will display the
            ;; character ?\xBB at that column followed by a TAB which goes to
            ;; the next TAB column.
            ;; If this is a problem for you, please, comment the line below.
            (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

    ;; スペースは全角のみを可視化
    (setq whitespace-space-regexp "\\(\u3000+\\)")

    ;;(global-whitespace-mode 1)

    (defvar my/bg-color "#232323")
    (set-face-attribute 'whitespace-trailing nil
                        :background my/bg-color
                        :foreground "DeepPink"
                        :underline t)
    (set-face-attribute 'whitespace-tab nil
                        :background my/bg-color
                        :foreground "LightSkyBlue"
                        :underline t)
    (set-face-attribute 'whitespace-space nil
                        :background my/bg-color
                        :foreground "GreenYellow"
                        :weight 'bold)
    (set-face-attribute 'whitespace-empty nil
                        :background my/bg-color)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #mozc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-mozc-on ()
  (toggle-input-method)
  )
(defun my-mozc-off ()
  (toggle-input-method nil)
  )
(defun my-mozc ()
  (use-package mozc
    :config
    (setq default-input-method "japanese-mozc")

    ;; mozcの設定
    (require 'mozc)
    (setq default-input-method "japanese-mozc")

    ;; 全角半角キーで on/off
    (global-set-key [zenkaku-hankaku] 'toggle-input-method)

    ;; 変換キーでon
    (global-set-key [henkan]
                    (lambda () (interactive)
                      (when (null current-input-method) (toggle-input-method))))
    ;; 無変換キーでon
    (global-set-key [muhenkan]
                    (lambda () (interactive)
                      (inactivate-input-method)))
    ;; 全角半角キーと無変換キーのキーイベントを横取りする
    (defadvice mozc-handle-event (around intercept-keys (event))
      "Intercept keys muhenkan and zenkaku-hankaku, before passing keys
to mozc-server (which the function mozc-handle-event does), to
properly disable mozc-mode."
      (if (member event (list 'zenkaku-hankaku 'muhenkan))
          (progn
            (mozc-clean-up-session)
            (toggle-input-method))
        (progn                          ;(message "%s" event) ;debug
          ad-do-it)))
    (ad-activate 'mozc-handle-event)

    ;; ミニバッファではなく、カーソル直下に候補リストを表示する
    (setq mozc-candidate-style 'overlay)

    )
  (use-package mozc-im
    :config
    )
  (use-package mozc-popup
    :config
    ;; popupスタイル を使用する
    (setq mozc-candidate-style 'popup)
    )
  )
;;-----------------------------------------------------------------
;; DDSKK
;;-----------------------------------------------------------------
(defun my-ddskk ()
  (use-package skk
    ;; このキーが押下されたときにライブラリ読込
    :bind (("C-x C-j" . skk-mode))

    ;; ライブラリ読み込み前に実行
    :init
    (custom-set-variables '(skk-sticky-key ";")) ;; 本体読込前に定義しないとうまくいかない
    ;; ライブラリ読み込み後に実行
    :config
    (setq default-input-method "japanese-skk")
    (setq skk-user-directory "~/.emacs.d/SKK") ;; 設定ファイル、個人辞書ファイルの置き場
    (setq skk-init-file "~/.emacs.d/SKK/init") ;; 設定ファイルの指定

    (add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
    (add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)

    ;; SKKモード時のフェイス
    ;; (set-face-foreground 'skk-emacs-hiragana-face "red")
    ;; (set-face-background 'skk-emacs-hiragana-face "black")
    ;; (set-face-foreground 'skk-emacs-katakana-face "green")
    ;; (set-face-background 'skk-emacs-katakana-face "black")

    ;; 候補表示
    ;; (setq skk-show-inline t)                          ; 変換候補の表示位置
    ;; (setq skk-show-tooltip t)                         ; 変換候補の表示位置
    (setq skk-show-candidates-always-pop-to-buffer t) ; 変換候補の表示位置
    (setq skk-henkan-show-candidates-rows 2) ; 候補表示件数を2列に

    ;; 動的候補表示
    (setq skk-dcomp-activate t)          ; 動的補完
    (setq skk-dcomp-multiple-activate t) ; 動的補完の複数候補表示
    (setq skk-dcomp-multiple-rows 10)    ; 動的補完の候補表示件数
    ;; 動的補完の複数表示群のフェイス
    (set-face-foreground 'skk-dcomp-multiple-face "Black")
    (set-face-background 'skk-dcomp-multiple-face "LightGoldenrodYellow")
    (set-face-bold-p 'skk-dcomp-multiple-face nil)
    ;; 動的補完の複数表示郡の補完部分のフェイス
    (set-face-foreground 'skk-dcomp-multiple-trailing-face "dim gray")
    (set-face-bold-p 'skk-dcomp-multiple-trailing-face nil)
    ;; 動的補完の複数表示郡の選択対象のフェイス
    (set-face-foreground 'skk-dcomp-multiple-selected-face "White")
    (set-face-background 'skk-dcomp-multiple-selected-face "LightGoldenrod4")
    (set-face-bold-p 'skk-dcomp-multiple-selected-face nil)
    ;; 動的補完時に下で次の補完へ
    (define-key skk-j-mode-map (kbd "<down>") 'skk-completion-wrapper)


    ;;変換学習機能の追加
    (require 'skk-study)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #custom
;; customize 変数の読み込み
;; ~/.emacs.d/custom.elがない場合は新規作成する。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-create-custom-file ()
  (cond ((file-exists-p custom-file)
         (load custom-file))
        (t
         (with-current-buffer (find-file-noselect custom-file)
           (save-buffer))
         )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #last-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-last-buffer ()
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

  (define-key global-map (kbd "C-t") 'switch-to-last-buffer-or-other-window)
  ;;(define-key overriding-local-map (kbd "C-t") 'switch-to-last-buffer-or-other-window)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #simple-screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-simple-screen ()
    (use-package simple-screen
      :config
      (global-set-key (kbd "C-c") 'simple-screen-map)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #e2wm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-e2wm ()
  (use-package e2wm
    :init
    (global-set-key (kbd "M-+") 'e2wm:start-management)
    :config

    (e2wm:add-keymap
     e2wm:pst-minor-mode-keymap
     '(("<M-left>" . e2wm:dp-code )                 ; codeへ変更
       ("<M-right>"  . e2wm:dp-two)                 ; twoへ変更
       ("<M-up>"    . e2wm:dp-doc)                  ; docへ変更
       ("<M-down>"  . e2wm:dp-dashboard)            ; dashboardへ変更
       ("C-."       . e2wm:pst-history-forward-command) ; 履歴進む
       ("C-,"       . e2wm:pst-history-back-command)    ; 履歴戻る
       ("C-M-s"     . e2wm:my-toggle-sub) ; subの表示をトグルする
       ("prefix L"  . ielm)         ; ielm を起動する（subで起動する）
       ("M-m"       . e2wm:pst-window-select-main-command) ; メインウインドウを選択する
       ) e2wm:prefix-key)

    (e2wm:add-keymap
     e2wm:dp-doc-minor-mode-map
     '(("prefix I" . info))             ; infoを起動する
     e2wm:prefix-key)

    (defun e2wm:my-toggle-sub ()        ; Subをトグルする関数
      (interactive)
      (e2wm:pst-window-toggle 'sub t 'main))

    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #my-which-key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-which-key ()
  (use-package which-key
    :config
    ;; 3つの表示方法どれか1つ選ぶ
    (which-key-setup-side-window-bottom) ;ミニバッファ
    ;; (which-key-setup-side-window-right)     ;右端
    ;; (which-key-setup-side-window-right-bottom) ;両方使う

    (which-key-mode 1)

    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #anything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-anything ()
    (use-package anything-config
      :config
      (setq anything-idle-delay 0.3)
      (setq anything-input-idle-delay 0)

      (global-set-key (kbd "M-x") 'anything-M-x)
      (global-set-key (kbd "C-x b") 'anything-buffers-list)
      ;;(global-set-key (kbd "C-x b") 'anything-mini)
      (global-set-key (kbd "C-x a r") 'anything-recentf)
      ;;(global-set-key (kbd "C-x a h b") 'anything-hatena-bookmark)
      (global-set-key (kbd "C-x a b") 'anything-bookmarks)
      (global-set-key (kbd "C-x a i") 'anything-imenu)


      (setq anything-enable-shortcuts 'prefix)
      (define-key anything-map (kbd "@") 'anything-select-with-prefix-shortcut)

      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'helm-config)
(defun my-helm ()
  (use-package helm-config
    :bind (("M-y" . helm-show-kill-ring) ; 過去のkillリングの内容を表示する。
           )

    ;; :init
    ;; ;; nothing
    ;; (message "helm-init")
    :config
    (message "helm-config")

    ;;(helm-dired-bindings 1)

    ;; nil:helmのファイル一覧でパスを表示する
    (setq helm-ff-transformer-show-only-basename nil)

    ;;(helm-mode t) ; なんでもhelm

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
    )

  ;; (use-package helm-gtags
  ;;   :config
  ;;   (add-hook 'c-mode-hook (lambda () (helm-gtags-mode)))
  ;;   (add-hook 'java-mode-hook (lambda () (helm-gtags-mode)))
  ;;   (add-hook 'jde-mode-hook (lambda () (helm-gtags-mode)))

  ;;   ;; customize
  ;;   (setq helm-c-gtags-path-style 'relative)
  ;;   (setq helm-c-gtags-ignore-case t)
  ;;   (setq helm-c-gtags-read-only nil)

  ;;   ;; 関数の定義元へ移動
  ;;   (local-set-key (kbd "C-c t j") 'helm-gtags-find-tag)
  ;;   ;; 関数を参照元の一覧を表示．RET で参照元へジャンプできる
  ;;   (local-set-key (kbd "C-c t r") 'helm-gtags-find-rtag)
  ;;   ;; 変数の定義元と参照元の一覧を表示．RET で該当箇所へジャンプできる．
  ;;   (local-set-key (kbd "C-c t s") 'helm-gtags-find-symbol)
  ;;   ;; すべてのシンボルから選択する
  ;;   (local-set-key (kbd "C-c t S") 'helm-gtags-select)
  ;;   ;; すべてのシンボルから選択する
  ;;   (local-set-key (kbd "C-c t p") 'helm-gtags-parse-file)
  ;;   ;; 元にもどる
  ;;   (local-set-key (kbd "C-c t b") 'helm-gtags-pop-stack))
  )

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
;; #company-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-company-mode ()
  (use-package company
    :config
    (global-company-mode)                  ; 全バッファで有効にする
    (setq company-idle-delay 0)            ; デフォルトは0.5
    (setq company-minimum-prefix-length 2) ; デフォルトは4
    (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る

    (bind-keys :map company-active-map
               ("M-n" . nil)
               ("M-p" . nil)
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               ("C-h" . nil))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #hydra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-hydra ()
  (use-package hydra
    :config
    (global-set-key
     (kbd "C-z")
     (defhydra hydra-move ()
       "move"
       ("f" forward-char "right")
       ("b" backward-char "left")
       ("n" next-line "down")
       ("p" previous-line "p")
       ("SPC" scroll-up-command "down")
       ("" scroll-down-command "up")
       ("." hydra-repeat "repeat")))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #edit-server
;; for Edit With Emacs(chrome)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-edit-server ()
  (use-package edit-server
    :config
    (setq edit-server-new-frame t)
    (edit-server-start)
    (setq network-coding-system-alist
          '(("nntp" . (junet-unix . junet-unix))
            (9292 . (utf-8 . utf-8))    ; edit-server文字化け対策
            (110 . (no-conversion . no-conversion))
            (25 . (no-conversion . no-conversion))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #emacs-server
;; for EmacsClient
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-emacs-server ()
  (use-package server
    :config
    (unless (server-running-p)
      (server-start))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; diredでWindowsに関連付けられたアプリを起動する
(defun uenox-dired-winstart ()
    "Type '[uenox-dired-winstart]': win-start the current line's file."
    (interactive)
    (if (eq major-mode 'dired-mode)
        (let ((fname (dired-get-filename)))
          (w32-shell-execute "open" fname)
          (message "win-started %s" fname))))

;; http://d.hatena.ne.jp/gan2/20070709/1184003949
;; フォルダを開く時, 新しいバッファを作成しない
;; バッファを作成したい時にはoやC-u ^を利用する
(defvar my-dired-before-buffer nil)
(defadvice dired-advertised-find-file
    (before kill-dired-buffer activate)
  (setq my-dired-before-buffer (current-buffer)))

(defadvice dired-advertised-find-file
    (after kill-dired-buffer-after activate)
  (if (eq major-mode 'dired-mode)
      (kill-buffer my-dired-before-buffer)))

(defadvice dired-up-directory
    (before kill-up-dired-buffer activate)
  (setq my-dired-before-buffer (current-buffer)))

(defadvice dired-up-directory
    (after kill-up-dired-buffer-after activate)
  (if (eq major-mode 'dired-mode)
            (kill-buffer my-dired-before-buffer)))

(defun my-dired ()
  (use-package dired
    :config

    ;; 2画面ファイラ化。
    (setq dired-dwim-target t)

    ;; ディレクトリを先に表示する
    (setq ls-lisp-dirs-first t)

    ))
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
        (setq org-directory (expand-file-name "~/Dropbox/org")))
    (if (equal "PC-18176" hostname)
        (setq org-directory (expand-file-name "~/docs/org")))
    ;; TODO org-directoryの判定は、現状のnilかどうかの判定から
    ;;      ディレクトリがあるかどうかに書き換える。
    (if (not (null org-directory))
        (progn
          (require 'em-glob)
          (setq org-agenda-files (eshell-extended-glob (concat org-directory "**/*.org")))))


    (defun my-org-meta-return (&optional prefix)
      (interactive "p") ; pは数引数(C-u)を受けとり、この場合はprefixに束縛される。
      (move-end-of-line nil)
      (org-insert-heading-dwim prefix))

    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #eww
;; http://rubikitch.com/2014/11/25/eww-image/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eww-disable-images ()
  "eww で画像表示させない"
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image-alt)
  (eww-reload))

(defun eww-enable-images ()
  "eww で画像表示させる"
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image)
  (eww-reload))

(defun shr-put-image-alt (spec alt &optional flags)
  (insert alt))

;; はじめから非表示
(defun eww-mode-hook--disable-image ()
  (setq-local shr-put-image-function 'shr-put-image-alt))

(defun my-eww ()
  (use-package eww
    :config
    ;;:commands (eww)
    (setq browse-url-browser-function 'eww-browse-url) ;; emacsの標準ブラウザとしてewwを指定する。
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

    (add-hook 'eww-mode-hook 'eww-mode-hook--disable-image)

    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #twitter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-twitter ()
  (use-package twittering-mode
    :config
    ;;
    ;; 認証
    ;;
    (setq twittering-auth-method 'oauth)
    (setq twittering-use-master-password t)
    (setq twittering-private-info-file (expand-file-name "~/.twittering-mode.gpg"))

    (bind-keys :map twittering-mode-map
               ("F" . twittering-favorite)
               ("R" . twittering-native-retweet)
               ("s" . twittering-search)
               ("o" . twittering-toggle-or-retrieve-replied-statuses))

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
          '(
            ;; 括弧を変更するとユーザにもとづくおすすめが表示されるぽい
            ;;"$related-to(hoge)"
            "zarudama_plus/geeks"
            "zarudama_plus/friend"
            ":search/clojure/"
            ":search/libgdx/"
            ":search/emacs/"
            ":search/lisp/"
            ":search/bash windows/"
            ":favorites"
            ":home"
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

    ;; %FOLD{
    ;;   %RT{
    ;;     %FACE[bold]{
    ;;       RT
    ;;   }
    ;; }
    ;; %i%S[%s] >> %r %C{%Y-%m-%d %H:%M:%S}
    ;; %@{}n
    ;; %FOLD[ ]{%T%RT{nretweeted by %s @%C{%Y-%m-%d %H:%M:%S}}}}
    (add-hook 'twittering-mode-hook
              (lambda ()
                (set-face-bold-p 'twittering-username-face t)
                (set-face-foreground 'twittering-username-face "DeepSkyBlue3")
                (set-face-foreground 'twittering-uri-face "gray60")
                ;; (setq twittering-status-format
                ;;       "%FOLD{%RT{%FACE[bold]{RT}}%i%s>>%r @%C{%Y-%m-%d %H:%M:%S} %@{}\n%FOLD[ ]{%T%RT{\nretweeted by %s @%C{%Y-%m-%d %H:%M:%S}}}}")
                (setq twittering-status-format
                      "%FOLD{%RT{%FACE[bold]{RT}}%i%S[%s] >> %r %C{%Y-%m-%d %H:%M:%S} %@{}[%f]\n%FOLD[ ]{%T%RT{\nretweeted by %s @%C{%Y-%m-%d %H:%M:%S}}}}")
                ))
    ;; ;; URL短縮サービスをj.mpに
    ;; ;; YOUR_USER_IDとYOUR_API_KEYを自分のものに置き換えてください
    ;; ;; from http://u.hoso.net/2010/03/twittering-mode-url-jmp-bitly.html
    ;; (add-to-list 'twittering-tinyurl-services-map
    ;;           '(jmp . "http://api.j.mp/shorten?version=2.0.1&login=YOUR_USER_ID&apiKey=YOUR_API_KEY&format=text&longUrl="))
    ;; (setq twittering-tinyurl-service 'jmp)

    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #elisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-elisp ()
  (find-function-setup-keys) ;; C-x F, C-x Kなどを使えるようにする。

  (use-package lispxmp
    :config
    (define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)
    (define-key lisp-interaction-mode-map (kbd "C-c C-d") 'lispxmp)
    )
  (use-package eldoc-extension
    :config
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))
  (use-package paredit
    :config
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'ielm-mode-hook 'enable-paredit-mode)
    )
  (use-package rainbow-delimiters
    :config
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-clojure ()
  (use-package cider
    :config
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

    (add-hook 'clojure-mode-hook 'cider-mode)
    ;;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-mode-hook 'eldoc-mode)
    )
  (use-package ac-cider
    :config
    (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
    (add-hook 'cider-mode-hook 'ac-cider-setup)
    (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
    )
  (use-package paredit
    :config
    (add-hook 'clojure-mode-hook 'enable-paredit-mode)
    (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
    )

  (use-package rainbow-delimiters
    :config
    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #slime
;; SLIME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-slime ()
  (use-package slime
    :config
    ;;(setq inferior-lisp-program "clisp -K full") ;; clispのREGEXPパッケージを読み込む場合はこっち。
    (setq inferior-lisp-program "clisp")
    (slime-setup '(slime-fancy slime-repl slime-banner))
    ;;(setq slime-contribs '(slime-fancy))
    (add-hook 'lisp-mode-hook 'slime-mode)
    (add-hook 'slime-mode-hook 'set-up-slime-ac)
    )
  (use-package ac-slime
    :config
    (add-hook 'slime-mode-hook 'set-up-slime-ac)
    (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
    )
  (use-package paredit
    :config
    (add-hook 'lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    (add-hook 'slime-mode-hook 'enable-paredit-mode)
    (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
    )
  (use-package rainbow-delimiters
    :config
    (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'slime-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scheme-other-window ()
  "Run Gauche on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(defun my-scheme ()
  ;;(setq scheme-program-name "gosh -i")
  ;;(setq scheme-program-name "/cygdrive/c/Users/mikio/bin/kawa")
  (setq scheme-program-name "java -cp c:/Users/mikio/bin/kawa-2.1.jar kawa.repl --console")

  (use-package cmuscheme
    :commands (scheme-mode run-scheme)
    :config
    (use-package paredit
      :config
      (add-hook 'scheme-mode-hook 'enable-paredit-mode)
      (add-hook 'inferior-scheme-mode-hook 'enable-paredit-mode))
    (use-package rainbow-delimiters
      :config
      (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
      (add-hook 'inferior-scheme-mode-hook 'rainbow-delimiters-mode)
      )))
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
;; #meghanada
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-meghanada()
  (use-package meghanada
    :config
    (add-hook 'java-mode-hook
              (lambda ()
                ;; meghanada-mode on
                (meghanada-mode t)
                ;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
                ))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #groovy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-groovy()
  (use-package groovy-mode
    :config
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #golang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-golang()
  (use-package go-mode
    :config
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    (add-hook 'go-mode-hook (lambda ()
                              (local-set-key (kbd "M-.") 'godef-jump)
                              (local-set-key (kbd "M-,") 'pop-tag-mark)
                              (flycheck-mode))))
  (use-package company-go
    :config
    (add-to-list 'company-backends 'company-go))
  (use-package go-eldoc
    :config
    (add-hook 'go-mode-hook 'go-eldoc-setup)))

;;
;; http://emacs-jp.github.io/programming/golang.html
;;
(defvar my/helm-go-source
  '((name . "Helm Go")
    (candidates . (lambda ()
                    (cons "builtin" (go-packages))))
    (action . (("Show document" . godoc)
               ("Import package" . my/helm-go-import-add)))))

(defun my/helm-go-import-add (candidate)
  (dolist (package (helm-marked-candidates))
    (go-import-add current-prefix-arg package)))

(defun my/helm-go ()
  (interactive)
  (helm :sources '(my/helm-go-source) :buffer "*helm go*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-web-mode()
  (use-package web-mode
    :mode (("\\.html?\\'" . web-mode)
           ("\\.jsp\\'"   . web-mode)
           ("\\.gsp\\'"   . web-mode))
    :config
    (add-hook 'web-mode-hook
              (lambda ()
                (setq web-mode-markup-indent-offset 2)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(my-environment-init)
(my-init)
;;(my-create-custom-file)
(my-last-buffer)
(my-simple-screen)
(my-whitespace)
(my-which-key)
;;(my-e2wm)
;;(my-helm)
(my-anything)
;;(my-auto-complete)
(my-company-mode)
(my-ddskk)
(my-hydra)
(my-edit-server)
;;(my-emacs-server)

(my-dired)
(my-org)
(my-eww)
(my-twitter)

(my-clojure)
(my-elisp)
(my-slime)
(my-scheme)
(my-java)
(my-meghanada)
;;(my-groovy) ;; emacs25.1でエラーがでる。
(my-golang)
(my-web-mode)
