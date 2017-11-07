;;; tiide.el -- TI Integrated Design Environment.

;; Copyright (C) 2017 Tero Isannainen

;; Tiide package includes features that makes the use of gud-gdb and
;; c-eldoc convenient.
;;
;; Tiide env configuration is read from ".tiide.el". Configuration is
;; called Tiide Config. ".tiide.el" file is placed to the root dir of
;; project and it is called Tiide Root. The file defines relative
;; locations for relevant files and also the command used to compile
;; the project.
;;
;; Example:
;;    '(
;;        (tiide-gdbinit . ".gdbinit")
;;        (tiide-compile . "rake test:all")
;;        (tiide-compdir . ".")
;;        (tiide-include . ("src" "test" "vendor/ceedling/vendor/unity/src"))
;;        )
;;
;; "tiide-gdbinit" specifies the relative path from Tiide Root to
;; ".gdbinit" file.
;;
;; "tiide-compile" specifies the project's compilation command.
;;
;; "tiide-compdir" specifies the project's compilation directory.
;;
;; "tiide-include" specifies relative paths that should be part of
;; "c-eldoc" include paths. These are required in order to show
;; function signatures in minibuffer when cursor is at function call
;; in source file buffer.
;;
;; User has the following commands available:
;;   tiide-refresh      - Update Tiide Config (after ".tiide.el" edit).
;;   tiide-build        - Build project.
;;   tiide-debug        - Start GDB within Emacs.
;;   tiide-edit-gdbinit - Edit project's .gdbinit file in Emacs.
;;   tiide-edit-config  - Edit project's Config file in Emacs.
;;
;; Suggested global key bindings:
;;   (global-set-key (kbd "C-x 4 u") 'tiide-build)
;;   (global-set-key (kbd "C-x 4 g") 'tiide-debug)
;;   (global-set-key (kbd "C-x 4 e") 'tiide-edit-gdbinit)
;;   (global-set-key (kbd "C-x 4 i") 'tiide-get-breakpoint)
;;   (global-set-key (kbd "C-x 4 t") 'tiide-edit-config)


;; ------------------------------------------------------------
;; User configuration:

(require 'c-eldoc)
(require 'revive)

(defvar tiide-c-eldoc-include-base
    "`pkg-config gtk+-2.0 --cflags` -I./ -I../"
    "Default list of include dirs for c-eldoc. This list is
extended with \"tiide-include\" from .tiide.el.")

(defvar tiide-config-cache
    '()
    "Cache of configs for all Tiid Projects. Perform tiide-refresh
in order to refresh cache for the current project.")


;; ------------------------------------------------------------
;; Tiide internal implementation:

(defun tiide-find-root ()
    "Find and return .tiide.el file from dirs above, or nil if not found."
    (let ((curdir (expand-file-name ".")) (tiide-conf nil))
        (catch 'exit
            (while (not (string= "/" curdir))
                (let ((files (directory-files curdir)))
                    (while (car files)
                        (if (string= (car files) ".tiide.el")
                            (progn
                                (setq tiide-conf (car files))
                                (throw 'exit "dummy")))
                        (setq files (cdr files))))
                (setq curdir (directory-file-name (file-name-directory curdir)))))
        (if (string= curdir "/")
            nil
            curdir)))

(defun aget (key list)
    "Get value from associated list."
    (cdr (assoc key list)))


(defun tiide-eldoc-ctrl (active)
    "Enable or disable eldoc mode."
    (if active
        (eldoc-mode 1)
        (eldoc-mode -1)))


(defun tiide-get-config-from-file ()
    "Get all information related to Tiide env."
    (let ((root (tiide-find-root))
             (config))
        (if root
            (progn
                (setq config
                    (eval (car
                              (read-from-string
                                  (with-temp-buffer
                                      (insert-file-contents (format "%s/.tiide.el" root))
                                      (buffer-string))))))
                (append config
                    (list (cons 'tiide-root root))
                    (list (cons 'tiide-gdbinit-file (format "%s/%s" root (aget 'tiide-gdbinit config))))
                    (list (cons 'tiide-compile-dir (format "%s/%s" root (aget 'tiide-compdir config))))
                    (list (cons 'tiide-gdbinit-dir (directory-file-name
                                                       (file-name-directory
                                                           (format "%s/%s" root (aget 'tiide-gdbinit config))))))
                    (list (cons 'tiide-c-eldoc-include
                              (concat tiide-c-eldoc-include-base " "
                                  (mapconcat 'identity
                                      (mapcar
                                          (lambda (i)
                                              (if (string-match "^[a-zA-Z]" i)
                                                  (format "-I%s/%s" root i)
                                                  (format "-I%s" i)))
                                          (aget 'tiide-include config)) " "))))))
            nil)))


(defun tiide-get-config ()
    "Get Tiide env configuration either from cache or refresh."
    (unless (local-variable-p 'tiide-config)
        (let ((config nil))
            (setq config (tiide-get-config-from-file))
            (if config
                (progn
                    (tiide-eldoc-ctrl (aget 'tiide-doeldoc config))
                    (make-local-variable 'tiide-config)
                    (setq tiide-config (intern (aget 'tiide-root config)))
                    (unless (aget tiide-config tiide-config-cache)
                        (setq tiide-config-cache
                            (append tiide-config-cache (list (cons tiide-config config)))))))))
    (if (local-variable-p 'tiide-config)
        (aget tiide-config tiide-config-cache)
        nil))


(defun tiide-update-c-eldoc-includes ()
    "Update c-eldoc-includes variable with buffer relevant content."
    (let ((config (tiide-get-config)))
        (if config
            (setq c-eldoc-includes (aget 'tiide-c-eldoc-include config)))))

(add-hook 'c-eldoc-get-buffer-hook 'tiide-update-c-eldoc-includes)



;; ------------------------------------------------------------
;; User functions:

(defun tiide-refresh ()
    "Update Tiide Config after config file edit."
    (interactive)
    (let ((config nil) (rem nil) (cache nil))
        (setq config (tiide-get-config-from-file))
        (if config
            (progn
                (unless (local-variable-p 'tiide-config)
                    (make-local-variable 'tiide-config))
                (tiide-eldoc-ctrl (aget 'tiide-doeldoc config))
                (setq tiide-config (intern (aget 'tiide-root config)))

                ;; Search the old config by root info and store to rem.
                (seq-each
                    '(lambda (item)
                         (unless (equal (intern (aget 'tiide-root item)) tiide-config)
                             (setq cache (append cache (list item)))))
                    tiide-config-cache)

                (setq tiide-config-cache (append cache (list (cons tiide-config config))))))))


(defun tiide-debug ()
    "Start GDB using .gdbinit specified in Tiide config."
    (interactive)
    (let ((config (tiide-get-config)))
        (if config
            (gud-gdb (format "gdb --fullname -nx -x %s" (aget 'tiide-gdbinit-file config))))))


(defun tiide-edit-gdbinit ()
    "Edit .gdbinit file specified in Tiide config."
    (interactive)
    (let ((config (tiide-get-config)))
        (if config
            (find-file (aget 'tiide-gdbinit-file config)))))


(defun tiide-edit-config ()
    "Edit .tiide.el file."
    (interactive)
    (let ((root (tiide-find-root)))
        (if root
            (find-file (format "%s/.tiide.el" root)))))


(defun tiide-build ()
    "Build/compile based on Tiide config."
    (interactive)
    (let ((config (tiide-get-config)))
        (if config
            (with-temp-buffer
                (cd (aget 'tiide-compile-dir config))
                (compile (aget 'tiide-compile config))))))


(defun tiide-get-breakpoint ()
    "Storage current buffer and line info to yank buffer."
    (interactive)
    (let ((name (file-name-nondirectory (buffer-name (current-buffer))))
             (line (line-number-at-pos)))
        (kill-new (format "%s:%d" name line))
        (let ((config (tiide-get-config)))
            (if config
                (if (file-exists-p (aget 'tiide-gdbinit-file config))
                    (progn
                        (find-file (aget 'tiide-gdbinit-file config))
                        (end-of-buffer)
                        (if (search-backward-regexp "^run")
                            (progn
                                (beginning-of-line)
                                (forward-line -1)
                                (insert (format "break %s\n" (car kill-ring)))
                                (save-buffer)
                                ))
                        (switch-to-buffer name)))))))


(defun tiide-insert-config-template ()
    "Insert a template config file \".tiide.el\" to the current buffer."
    (interactive)
    (insert "(list
   '(tiide-compdir . \".\")
   '(tiide-compile . \"rake test:all\")
   '(tiide-doeldoc . t)
   '(tiide-gdbinit . \".gdbinit\")
   (cons 'tiide-include (list \"src\" (format \"%s/usr/include\" (getenv \"HOME\"))))
   )" ))


(defun tiide-create-gdbinit-template ()
    "Insert a template config file \".gdbinit\" to configured location."
    (interactive)
    (let ((config (tiide-get-config)))
        (if config
            (if (file-exists-p (aget 'tiide-gdbinit config))
                (message "You already have .gdbinit... no operation performed...")
                (progn
                    (find-file (aget 'tiide-gdbinit config))
                    (insert "file <path-to-bin>
set args <cmd-line-args>

break main

run
"
                        ))))))

(defun tiide-save-windows (&optional filename)
    "Save window config to file, default .tiide.windows."
    (interactive)
    (let ((file (concat (tiide-find-root) "/.tiide.windows" )))
        (if filename
            (setq file filename))
        (setq buf (create-file-buffer file))
        (set-buffer buf)
        (erase-buffer)
        (insert (prin1-to-string (current-window-configuration-printable)))
        (write-file (expand-file-name file))
        (kill-this-buffer)))

(defun tiide-load-windows (&optional filename)
    "Load window config from file, default .tiide.windows."
    (interactive)
    (let ((file (concat (tiide-find-root) "/.tiide.windows" )))
        (if filename
            (setq file filename))
        (restore-window-configuration
            (read
                (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string))))))

(provide 'tiide)
