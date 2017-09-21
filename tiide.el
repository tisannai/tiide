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
;;   tiide-refresh  - Update Tiide Config (after ".tiide.el" edit).
;;   tiide-build    - Build project.
;;   tiide-debug    - Start GDB within Emacs.
;;   tiide-edit     - Edit project's .gdbinit file in Emacs.
;;
;; Suggested global key bindings:
;;   (global-set-key (kbd "C-x 4 u") 'tiide-build)
;;   (global-set-key (kbd "C-x 4 g") 'tiide-debug)
;;   (global-set-key (kbd "C-x 4 e") 'tiide-edit)
;;   (global-set-key (kbd "C-x 4 i") 'tiide-get-breakpoint)


;; ------------------------------------------------------------
;; User configuration:

(require 'c-eldoc)

(defvar tiide-c-eldoc-include-base
   "`pkg-config gtk+-2.0 --cflags` -I./ -I../"
   "Default list of include dirs for c-eldoc. This list is
   extended with \"tiide-include\" from .tiide.el.")


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
               (list (cons 'tiide-gdbinit-file (format "%s/%s" root (cdr (assoc 'tiide-gdbinit config)))))
               (list (cons 'tiide-compile-dir (format "%s/%s" root (cdr (assoc 'tiide-compdir config)))))
               (list (cons 'tiide-gdbinit-dir (directory-file-name
                                                 (file-name-directory
                                                    (format "%s/%s" root (cdr (assoc 'tiide-gdbinit config)))))))
               (list (cons 'tiide-c-eldoc-include
                        (concat tiide-c-eldoc-include-base " "
                           (mapconcat 'identity
                              (mapcar
                                 (lambda (i)
                                    (if (string-match "^[a-zA-Z]" i)
                                       (format "-I%s/%s" root i)
                                       (format "-I%s" i)))
                                 (cdr (assoc 'tiide-include config))) " "))))))
         nil)))


(defun tiide-get-config ()
   "Get Tiide env configuration either from cache or refresh."
   (unless (local-variable-p 'tiide-config)
      (let ((config nil))
         (setq config (tiide-get-config-from-file))
         (if config
            (progn
               (make-local-variable 'tiide-config)
               (setq tiide-config config)))))
   (if (local-variable-p 'tiide-config)
      tiide-config
      nil))

(defun tiide-update-c-eldoc-includes ()
   "Update c-eldoc-includes variable with buffer relevant content."
   (let ((config (tiide-get-config)))
      (if config
         (setq c-eldoc-includes (cdr (assoc 'tiide-c-eldoc-include config))))))

(add-hook 'c-eldoc-get-buffer-hook 'tiide-update-c-eldoc-includes)
            


;; ------------------------------------------------------------
;; User functions:

(defun tiide-refresh ()
   "Update Tiide Config after config file edit."
   (interactive)
   (let ((config nil))
      (setq config (tiide-get-config-from-file))
      (if config
         (progn
            (unless (local-variable-p 'tiide-config)
               (make-local-variable 'tiide-config))
            (setq tiide-config config)))))

(defun tiide-debug ()
   "Start GDB using .gdbinit specified in Tiide config."
   (interactive)
   (let ((config (tiide-get-config)))
      (if config
         (gud-gdb (format "gdb --fullname -nx -x %s" (cdr (assoc 'tiide-gdbinit-file config)))))))


(defun tiide-edit ()
   "Edit .gdbinit file specified in Tiide config."
   (interactive)
   (let ((config (tiide-get-config)))
      (if config
         (find-file (cdr (assoc 'tiide-gdbinit-file config))))))


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
            (cd (cdr (assoc 'tiide-compile-dir config)))
            (compile (cdr (assoc 'tiide-compile config)))))))

(defun tiide-get-breakpoint ()
   "Storage current buffer and line info to yank buffer."
   (interactive)
   (let ((name (buffer-name (current-buffer)))
           (line (line-number-at-pos)))
      (kill-new (format "%s:%d" name line))))

(defun tiide-insert-config-template ()
   "Insert a template config file \".tiide.el\" to the current buffer."
   (interactive)
   (insert "(list
   '(tiide-compdir . \".\")
   '(tiide-compile . \"rake test:all\")
   '(tiide-gdbinit . \".gdbinit\")
   (cons 'tiide-include (list \"src\" (format \"%s/usr/include\" (getenv \"HOME\"))))
   )" ))

(provide 'tiide)
