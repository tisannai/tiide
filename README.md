# Tiide

Tiide is a minimalistic C language IDE for Emacs. It integrates
c-eldoc, compile and gud-gdb packages as C project development
environment.

Tiide enables the user to use all features of Tiide from any buffer
that exists within the Tiide Project hierarchy. User can stay in the
current buffer (and within Emacs) for all basic iterations regarding
program development: edit, (re-config), compile, run/debug.

Current buffer is under Tiide Project Root, in directory
hierarchy. Tiide finds the Project Root by traveling up in directory
hierarchy from the directory that includes the file of current buffer.

Project Root directory is identified by placing ".tiide.el" file to
Root directory. ".tiide.el" is called Tiide Config. Tiide Project
contains all files recursively under Project Root.

Tiide provides following commands (among others):

* tiide-refresh: update Tiide Config cache after editing ".tiide.el".

* tiide-debug: run gud-gdb with Project's ".gdbinit" file.

* tiide-edit-gdbinit: open Project's ".gdbinit" for editing.

* tiide-build: build Project.

* tiide-edit-config: open Project's Config for editing.


User is advised to map these commands directly to keys. For example:

    (global-set-key (kbd "C-x 4 u") 'tiide-build)
    (global-set-key (kbd "C-x 4 g") 'tiide-debug)
    (global-set-key (kbd "C-x 4 e") 'tiide-edit-gdbinit)
    (global-set-key (kbd "C-x 4 i") 'tiide-get-breakpoint)
    (global-set-key (kbd "C-x 4 t") 'tiide-edit-config)


Tiide Config includes information about the users development
environment.

Example configuration (".tiide.el content"):

    (list
       '(tiide-compdir . ".")
       '(tiide-compile . "rake test:all")
       '(tiide-gdbinit . ".gdbinit")
       (cons 'tiide-include (list
                               "src"
                               "test"
                               "vendor/ceedling/vendor/unity/src"
                               (format "%s/usr/include" (getenv "HOME"))))
       )

Configuration is a list of cons cells, i.e. an association list.

This configuration is for a project that uses Ceedling for
testing. "src" includes source files and tests are in "test"
directory.

The configuration specifies:

* compilation is performed at Root directory,

* compilation is performed with "rake test:all" command,

* ".gdbinit" file is in the Root directory, and

* project include files are in "<root>/src", "<root>/test", and
  "<root>/vendor/ceedling/vendor/unity/src" directories. There are
  also additional includes taken from users home directory.

"tiide-compdir" and "tiide-compile" are used by Emacs "compile"
command. "tiide-gdbinit" is used by "gud-gdb".

"tiide-include" is used by "c-eldoc" indirectly. Tiide updates
"c-eldoc-includes" variable before the function signature lookup is
performed. Relative paths are extended with Project Root, and absolute
paths are used as is. User should list common includes in
"tiide-c-eldoc-include-base" variable (e.g. in ".emacs"):

    (setq tiide-c-eldoc-include-base "`pkg-config gtk+-2.0 --cflags` -I./ -I../")

Variable is augmented by Tiide with Project specific "-I"
entries. This ensures that all function signatures are found by
"c-eldoc" within the Project.


Tiide library by Tero Isannainen, (c) Copyright 2017.
