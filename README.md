# Tiide

Tiide is a minimalistic C language IDE for Emacs. It integrates
c-eldoc, compile and gud-gdb packages to users C project development.

Tiide enables the user to launch compilation and debugging from the
current buffer. Current buffer identifies the Tiide Project
indirectly. Tiide finds the Project by traveling up in directory
hierarchy from the file of current buffer.

Project Root directory is identified by placing ".tiide.el" file to
the root directory. ".tiide.el" is called Tiide Config. Tiide Project
covers all files below the Project Root.

Tiide provides following commands:

* tiide-refresh: update Tiide Config cache after editing ".tiide.el".

* tiide-debug: run gud-gdb with Project's ".gdbinit" file.

* tiide-edit: open Project's ".gdbinit" for editing.

* tiide-build: build Project.


User is advised to map these commands directly to keys. For example:

    (global-set-key (kbd "C-x 4 u") 'tiide-build)
    (global-set-key (kbd "C-x 4 g") 'tiide-debug)
    (global-set-key (kbd "C-x 4 e") 'tiide-edit)
    (global-set-key (kbd "C-x 4 i") 'tiide-get-breakpoint)

Tiide Config includes information about the users dev env.

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

This configuration is for a project that uses Ceedling for
testing. "src" include source files and tests are in "test" directory.

The configuration specifies:

* compilation is performed at Root directory, and

* compilation is performed with "rake test:all" command,

* ".gdbinit" file is in the Root directory,

* project include files are in "<root>/src", "<root>/test", and
  "<root>/vendor/ceedling/vendor/unity/src" directories. There are
  additional includes taken from users home directory.

"tiide-compdir" and "tiide-compile" are used by "compile"
command. "tiide-gdbinit" is used by "gud-gdb".

"tiide-include" is used by "c-eldoc" indirectly. Tiide updates
"c-eldoc-includes" variable before the function prototype lookup is
performed. Relative paths are extended with Project Root, and absolute
paths are used as is. User should setup the base includes to
"tiide-c-eldoc-include-base" variable, for example in ".emacs":

    (setq tiide-c-eldoc-include-base "`pkg-config gtk+-2.0 --cflags` -I./ -I../")

The base is extended by Project specific "-I" entries. This ensures
that all function prototypes are found by "c-eldoc".


Tiide library by Tero Isannainen, (c) Copyright 2017.
