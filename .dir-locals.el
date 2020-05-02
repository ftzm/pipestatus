;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

;((haskell-mode . ((dante-repl-command-line . ("nix-shell" (concat
;								(expand-file-name (vc-root-dir)) "shell.nix") "--run" "cabal new-repl --builddir=dist/dante")))))

((nil . ((dante-methods . (impure-nix)))))
