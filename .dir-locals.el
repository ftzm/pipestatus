;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode . ((dante-repl-command-line . ("nix-shell" "--attr"
					      "pipestatus.env" (concat
					      (expand-file-name dante-project-root) "release.nix") "--run" "cabal new-repl --builddir=dist/dante")))))
