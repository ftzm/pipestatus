;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode . ((dante-repl-command-line . ("nix-shell" "--attr" "pipestatus.env" "/home/matt/dev/pipestatus/release.nix" "--run" "cabal repl --builddir=dist/dante")))))
