let
  onix = import (builtins.fetchGit {
    url = "https://github.com/rizo/onix.git";
    rev = "7fbdec45e3c6f8db2eb5fa0c41fcd94c67babf37";
  }) { verbosity = "info"; };

in onix.env {
  path = ./.;

  vars = {
    "with-test" = true;
    "with-doc" = true;
    "with-dev-setup" = true;
  };

  deps = {
    "ocaml-base-compiler" = "<5.4";
    "ocaml-lsp-server" = "*";
    "ocamlformat" = "*";
  };
}
