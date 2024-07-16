let
  onix = import (builtins.fetchGit {
    url = "https://github.com/rizo/onix.git";
    rev = "965220946cb33005e60ab568cf8dade3371d8150";
  }) { verbosity = "info"; };

in onix.env {
  path = ./.;

  vars = {
    "with-test" = true;
    "with-doc" = true;
    "with-dev-setup" = true;
  };

  deps = {
    "ocaml-system" = "*";
    "ocamlformat" = "*";
    "ocaml-lsp-server" = "*";
  };
}
