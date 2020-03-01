{ghc}:
with (import <nixpkgs> {});
 
haskell.lib.buildStackProject {
    inherit ghc;
    name = "ScottyEnv";
    buildInputs = [pkgs.figlet zlib.dev zlib.out glpk pcre haskell.compiler.ghc865];
    shellHook = ''
    export PORT=3003;
    figlet "starts in "$PORT;
    '';
}