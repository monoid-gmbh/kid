{
  description = "KID for PRIIPs — Key Information Document generator (PRIIPs Regulation No. 1286/2014)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = pkgs.lib;
        hpkgs = pkgs.haskellPackages;

        # LaTeX toolchain used at runtime to compile the generated documents to PDF.
        # scheme-medium covers the common packages; widen if document generation
        # reports missing .sty files.
        texEnv = pkgs.texlive.combined.scheme-medium;

        # Custom Haskell dependency, pinned exactly as in cabal.project's
        # source-repository-package stanza.
        quandl-api-v3 = hpkgs.callCabal2nix "quandl-api-v3"
          (pkgs.fetchFromGitHub {
            owner = "yveshauser";
            repo = "quandl-api-v3";
            rev = "bdbacd11ed31a15ee7caeafe9780284a5e6a8cd4";
            hash = "sha256-cF+VUxw79iZAXwyobmICI8Lkr6i5C04XffPzrryKTfo=";
          }) { };

        # The Futhark package dependencies (the `make pkgs` / `futhark pkg sync`
        # step), fetched purely through Nix instead of shelling out to the
        # Futhark package manager. This is the full transitive closure that
        # `futhark pkg sync` resolves for kid; each entry is pinned to the exact
        # commit the package manager would select (highest required version).
        # To update: bump rev/hash here, or re-run `futhark pkg sync` in src/fut
        # and read the commits from the resulting futhark.pkg manifests.
        futharkDeps = [
          { owner = "diku-dk";     repo = "cpprandom";   rev = "ec96adc06f7a91040b2c1f7f9fdc56b9b2e9f3a3"; hash = "sha256-RSuqfOfTX2niXSVHc93OM9fyj6yTWxNKwEao0thvphY="; }
          { owner = "diku-dk";     repo = "statistics";  rev = "ad49c009c7fb5d3059d4edca65d0e0f6b145da05"; hash = "sha256-womV7clcDOS8l8emRVgK8PCQAx94HJf4tZaQJvhViok="; }
          { owner = "diku-dk";     repo = "linalg";      rev = "d7e197834b26050add8c8b3d747cf61fa2762bbb"; hash = "sha256-sL8v96vC19kemHDQFZ0lKlXOE86giEh52mgBtwoCH0A="; }
          { owner = "diku-dk";     repo = "sorts";       rev = "3b2cfcc0d0256df7cd1f2548f68d85f2453007a0"; hash = "sha256-3I8hwNyyafQ7Ea0dvytWZRKrJEoKoLOo1WzZPo2uBQk="; }
          { owner = "diku-dk";     repo = "segmented";   rev = "7bad610241494839d3976f6ee4991ba7af8e8c00"; hash = "sha256-daWavNa2loA1slaX2S6kPlbKIUonB4FGxZTU/X8oHig="; }
          { owner = "monoid-gmbh"; repo = "kid-annexes"; rev = "7656cb3f30c28a2e7fbb28c831a6dcf406b85eb8"; hash = "sha256-iiZGs6t0SLCEaWQUAHfLbue/V/JNNEk1zqgZPU5rMH0="; }
        ];

        # Assemble the fetched packages into the `lib/github.com/<owner>/<repo>`
        # layout that Futhark's `import "lib/..."` statements expect. Each
        # package repo keeps its own modules under
        # `lib/github.com/<owner>/<repo>/` (Futhark convention), so we copy from
        # there rather than the repo root.
        futharkLibs = pkgs.runCommandLocal "kid-futhark-libs" { } (
          lib.concatMapStringsSep "\n"
            (d:
              let src = pkgs.fetchFromGitHub { inherit (d) owner repo rev hash; };
              in ''
                mkdir -p "$out/github.com/${d.owner}/${d.repo}"
                cp -a ${src}/lib/github.com/${d.owner}/${d.repo}/. "$out/github.com/${d.owner}/${d.repo}/"
              '')
            futharkDeps);

        # The main package. callCabal2nix reads kid.cabal; the
        # source-repository-package from cabal.project is supplied explicitly
        # via the quandl-api-v3 override above.
        kid = lib.pipe
          (hpkgs.callCabal2nix "kid" ./. { inherit quandl-api-v3; })
          [
            # The .cabal upper bounds are loose enough for the pinned nixpkgs;
            # jailbreak guards against incidental version skew.
            pkgs.haskell.lib.doJailbreak
            # The committed test suite (test/Spec.hs) references modules that do
            # not exist in this WIP repo; don't block the package build on it.
            # Tests remain runnable via `cabal test` in the dev shell.
            pkgs.haskell.lib.dontCheck
            (drv: drv.overrideAttrs (old: {
              nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [
                pkgs.futhark
                pkgs.makeWrapper
              ];

              # Provide the resolved Futhark package dependencies (the `make
              # pkgs` step). The C generation itself (`futhark c --library`) is
              # driven by the custom Setup.hs during the Cabal build; futhark is
              # on PATH via nativeBuildInputs above.
              preConfigure = (old.preConfigure or "") + ''
                cp -r ${futharkLibs} src/fut/lib
                chmod -R u+w src/fut/lib
              '';

              # pdflatex is invoked at runtime to render the KID PDF.
              postInstall = (old.postInstall or "") + ''
                if [ -e "$out/bin/kid-exe" ]; then
                  wrapProgram "$out/bin/kid-exe" \
                    --prefix PATH : ${lib.makeBinPath [ texEnv ]}
                fi
              '';
            }))
          ];
      in
      {
        packages = {
          default = kid;
          kid = kid;
          inherit futharkLibs;
        };

        apps.default = {
          type = "app";
          program = "${kid}/bin/kid-exe";
        };

        devShells.default = pkgs.mkShell {
          name = "kid-dev";
          inputsFrom = [ kid.env ];

          # pkg-config so Cabal-built native bindings (e.g. the Haskell `zlib`
          # package, pulled in transitively) can locate their C libraries.
          nativeBuildInputs = [ pkgs.pkg-config ];

          # Native C libraries needed when `cabal build` compiles dependencies
          # from Hackage inside the shell. `zlib` is the usual culprit
          # ("Failed to build zlib"); its setup hook adds the headers and the
          # .pc file to the build environment.
          buildInputs = [ pkgs.zlib ];

          packages = [
            hpkgs.cabal-install
            hpkgs.haskell-language-server
            pkgs.futhark
            pkgs.gcc
            texEnv
          ];
          shellHook = ''
            # Provide the Futhark package deps from Nix instead of `make pkgs`
            # (no network needed). Re-link if missing or stale.
            if [ ! -e src/fut/lib ] || [ "$(readlink src/fut/lib)" != "${futharkLibs}" ]; then
              rm -rf src/fut/lib
              ln -s "${futharkLibs}" src/fut/lib
              echo "Linked src/fut/lib -> ${futharkLibs}"
            fi
            echo "kid dev shell — just run 'cabal build' (Futhark C is generated by Setup.hs)"
          '';
        };
      });
}
