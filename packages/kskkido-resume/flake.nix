{
  description = "kskkido resume";
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
  };
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
        with pkgs;
        let 
          latex = texlive.combine {
            inherit (texlive)
              scheme-small
              footmisc
              pagecolor
              etoolbox
              xcolor
              tcolorbox
              lastpage
              fancyhdr
              polyglossia
              xunicode
              xltxtra
              xecjk
              marginnote
              sectsty
              hyperref
              environ
              trimspaces
              tex-gyre
              hardwrap
              catchfile
              tikzfill
              alegreya;
          };
          fontsConfig = makeFontsConf {
            fontDirectories = [ "${alegreya-sans}/share/fonts/otf" ];
          };
          outputDir = "public";
          resumeTex = "resume.tex";
          resumeEnSrc = "resume-en.yml";
          resumeEnPdf = "resume-en.pdf";
          resumeJaSrc = "resume-ja.yml";
          resumeJaPdf = "resume-ja.pdf";
          coverLetterTex = "cover-letter.tex";
          coverLetterSrc = "cover-letter.yml";
          coverLetterPdf = "cover-letter.pdf";
        in
          rec {
            packages = rec {
              resume-to-pdf = runCommand "resume-to-pdf"
                {
                  file = writeShellScript "resume-to-pdf-internal"
                    ''
                      export FONTS_CONFIG=${fontsConfig}
                      mkdir -p ${outputDir}
                      pandoc -f markdown ${resumeEnSrc} -o ${outputDir}/${resumeEnPdf} --template=${resumeTex} --pdf-engine=xelatex
                    '';
                  buildInputs = [makeWrapper];
                }
                ''
                  makeWrapper "$file" "$out/bin/resume-to-pdf" --set PATH ${lib.strings.makeBinPath [
                    toybox
                    latex
                    librsvg
                    pandoc
                  ]}
                '';
              default = resume-to-pdf;
            };
            devShell = mkShell {
              buildInputs = with packages; [
                latex
                librsvg
                pandoc
                resume-to-pdf
              ];
            shellHook =
              ''
                echo "Hello shell"
              '';
            };
          }
    );
}

