{ pkgs ? import <nixpkgs> {}}:

pkgs.mkShell {
    buildInputs = with pkgs; [
        rustc
        cargo
        clippy
        rustfmt
        openssl
        pkg-config
    ];
}
