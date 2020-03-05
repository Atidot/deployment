NIX_FLAGS=--cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz

build:
	nix-build ${NIX_FLAGS} nix/default.nix

build-js:
	nix-build ${NIX_FLAGS} nix/nodejs.nix

shell:
	nix-shell ${NIX_FLAGS} nix/default.nix

notebook:
	nix-shell ${NIX_FLAGS} -A env nix/notebook.nix --command "jupyter lab --notebook-dir=../examples/"

docker:
	nix-build ${NIX_FLAGS} nix/docker.nix -o docker.tar.gz

editor:
	nix-build ${NIX_FLAGS} -A ghcjs.atidot-deployment-editor nix/editor.nix
