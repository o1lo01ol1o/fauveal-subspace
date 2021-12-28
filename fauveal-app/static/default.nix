{ pkgs }:
let
  # The nixified node project was generated from a package.json file in src using node2nix
  # See https://github.com/svanderburg/node2nix#using-the-nodejs-environment-in-other-nix-derivations
  nodePkgs = (pkgs.callPackage ./src {
    inherit pkgs;
    nodejs = pkgs.nodejs-12_x;
  }).shell.nodeDependencies;

  # The frontend source files have to be passed in so that tailwind's purge option works
  # See https://tailwindcss.com/docs/optimizing-for-production#removing-unused-css
  frontendSrcFiles = ../frontend;

in pkgs.stdenv.mkDerivation {
  name = "static";
  src = ./src;
  buildInputs = [pkgs.nodejs];
  installPhase = ''
    mkdir -p $out/css
    mkdir -p $out/images

    # Setting up the node environment:
    ln -s ${nodePkgs}/lib/node_modules ./node_modules
    export PATH="${nodePkgs}/bin:$PATH"

    # bundle js deps using webpack
    cp -r js/entry.js $out/entry.js
    npm run build

    cp -r ./dist/bundle.min.js $out/bundle.min.js
    cp -r ./dist/bundle.js $out/bundle.js

    cp -r ./dist/bundle.min.js.map $out/bundle.min.js.map
    cp -r ./dist/bundle.js.map $out/bundle.js.map

    # We make the frontend haskell source files available here:
    # This corresponds to the path specified in tailwind.config.js
    ln -s ${frontendSrcFiles} frontend

    # Run the postcss compiler:
    postcss css/styles.css -o $out/styles.css

    # We can write other commands to produce more static files as well:
    cp -r images/* $out/images/

  '';
}
