let rfpath = ./dep/reflex-platform;
in { system ? builtins.currentSystem, obelisk ? import ./.obelisk/impl {
  reflex-platform-func = args@{ ... }:
    import rfpath (args // {
      inherit system;
      hlsSupport = true;
    });
  inherit system;
  iosSdkVersion = "13.2";
  config.android_sdk.accept_license = true;
  terms.security.acme.acceptTerms = true;
}, projectOverrides ? { }, withHoogle ? false }:
with obelisk;
project ./. ({ hackGet, pkgs, ... }@args:
  {
    inherit withHoogle;
    staticFiles = import ./static { pkgs = obelisk.nixpkgs; };
    android.applicationId = "fauveal.app";
    android.displayName = "fauveal.app";
    ios.bundleIdentifier = "fauveal.app";
    ios.bundleName = "fauveal.app";
    packages = {
    };
    shellToolOverrides = ghc: super: {
      inherit (pkgs) nixfmt;
      };

    overrides = (pkgs.lib.composeExtensions (import (hackGet ./dep/rhyolite) ({
      inherit (args) pkgs;
      inherit obelisk;
    })).haskellOverrides (self: super:
      with pkgs.haskell.lib;
      let
        isGHCJS = self.ghc.isGhcjs or false;
        dontCheckGHCJS = if isGHCJS then dontCheck else x: x;
        dontHaddockGHCJS = if isGHCJS then dontHaddock else x: x;
      in let
        githubRepo = fq: rev:
          builtins.fetchTarball
          ("https://github.com/" + fq + "/archive/" + rev + ".tar.gz");
        srcs = {
          # bitvec = githubRepo "Bodigrim/bitvec"
          #   "7150061b882967c7b31ee551771dd9f8ca226d6a";
          # thread-supervisor = githubRepo "nshimaza/thread-supervisor"
          #   "ac7e21eadd2489c85aa9a630ae89c629280032bc";
          # push-notifications = githubRepo "BeFunctional/push-notifications"
          #   "486531c5950df9fec8071310438584338d145d05";
          # vessel = githubRepo "BeFunctional/vessel"
          #   "1964fa7f8e8e4b8a1f6b1ad54283a68972a8b774";
        };
      in {
        active = dontCheck super.active;
        reflex = dontCheck super.reflex;
        # aeson-gadt-th = self.callHackage "aeson-gadt-th" "0.2.5.1" { };
        # linear = self.callHackage "linear" "1.21.4" { };
        # vessel= self.callHackageDirect {
        #   pkg = "vessel";
        #   ver = "0.2.1.0";
        #   sha256 = "k5OBmTd94O4cWPW3PvtrhMDtKYRvfVnAmdh0sRPbG/c=";
        # } { };
        # universe-some = self.callHackage "universe-some" "1.2.1" { };
        servant = self.callHackage "servant" "0.18.3" { };
        servant-server = self.callHackage "servant-server" "0.18.3" { };
        servant-client = self.callHackage "servant-client" "0.18.3" { };
        servant-client-core = self.callHackage "servant-client-core" "0.18.3" { };

        http-media = dontCheck super.http-media;
        # base-orphans  = self.callHackage "base-orphans" "0.8.6" { };
        diagrams-contrib = dontCheck super.diagrams-contrib;
        # network = dontCheck (self.callHackage "network" "3.0.0.1" {});
        # constraints = self.callHackage "constraints" "0.10.1" {};
        # monad-logger = self.callHackage "monad-logger" "0.3.36" {};
        # # unliftio-core  = self.callHackage "unliftio-core" "0.1.2.0" {};
        # cryptonite = self.callHackage "cryptonite" "0.26" {};
        # singletons = dontCheck (self.callHackage "singletons" "2.5.1" {});
        # # th-desugar = self.callHackage "th-desugar" "1.9" {};
        # hspec-wai = self.callHackage "hspec-wai" "0.9.0" {};
        # bitvec = (self.callCabal2nix "bitvec" srcs.bitvec { });
        # json-autotype = dontCheck (self.callHackage "json-autotype" "3.1.2" {});
        # ghc-typelits-presburger = self.callHackageDirect {
        #   pkg = "ghc-typelits-presburger";
        #   ver = "0.4.0.0";
        #   sha256 = "04ssl4y17mvm3ggnkjn705s1b4a7g7fi4sm7zw96970a8jdnbfs3";
        # } { };
        # subcategories = self.callHackageDirect {
        #   pkg = "subcategories";
        #   ver = "0.1.1.0";
        #   sha256 = "0q4h7420z6bjvdgrn0wjz3qmmp2n8xqxc159rdsgzjaam356xkxs";
        # } { };
        # sized = self.callHackageDirect {
        #   pkg = "sized";
        #   ver = "1.0.0.0";
        #   sha256 = "05md5m79wij3fh76xbq9yms953164145smhd9bvkngs4z91vbbyc";
        # } { };
        # network-bsd = self.callHackageDirect {
        #   pkg = "network-bsd";
        #   ver = "2.8.1.0";
        #   sha256 = "103vvbcrsklrjgd7a0ic99h1iaj04lyxf3y874m9kgazwj7a3vcp";
        # } { };
        # HaskellNet-SSL = self.callHackageDirect {
        #   pkg = "HaskellNet-SSL";
        #   ver = "0.3.4.4";
        #   sha256 = "05mh7ydf3ngr32wnxk896sj6c9gcddr4m5af2dbs1yzpjfr77d30";
        # } { };
        # these = self.callHackage "these" "1.1.1.1" {};
        # witherable = self.callHackage "witherable" "0.3.2" {};
        # generic-data = dontCheck (self.callHackageDirect {
        #   pkg = "generic-data";
        #   ver = "0.8.3.0";
        #   sha256 = "18l3mg9fclsqha0n9zjgyj6kj9r211x0lc7a4mmzskc41ykwnxcw";
        # } { });
        # vessel = dontCheck
        #   (self.callCabal2nix "vessel"
        #     (srcs.vessel) { });
        # push-notifications = dontCheck
        #   (self.callCabal2nix "push-notifications"
        #     (srcs.push-notifications) { });
        # wide-word = dontCheck (self.callHackageDirect {
        #   pkg = "wide-word";
        #   ver = "0.1.1.2";
        #   sha256 = "16czh619fiwximz3sxk32f2776saqvr0yrz6kv3bddwxhwxj09rf";
        # } { });
        # ip = dontCheck (super.ip); # marked as broken
      }));
  } // projectOverrides)