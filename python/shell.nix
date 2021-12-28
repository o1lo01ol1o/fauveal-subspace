let
  mach-nix = import (builtins.fetchGit {
    url = "https://github.com/DavHau/mach-nix";
    ref = "refs/tags/3.3.0";
  }) {
      python = "python38";
      pypiDataRev = "2b20310d37bbd2deefcfb549baa9ac5272e17715";
      };

  pyEnv = mach-nix.mkPython rec {
    requirements = ''
      numpy>=1.17
      scipy>=1.3.1
      scikit-learn>=0.22
      numba>=0.51.2
      pynndescent>=0.5
      tbb>=2019.0
      tqdm
      eth-account
      click
      umap-learn>=0.5.0
      pillow
      PyScaffold
      jupyterlab
      pandas
      matplotlib
      setuptools
      seaborn
      setuptools-scm
      pytest
      opencv-python
      autopep8
      torch
      pip
      scikit-image
      hdbscan>=0.8.20
      beautifulsoup4
      datashader
      bokeh
      holoviews
      colorcet
      pyscaffoldext-dsproject
    '';
    providers.numpy = "nixpkgs";
    providers.scipy = "nixpkgs";
    providers.numba = "nixpkgs";
  };
  in
  mach-nix.nixpkgs.mkShell {

  buildInputs = [
    mach-nix.nixpkgs.pkgconfig
    pyEnv
    mach-nix.nixpkgs.opencv
  ] ;

  shellHook = ''
    jupyter lab --notebook-dir=/Users/timpierson/arity/fauveal-subspace/python/munger/notebooks
  '';
}
