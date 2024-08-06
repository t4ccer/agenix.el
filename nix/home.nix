## An example Home Manager configuration, to see how to use the package.
## This also serves as a test for the overlays, etc.
{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    extraConfig = "(require 'agenix)";
    extraPackages = epkgs: [epkgs.agenix];
  };

  # These attributes are simply required by home-manager.
  home = {
    homeDirectory = /tmp/example;
    stateVersion = "24.05";
    username = "example-user";
  };
}
