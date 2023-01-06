{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    extraConfig = ''
      (custom-set-variables
       '(agenix-age-program "${pkgs.age}/bin/age"))
      (require 'agenix)
    '';
    extraPackages = epkgs: [epkgs.agenix];
  };
}
