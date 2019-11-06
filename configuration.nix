{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.kernelModules = [ "iwlwifi" ];
  boot.extraModprobeConfig = ''
      options iwlwifi bt_coex_active=0 power_save=Y 11n_disable=8 wd_disable=1 
    '';
  nixpkgs.config.allowUnfree = true;
  hardware.enableRedistributableFirmware = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/sda2";
      preLVM = true;
    }
  ];

  networking.hostName = "freni"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.useDHCP = false;
  networking.interfaces.enp0s25.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Belgrade";

  environment.systemPackages = with pkgs; [
    cargo
    firefox
    emacs
    git
    openjdk11
    leiningen
    compton
    ripgrep
    rxvt_unicode-with-plugins     
    wget
    vim
    which
    zsh
  ];

  fonts.fonts = with pkgs; [
    terminus_font
    liberation_ttf
  ];

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  services.printing.enable = true;
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "ctrl:nocaps";
  services.xserver = {

    displayManager.sessionCommands = ''
      ${pkgs.xlibs.xset}/bin/xset r rate 200 50
    '';

    desktopManager = {
      default = "none";
      xterm.enable = false; 
    };

    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        dmenu
        i3status-rust
      ];
    };
  };

  systemd.user.services."compton" = {
    enable = true;
    description = "";
    wantedBy = [ "default.target" ];
    path = [ pkgs.compton ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.compton}/bin/compton -b --config /home/phil/.config/compton.conf";
  };

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      intel-media-driver # only available starting nixos-19.03 or the current nixos-unstable
    ];
  };

  services.xserver.libinput.enable = true;

  users.users.phil = {
    isNormalUser = true;
    extraGroups = [ "wheel" "audio" "video" "disk" ]; # Enable ‘sudo’ for the user.
  };

  system.stateVersion = "19.09"; # Did you read the comment?
}

