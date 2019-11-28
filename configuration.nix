{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;
  hardware.enableRedistributableFirmware = true;

  boot = {
    kernelPackages = pkgs.linuxPackages_latest; 
    kernelModules = [ "iwlwifi" ];

    initrd.luks.devices = [{
      name = "root";
      device = "/dev/sda2";
      preLVM = true;
    }];

    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  networking = { 
    hostName = "nixos";
    wireless.iwd.enable = true;

    firewall.enable = true;
    useDHCP = false;
    interfaces.enp0s25.useDHCP = true;
    interfaces.wlan0.useDHCP = true;

    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Belgrade";

  environment.systemPackages = with pkgs; [
    # dev, tools
    cargo
    clang
    git
    gcc
    gdb
    gnumake
    fzf
    llvm
    lldb
    openjdk11
    leiningen
    rustc
    ripgrep
    rr
    valgrind
    

    # system
    acpi
    acpid
    compton
    powertop

    # apps
    feh    
    emacs
    firefox
    tdesktop
    rxvt_unicode-with-plugins
    vim
    which
    wget
  ];

  fonts.fonts = with pkgs; [
    terminus_font
    liberation_ttf
    dejavu_fonts
    noto-fonts
  ];

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # services.openssh.enable = true;
  # services.upower.enable = true;
  services.printing.enable = true;
  powerManagement.enable = true;
  powerManagement.powertop.enable = true;
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.xserver = {
    enable = true;
    autorun = false;
    layout = "us";
    xkbOptions = "ctrl:nocaps";
    libinput.enable = true;

    displayManager = {
      sessionCommands = ''
        ${pkgs.xlibs.xset}/bin/xset r rate 200 50
      '';
    };

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

  location.provider = "geoclue2";
  services.redshift = {
    enable = true;
  };

  systemd.user.services."compton" = {
    enable = true;
    description = "";
    wantedBy = [ "default.target" ];
    path = [pkgs.compton];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.compton}/bin/compton -b --config /home/phil/.config/compton.conf";
  };

  users.users.phil = {
    isNormalUser = true;
    extraGroups = [ "wheel" "audio" "video" "disk" ]; # Enable ‘sudo’ for the user.
  };

  system.stateVersion = "19.09";
}

