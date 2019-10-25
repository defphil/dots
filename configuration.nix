# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;

  # Use the systemd-boot EFI boot loader.
  boot = {
  cleanTmpDir = true; 
  loader.systemd-boot.enable = true;
  loader.efi.canTouchEfiVariables = true;
  initrd.luks.devices = [ {
        name = "root";
        device = "/dev/sda2";
        preLVM = true;
      }
  ];

  };
    networking.hostName = "nix"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;
  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s25.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Belgrade";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget 
    gcc
    gdb
    fzf
    htop
    screenfetch
    redshift
    rustc
    cargo
    p7zip
    unrar
    unzip
    valgrind
    vlc
    xdg-user-dirs
    zip
    emacs
    tmux
    firefox
    git
    ripgrep
    networkmanagerapplet
    nix-prefetch-scripts
    tdesktop
    gnumake
    gnupg
    vim
    which    
    zsh
    extra-cmake-modules 
    weechat
    cmake
    dbus
    sway
    swaylock
    swayidle
    wl-clipboard
    light
    bemenu
    i3status-rust
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # services.openssh.enable = true;

  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Enable CUPS to print documents.
  services = {
    locate.enable = true;
    printing.enable = true;
  };
  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
# 
   services.xserver = {
     enable = true;
     autorun = false;
     layout = "us";
     xkbOptions = "ctrl:nocaps";
     libinput.enable = true;
 		# displayManager.lightdm.enable = true;
 		# windowManager.i3.enable = true;	
     # displayManager.extraSessionFilePackages = [ sway ];
   };
	
# 	services.compton = {
# 		enable = true;
# 	backend = "glx";
# 		shadow = false;
#     fade = false;
# 		vSync = true;
# 		
# 	};
programs.sway.enable = true;

  users = {     defaultUserShell = pkgs.zsh;  };
  users.users.phil = {
    isNormalUser = true;
    group = "users";
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "video" "audio" "networkmanager" "disk" ]; # Enable ‘sudo’ for the user.
  };

  system.stateVersion = "19.09"; # Did you read the comment?

}

