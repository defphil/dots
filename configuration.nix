# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, callPackage, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/sdb2";
      preLVM = true;
    }
  ];

  nixpkgs.config.allowUnfree = true;
nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
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

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };
  time.timeZone = "Europe/Belgrade";

  fonts.enableCoreFonts = true;
  fonts.fonts = with pkgs; [
  	corefonts
  	terminus_font
  	dejavu_fonts
  ];
  environment.profileRelativeEnvVars.XCURSOR_PATH = [ "/share/icons" ];

  environment.systemPackages = with pkgs; [
    firefox
    git
    gnupg
    gtk_engines
    compton
    nix-prefetch-scripts
    which
    wget
    vim
    acpi
    light
    actkbd
    xdg_utils
    zsh
    rxvt_unicode
    tdesktop
  ];

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  services.printing.enable = true;
  services.acpid.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;
	
  services.compton.enable = true;
  services.compton.shadow = false;
  services.xserver = {


    enable = true;
    layout = "us";
    xkbOptions = "ctrl:nocaps";
    videoDrivers = [ "intel" ];
    deviceSection = ''                                                                            
    Option "DRI" "2"                                                                            
    Option "TearFree" "true"                                                                    
  '';

    desktopManager = {
		default = "none";
		xterm.enable = false;
	};
    windowManager.default = "none";
    windowManager.i3.enable = true;
    displayManager.sessionCommands = ''
		xrdb "${pkgs.writeText "xrdb.conf" ''
			XTerm*background: #080808
			URxvt*background: #080808
			URxvt*foreground: #ffffff
			XTerm*foreground: #ffffff
			XTerm*font: -xos4-terminus-medium-r-normal-*-14-*-*-*-*-*-*-*
			URxvt*font: -xos4-terminus-medium-r-normal-*-14-*-*-*-*-*-*-*
			URxvt.scrollBar: false
			URxvt.cursorColor: #ff1060
  			URxvt.colorUL:              #AED210
  			URxvt.perl-ext:             default,url-select
  			URxvt.keysym.M-u:           perl:url-select:select_next
  			URxvt.url-select.launcher:  /usr/bin/firefox -new-tab
  			URxvt.url-select.underline: true
			bindsym XF86MonBrightnessUp exec xbacklight -inc 20; # increase screen brightness
			bindsym XF86MonBrightnessDown exec xbacklight -dec 20; # decrease screen brightness
		''}"
	'';
  };

  services.xserver.libinput.enable = true;
  programs.light.enable = true;
  services.actkbd = {
    enable = true;
    bindings = [
      { keys = [ 224 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -A 10"; }
      { keys = [ 225 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -U 10"; }
    ];
  };

  programs.zsh.enable = true;
  users.users.phil = {
    isNormalUser = true;	
    extraGroups = [ "wheel" "video" "audio" "disk" "networkmanager" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };

  systemd.user.services."urxvtd" = {
    enable = true;
    description = "rxvt unicode daemon";
    wantedBy = [ "default.target" ];
    path = [ pkgs.rxvt_unicode ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.rxvt_unicode}/bin/urxvtd -q -o";
  };

  nix = {
    gc.automatic = true;
    useSandbox = true;
   # package = pkgs.nixUnstable;
  };

 system.stateVersion = "19.03"; # Did you read the comment?

}
