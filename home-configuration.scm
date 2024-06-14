;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
	     (gnu home services)
             (gnu packages)
             (gnu packages gnupg)
             (gnu services)
             (guix gexp)
             (gnu home services shells)
             (gnu home services ssh)
             (gnu home services gnupg)
             (gnu home services dotfiles))

(home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.
 (packages (specifications->packages (list "cmake"
					   "flatpak"
					   "font-google-noto"
					   "font-google-noto-emoji"
					   "font-google-noto-sans-cjk"
					   "font-google-noto-serif-cjk"
					   "font-hack"
					   "gnome-tweaks"
					   "gnupg"
					   "htop"
					   "igt-gpu-tools"
					   "intel-media-driver-nonfree"
                                           "kitty"
					   "libva-utils"
					   "ncurses"
					   "pinentry"
                                           "pinentry-emacs"
                                           "pinentry-gnome3"
                                           "pinentry-tty"
                                           "pinentry-qt"
 					   "gcc-toolchain"
 					   "make"
                                           "bash-completion"
                                           "emacs-pgtk-xwidgets"
                                           "firefox"
                                           "git"
                                           "google-chrome-stable"
                                           "libvterm"
                                           "moonlight-qt"
                                           "mpv"
                                           "neovim"
                                           "perl"
                                           "python-wrapper"
                                           "ruby"
                                           "node"
                                           "tree"
                                           "ungoogled-chromium-wayland"
                                           "virt-manager"
                                           "yt-dlp"
                                           "zsh"
                                           "zsh-autopair"
                                           "zsh-autosuggestions"
                                           "zsh-completions"
                                           "zsh-history-substring-search"
                                           "zsh-syntax-highlighting")))

 ;; Below is the list of Home services.  To search for available
 ;; services, run 'guix home search KEYWORD' in a terminal.
 (services
  (list (service home-dotfiles-service-type
                 (home-dotfiles-configuration
                  (layout 'stow)
                  (directories '("./Dotfiles"))))

        (service home-openssh-service-type)

        (service home-gpg-agent-service-type
                 (home-gpg-agent-configuration
                  (default-cache-ttl 34560000)
                  (max-cache-ttl 34560000)
                  (default-cache-ttl-ssh 34560000)
                  (max-cache-ttl-ssh 34560000)
                  (pinentry-program
                   (file-append pinentry "/bin/pinentry-emacs"))))

        (service home-ssh-agent-service-type
              (home-ssh-agent-configuration
               (extra-options '("-t" "12h")))))))
