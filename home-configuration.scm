;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
	     (gnu home services)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services shells))

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
  (list (service home-bash-service-type
                 (home-bash-configuration
                  (bashrc (list (local-file
                                 "bash/dot-bashrc.bash")))
                  (bash-profile (list (local-file
                                       "bash/dot-bash_profile.bash")))))

        (service home-xdg-configuration-files-service-type
                 ;; Neovim config
		 `(("nvim/coc-settings.json" ,(local-file "nvim/coc-settings.json"))
		   ("nvim/init.vim" ,(local-file "nvim/init.vim"))
		   ;; Git config
                   ("git/config" ,(local-file "git/gitconfig"))
                   ;; Kitty config
                   ("kitty/kitty.conf" ,(local-file "kitty/kitty.conf"))
                   ;; Mpv config
                   ("mpv/scripts/nextfile.lua" ,(local-file "mpv/scripts/nextfile.lua"))
		   ("mpv/input.conf" ,(local-file "mpv/input.conf"))
		   ("mpv/mpv.conf" ,(local-file "mpv/mpv.conf"))))
        
	(service home-files-service-type
                 ;; Git prompt
		 `(("Scripts/git-prompt.sh" ,(local-file "scripts/git-prompt.sh"))
		   ;; Emacs config
                   (".emacs.d/init.el" ,(local-file "emacs/init.el"))
		   (".emacs.d/early-init.el" ,(local-file "emacs/early-init.el"))
                   (".emacs.d/straight/versions/default.el" ,(local-file "emacs/straight/versions/default.el"))
		   ;; GPG
                   (".gnupg/gpg-agent.conf" ,(local-file "gpg/gpg-agent.conf"))
                   (".gnupg/gpg.conf" ,(local-file "gpg/gpg.conf")))))))
