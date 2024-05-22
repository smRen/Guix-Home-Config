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
 (packages (specifications->packages (list "neovim"
					   "git"
                                           "glibc-locales"
					   "nss-certs")))

 ;; Below is the list of Home services.  To search for available
 ;; services, run 'guix home search KEYWORD' in a terminal.
 (services
  (list (service home-bash-service-type
                 (home-bash-configuration
                  (bashrc (list (local-file
                                 "/home/smakey18/Projects/Guix-Home-Config/bash/dot-bashrc.bash"
                                 "bashrc")))
                  (bash-profile (list (local-file
                                       "/home/smakey18/Projects/Guix-Home-Config/bash/dot-bash_profile.bash"
                                       "bash_profile")))))

        (simple-service 'env-config
			home-environment-variables-service-type
			`(("SSL_CERT_DIR" . "$HOME/.guix-home/profile/etc/ssl/certs")
			  ("SSL_CERT_FILE" . "$HOME/.guix-home/profile/etc/ssl/certs/ca-certificates.crt")
			  ("GIT_SSL_CAINFO" . "$HOME/.guix-home/profile/etc/ssl/certs/ca-certificates.crt")))

        (simple-service 'nvim-config
			home-xdg-configuration-files-service-type
			`(("nvim/coc-settings.json" ,(local-file "nvim/coc-settings.json"))
			  ("nvim/init.vim" ,(local-file "nvim/init.vim"))))
        
	(simple-service 'emacs-config
			home-xdg-configuration-files-service-type
			`(("emacs/init.el" ,(local-file "emacs/init.el"))
			  ("emacs/early-init.el" ,(local-file "emacs/early-init.el"))
                          ("emacs/straight/versions/default.el" ,(local-file "emacs/straight/versions/default.el"))))

        (simple-service 'scripts-config
			home-files-service-type
			`(("Scripts/git-prompt.sh" ,(local-file "scripts/git-prompt.sh"))))
        
        (simple-service 'gnugpg-config
			home-files-service-type
			`((".gnupg/gpg-agent.conf" ,(local-file "gpg/gpg-agent.conf"))
                          (".gnupg/gpg.conf" ,(local-file "gpg/gpg.conf"))))
        
	(simple-service 'git-config
			 home-xdg-configuration-files-service-type
			`(("git/config" ,(local-file "git/gitconfig"))))
	
	(simple-service 'mpv-config
			home-xdg-configuration-files-service-type
			`(("mpv/scripts/nextfile.lua" ,(local-file "mpv/scripts/nextfile.lua"))
			  ("mpv/input.conf" ,(local-file "mpv/input.conf"))
			  ("mpv/mpv.conf" ,(local-file "mpv/mpv.conf")))))))
