# Add paths
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "/var/lib/flatpak/exports/bin" ]; then
    PATH="/var/lib/flatpak/exports/bin:$PATH"
fi

if [ -d "$HOME/.local/share/flatpak/exports/bin" ]; then
    PATH="$HOME/.local/share/flatpak/exports/bin:$PATH"
fi

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi


# For Guix on foreign distros
# Locales
export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale
# X.509 Certificates (Possibly more so check)
export SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs"
export SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt"
export GIT_SSL_CAINFO="$SSL_CERT_FILE"
export CURL_CA_BUNDLE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt"
