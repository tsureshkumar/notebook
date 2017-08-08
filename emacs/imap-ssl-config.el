(if (or (eq system-type 'gnu/linux)
        (eq system-type 'linux))
    (setq imap-ssl-program "/usr/bin/openssl s_client -ssl3 -connect %s:%p"))
