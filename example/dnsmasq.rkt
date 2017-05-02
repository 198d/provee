#lang racket
(require "../provee/main.rkt"
         "../provee/file.rkt"
         "../provee/ensure.rkt"
         "../provee/ensure/result.rkt")


(provee:script dnsmasq
               ([interface "eth0" "Interface for Dnsmasq to bind to"]
                [dhcp-range "192.168.178.50,192.168.178.200"
                 "DHCP address pool to use"]
                [lease-time "12h" "Lease time for addresses"]
                [domain "home.example.com"
                 "Domain to reply with and serve DNS for"]
                [config-path "/etc/dnsmasq.d/home.conf"
                 "Path to write config file at"])
               #:runtime-paths ([templates "templates"])
  (let ([config-path "/etc/dnsmasq.d/home.conf"])
    (ensure:system-package/installed 'dnsmasq)
    (ensure:service/started 'dnsmasq)

    (define config-result
      (ensure:together
        (ensure:file/contents
          (dnsmasq-config-path)
          #:contents (render-template ([interface (dnsmasq-interface)]
                                       [domain (dnsmasq-domain)]
                                       [dhcp-range (dnsmasq-dhcp-range)]
                                       [lease-time (dnsmasq-lease-time)])
                       (build-path templates "dnsmasq.d/home.conf")))
        (ensure:file/attributes (dnsmasq-config-path)
                                #:owner 'root #:group 'root)))

    (when (result-changed? config-result)
      (ensure:service/restarted 'dnsmasq))))
