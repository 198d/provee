#lang racket
(require "../provee/main.rkt"
         "../provee/file.rkt"
         "../provee/ensure.rkt"
         "../provee/ensure/result.rkt")


(define provee-log-level (make-parameter 'info))
(define dnsmasq-interface (make-parameter "eth0"))
(define dnsmasq-dhcp-range (make-parameter "192.168.178.50,192.168.178.200"))
(define dnsmasq-lease-time (make-parameter "12h"))
(define dnsmasq-domain (make-parameter "home.example.com"))


(command-line
  #:once-each
  [("-i" "--interface") interface 
   "Sets the interface for Dnsmasq to bind to"
   (dnsmasq-interface interface)]
  [("-r" "--dhcp-range") range
   "Sets the DHCP address pool to use"
   (dnsmasq-dhcp-range range)]
  [("-L" "--lease-time") time
   "Sets the lease time for addresses"
   (dnsmasq-lease-time time)]
  [("-d" "--domain") domain
   "Sets the domain to reply with and serve DNS for"
   (dnsmasq-domain domain)]
  [("-l" "--log-level") log-level
   "Sets the provee logging level"
   (provee-log-level (string->symbol log-level))])


(provee:script #:log-level (provee-log-level)
               #:runtime-paths ([templates "templates"])
  (let ([config-path "/etc/dnsmasq.d/home.conf"])
    (ensure:system-package/installed 'dnsmasq)
    (ensure:service/started 'dnsmasq)

    (define config-result
      (ensure:together
        (ensure:file/contents
          config-path
          #:contents (render-template ([interface (dnsmasq-interface)]
                                       [domain (dnsmasq-domain)]
                                       [dhcp-range (dnsmasq-dhcp-range)]
                                       [lease-time (dnsmasq-lease-time)])
                       (build-path templates "dnsmasq.d/home.conf")))
        (ensure:file/attributes config-path #:owner 'root #:group 'root)))

    (when (result-changed? config-result)
      (ensure:service/restarted 'dnsmasq))))
