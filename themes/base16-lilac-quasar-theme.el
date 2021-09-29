(require 'base16-theme)

;;; Use my own color scheme
(defvar lilac-quasar-colors
  '(:base00 "#0e0a21"
     :base01 "#0e0a21"
     :base02 "#1d1448"
     :base03 "#858290"
     :base04 "#221e35"
     :base05 "#fcfbff"
     :base06 "#c2c1c7"
     :base07 "#fcfbff"
     :base08 "#8d33eb"
     :base09 "#8f32aa"
     :base0A "#c132eb"
     :base0B "#ed5be5"
     :base0C "#5733eb"
     :base0D "#e62cdc"
     :base0E "#ffa2ed"
     :base0F "#7a1eb6")
  "All colors for Base16 Lilac Quasar.")

;; Define the theme
(deftheme base16-lilac-quasar)

;; Add all the faces to the theme
(base16-theme-define 'base16-lilac-quasar lilac-quasar-colors)

;; Mark the theme as provided
(provide-theme 'base16-lilac-quasar)

(provide 'base16-lilac-quasar-theme)
