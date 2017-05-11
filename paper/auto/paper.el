(TeX-add-style-hook
 "paper"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("extarticle" "9pt" "letterpaper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "paper=letterpaper" "margin=1in") ("xcolor" "table" "x11names" "dvipsnames") ("algpseudocode" "noend")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "extarticle"
    "extarticle10"
    "extsizes"
    "geometry"
    "xcolor"
    "xspace"
    "amsmath"
    "amsfonts"
    "amssymb"
    "hyperref"
    "tikz"
    "multirow"
    "graphicx"
    "parskip"
    "algorithmicx"
    "algorithm"
    "algpseudocode"
    "multicol")
   (TeX-add-symbols
    '("todo" 1)))
 :latex)

