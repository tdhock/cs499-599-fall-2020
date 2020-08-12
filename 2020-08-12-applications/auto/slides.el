(TeX-add-style-hook
 "slides"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("xy" "all") ("inputenc" "utf8")))
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "tikz"
    "xy"
    "amsmath"
    "amssymb"
    "hyperref"
    "graphicx"
    "algorithmic"
    "multirow"
    "inputenc")
   (TeX-add-symbols
    '("algo" 1)
    "sign"
    "RR"
    "ZZ"
    "NN"))
 :latex)

