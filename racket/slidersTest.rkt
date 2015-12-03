#lang racket
(require
  "sliders.rkt")

(tabbed-sliders
 "Foo4"
 (lambda args (displayln args))
 '(("Tab1"
    ("Bar1" 1 10))
   ("Tab2"
    ("Bar2" 1 10)
    ("Baz2" 1 100))))