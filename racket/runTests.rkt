#lang racket

(let ([file-names (list "padraoDuarte-rhino.rkt" "patternDuarteGH_oldRosetta.rkt" "padraoDuarte.rkt")])
  (for ((file-name file-names))
    (system  (format "~a ~a" "\"C:\\Program Files\\Racket6.3\\Racket.exe\""
                   file-name))))


