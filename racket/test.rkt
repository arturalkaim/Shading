#lang racket
(require
  "fffidefiner.rkt")
(defffi "int init(int n)")
(defffi "int start(int n)")