#lang racket

(cond
  [else 3])

(let ([else #f])
  (cond
    [else 2]))

(let ([cond 1]
      [else 2])
  (cond
    [else 2]))

(match #f
  [else (cond
          [else 3])])

(cond
  [1 2]
  [3 4])

(match 1
  [_else 1])

(case 1
  [else 1])
