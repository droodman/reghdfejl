clear all
scalar N = 10000000
scalar K = 100
set obs `=N'
gen id1 = floor(runiform() * (N+1)/K)
gen id2 = floor(runiform() * (K+1))

drawnorm x1 x2
gen y = 3 * x1 + 2 * x2 + sin(id1) + cos(id2) + runiform()

set rmsg on

set processors 1
qui reghdfe   y x1 x2, a(id1) cluster(id1)
qui reghdfejl y x1 x2, a(id1) cluster(id1)
qui reghdfe   y x1 x2, a(id1 id2) cluster(id1 id2)
qui reghdfejl y x1 x2, a(id1 id2) cluster(id1 id2)

set processors 8
qui reghdfe   y x1 x2, a(id1) cluster(id1)
qui reghdfejl y x1 x2, a(id1) cluster(id1)
qui reghdfe   y x1 x2, a(id1 id2) cluster(id1 id2)
qui reghdfejl y x1 x2, a(id1 id2) cluster(id1 id2)
