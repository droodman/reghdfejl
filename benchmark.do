clear all
scalar N = 100000000
scalar K = 100
set obs `=N'
gen id1 = runiformint(1, N/K)
gen id2 = runiformint(1,   K)

drawnorm x1 x2
gen double y = 3 * x1 + 2 * x2 + sin(id1) + cos(id2) + runiform()

set rmsg on

set processors 1
qui areg      y x1 x2, a(id1) cluster(id1)
qui reghdfe   y x1 x2, a(id1) cluster(id1)
qui reghdfejl y x1 x2, a(id1) cluster(id1)
qui reghdfejl y x1 x2, a(id1) cluster(id1) gpu
qui reghdfe   y x1 x2, a(id1 id2) cluster(id1 id2)
qui reghdfejl y x1 x2, a(id1 id2) cluster(id1 id2)
qui reghdfejl y x1 x2, a(id1 id2) cluster(id1 id2) gpu

set processors 6
qui areg      y x1 x2, a(id1) cluster(id1)
qui reghdfe   y x1 x2, a(id1) cluster(id1)
qui reghdfejl y x1 x2, a(id1) cluster(id1)
qui reghdfejl y x1 x2, a(id1) cluster(id1) gpu
qui reghdfe   y x1 x2, a(id1 id2) cluster(id1 id2)
qui reghdfejl y x1 x2, a(id1 id2) cluster(id1 id2)
qui reghdfejl y x1 x2, a(id1 id2) cluster(id1 id2) gpu
