clear all
scalar N = 10000000
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
qui areg      y x1 x2, a(id1 id2) cluster(id1)
qui reghdfe   y x1 x2, a(id1 id2) cluster(id1)
qui reghdfejl y x1 x2, a(id1 id2) cluster(id1)
qui reghdfejl y x1 x2, a(id1 id2) cluster(id1) gpu

set processors 6
qui areg      y x1 x2, a(id1) cluster(id1)
qui reghdfe   y x1 x2, a(id1) cluster(id1)
qui reghdfejl y x1 x2, a(id1) cluster(id1)
qui reghdfejl y x1 x2, a(id1) cluster(id1) gpu
qui areg      y x1 x2, a(id1 id2) cluster(id1)
qui reghdfe   y x1 x2, a(id1 id2) cluster(id1)
qui reghdfejl y x1 x2, a(id1 id2) cluster(id1)
qui reghdfejl y x1 x2, a(id1 id2) cluster(id1) gpu

/*
. set processors 1
    The maximum number of processors or cores being used is changed from 6 to 1.  It can be set to any number between 1 and 20
r; t=0.00 9:10:44

. qui areg      y x1 x2, a(id1) cluster(id1)
r; t=34.53 9:11:19

. qui reghdfe   y x1 x2, a(id1) cluster(id1)
r; t=6.68 9:11:26

. qui reghdfejl y x1 x2, a(id1) cluster(id1)
r; t=2.42 9:11:28

. qui reghdfejl y x1 x2, a(id1) cluster(id1) gpu
r; t=2.19 9:11:30

. qui areg      y x1 x2, a(id1 id2) cluster(id1)
r; t=8.14 9:11:38

. qui reghdfe   y x1 x2, a(id1 id2) cluster(id1)
r; t=17.15 9:11:55

. qui reghdfejl y x1 x2, a(id1 id2) cluster(id1)
r; t=2.57 9:11:58

. qui reghdfejl y x1 x2, a(id1 id2) cluster(id1) gpu
r; t=1.84 9:12:00

. 
. set processors 6
    The maximum number of processors or cores being used is changed from 1 to 6.  It can be set to any number between 1 and 20
r; t=0.00 9:12:00

. qui areg      y x1 x2, a(id1) cluster(id1)
r; t=7.32 9:12:07

. qui reghdfe   y x1 x2, a(id1) cluster(id1)
r; t=3.93 9:12:11

. qui reghdfejl y x1 x2, a(id1) cluster(id1)
r; t=1.54 9:12:13

. qui reghdfejl y x1 x2, a(id1) cluster(id1) gpu
r; t=1.57 9:12:14

. qui areg      y x1 x2, a(id1 id2) cluster(id1)
r; t=4.97 9:12:19

. qui reghdfe   y x1 x2, a(id1 id2) cluster(id1)
r; t=10.35 9:12:30

. qui reghdfejl y x1 x2, a(id1 id2) cluster(id1)
r; t=2.20 9:12:32

. qui reghdfejl y x1 x2, a(id1 id2) cluster(id1) gpu
r; t=1.83 9:12:34
*/