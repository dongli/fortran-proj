# Introduction

fortran-proj is another Fortran bindings for Proj C library. It should provide more concise APIs.

# Usage

```fortran
use proj

type(proj_type) p

call p%init(latlon_crs(), lcc_crs(36.14387d0, 105.5837d0, 30.0d0, 60.0d0))
lon = 105.5837
lat = 36.14387
call p%transform(lon, lat, x, y)
```

That's it!

# Contributors

- Li Dong
