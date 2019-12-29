program proj_test

  use unit_test
  use proj

  implicit none

  type(proj_type) p
  real(8) lon, lat, x, y

  call test_suite_init('Test Proj')

  call test_case_create('Init')

  call p%init(latlon_crs(), lcc_crs(36.14387d0, 105.5837d0, 30.0d0, 60.0d0))
  lon = 105.5837d0
  lat = 36.14387d0
  call p%transform(lon, lat, x, y)
  call assert_approximate(x, 0.0d0, __FILE__, __LINE__)
  call assert_approximate(y, 0.0d0, __FILE__, __LINE__)

  call test_suite_report()

  call test_suite_final()

end program proj_test
