program proj_test

  use unit_test
  use proj

  implicit none

  type(test_suite_type) test_suite

  type(proj_type) p
  character(256) src_crs
  character(256) dst_crs
  real(8) lon, lat, x, y

  call test_suite_init('Test Proj', test_suite)

  call test_case_create('Init', test_suite)

  src_crs = '+proj=latlon +ellps=sphere'
  dst_crs = '+proj=lcc +lat_0=36.14387 +lon_0=105.5837 +lat_1=30 +lat_2=60 +ellps=sphere'
  call p%init(src_crs, dst_crs)
  lon = 105.5837
  lat = 36.14387
  call p%transform(lon, lat, x, y)
  call assert_equal(x, 0.0d0, __FILE__, __LINE__)
  call assert_equal(y, 0.0d0, __FILE__, __LINE__)

  call test_suite_report(test_suite)

  call test_suite_final(test_suite)

end program proj_test
