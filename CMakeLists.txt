cmake_minimum_required(VERSION 3.6)

project(fortran-proj LANGUAGES C Fortran)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -traceback")
endif ()
get_directory_property(parent_dir PARENT_DIRECTORY)

include(FindPkgConfig)
pkg_check_modules(PROJ REQUIRED proj)
include_directories(${PROJ_INCLUDE_DIRS})
link_directories(${PROJ_LIBRARY_DIRS})

if (NOT parent_dir)
  add_subdirectory(lib/unit-test)
endif ()

set(sources
  src/proj_mod.F90
  src/proj.F90
)

add_library(fortran_proj ${sources})
target_link_libraries(fortran_proj proj)

if (NOT parent_dir)
  add_executable(proj_test.exe src/proj_test.F90)
  target_link_libraries(proj_test.exe fortran_proj fortran_unit_test)
endif ()
