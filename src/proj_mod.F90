module proj_mod

  use, intrinsic :: iso_c_binding

  implicit none

  private

  public proj_type
  public latlon_crs
  public lcc_crs

  integer(c_int), public, parameter :: PJ_FWD   =  1
  integer(c_int), public, parameter :: PJ_IDENT =  0
  integer(c_int), public, parameter :: PJ_INV   = -1

  type proj_type
    type(c_ptr) ctx
    type(c_ptr) pj
    character(:), allocatable :: src_crs
    character(:), allocatable :: dst_crs
  contains
    procedure :: init => proj_init
    procedure, private :: proj_transform_r4, proj_transform_r8
    generic :: transform => proj_transform_r4, proj_transform_r8
    procedure, private :: proj_inverse_transform_r4, proj_inverse_transform_r8
    generic :: inverse_transform => proj_inverse_transform_r4, proj_inverse_transform_r8
    final :: proj_final
  end type proj_type

  type, bind(c) :: PJ_COORD
    real(c_double) v(4)
  end type PJ_COORD

  interface lcc_crs
    module procedure lcc_crs_r4
    module procedure lcc_crs_r8
  end interface lcc_crs

  interface
    type(c_ptr) function proj_context_create() bind(c)
      use, intrinsic :: iso_c_binding
    end function proj_context_create

    subroutine proj_context_destroy(ctx) bind(c)
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: ctx
    end subroutine proj_context_destroy

    type(c_ptr) function proj_create_crs_to_crs(ctx, src_crs, dst_crs, area) bind(c)
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: ctx
      character(c_char) src_crs(*)
      character(c_char) dst_crs(*)
      type(c_ptr), value :: area
    end function proj_create_crs_to_crs

    subroutine proj_destroy(pj) bind(c)
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: pj
    end subroutine proj_destroy

    type(PJ_COORD) function proj_coord(x, y, z, t) bind(c)
      use, intrinsic :: iso_c_binding
      import PJ_COORD
      real(c_double), value :: x
      real(c_double), value :: y
      real(c_double), value :: z
      real(c_double), value :: t
    end function proj_coord

    type(PJ_COORD) function proj_trans(pj, direction, coord) bind(c)
      use, intrinsic :: iso_c_binding
      import PJ_COORD
      type(c_ptr), value :: pj
      integer(c_int), value :: direction
      type(PJ_COORD), value :: coord
    end function proj_trans

    integer(c_int) function proj_errno(pj) bind(c)
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: pj
    end function proj_errno

    type(c_ptr) function proj_errno_string(ierr) bind(c)
      use, intrinsic :: iso_c_binding
      integer(c_int), value :: ierr
    end function proj_errno_string

    integer(c_int) function strlen(str) bind(c)
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: str
    end function strlen
  end interface

contains

  subroutine proj_init(this, src_crs, dst_crs)

    class(proj_type), intent(out) :: this
    character(*), intent(in) :: src_crs
    character(*), intent(in) :: dst_crs

    this%ctx = proj_context_create()
    this%src_crs = trim(src_crs) // c_null_char
    this%dst_crs = trim(dst_crs) // c_null_char

    this%pj = proj_create_crs_to_crs(this%ctx, this%src_crs, this%dst_crs, c_null_ptr)
    if (proj_errno(this%pj) /= 0) then
      write(*, *) '[Error]: Failed to create pj object!'
      write(*, *) trim(this%src_crs)
      write(*, *) trim(this%dst_crs)
      write(*, *) get_proj_error_message(proj_errno(this%pj))
      stop 1
    end if

  end subroutine proj_init

  function latlon_crs(ellps) result(res)

    character(*), intent(in), optional :: ellps
    character(256) res

    character(256) ellps_

    if (present(ellps)) then
      ellps_ = ellps
    else
      ellps_ = 'sphere'
    end if

    res = '+proj=latlon +ellps=' // trim(ellps_)

  end function latlon_crs

  function lcc_crs_r4(lat_0, lon_0, lat_1, lat_2, x_0, y_0, ellps) result(res)

    real(4), intent(in) :: lat_0
    real(4), intent(in) :: lon_0
    real(4), intent(in) :: lat_1
    real(4), intent(in) :: lat_2
    real(4), intent(in), optional :: x_0
    real(4), intent(in), optional :: y_0
    character(*), intent(in), optional :: ellps
    character(256) res

    character(10) lat_0_s, lon_0_s, lat_1_s, lat_2_s
    character(20) x_0_s, y_0_s
    character(256) ellps_

    if (present(x_0)) then
      write(x_0_s, '(F20.5)') x_0
    else
      x_0_s = '0.0'
    end if
    if (present(y_0)) then
      write(y_0_s, '(F20.5)') y_0
    else
      y_0_s = '0.0'
    end if
    if (present(ellps)) then
      ellps_ = ellps
    else
      ellps_ = 'sphere'
    end if

    write(lat_0_s, '(F10.5)') lat_0
    write(lon_0_s, '(F10.5)') lon_0
    write(lat_1_s, '(F10.5)') lat_1
    write(lat_2_s, '(F10.5)') lat_2

    write(res, '("+proj=lcc +lat_0=", A, " +lon_0=", A, " +lat_1=", A, " +lat_2=", A, " +x_0=", A, " +y_0=", A, " +ellps=", A)') &
      trim(adjustl(lat_0_s)), &
      trim(adjustl(lon_0_s)), &
      trim(adjustl(lat_1_s)), &
      trim(adjustl(lat_2_s)), &
      trim(adjustl(x_0_s)), &
      trim(adjustl(y_0_s)), &
      trim(ellps_)

  end function lcc_crs_r4

  function lcc_crs_r8(lat_0, lon_0, lat_1, lat_2, x_0, y_0, ellps) result(res)

    real(8), intent(in) :: lat_0
    real(8), intent(in) :: lon_0
    real(8), intent(in) :: lat_1
    real(8), intent(in) :: lat_2
    real(8), intent(in), optional :: x_0
    real(8), intent(in), optional :: y_0
    character(*), intent(in), optional :: ellps
    character(256) res

    character(10) lat_0_s, lon_0_s, lat_1_s, lat_2_s
    character(20) x_0_s, y_0_s
    character(256) ellps_

    if (present(x_0)) then
      write(x_0_s, '(F20.5)') x_0
    else
      x_0_s = '0.0'
    end if
    if (present(y_0)) then
      write(y_0_s, '(F20.5)') y_0
    else
      y_0_s = '0.0'
    end if
    if (present(ellps)) then
      ellps_ = ellps
    else
      ellps_ = 'sphere'
    end if

    write(lat_0_s, '(F10.5)') lat_0
    write(lon_0_s, '(F10.5)') lon_0
    write(lat_1_s, '(F10.5)') lat_1
    write(lat_2_s, '(F10.5)') lat_2

    write(res, '("+proj=lcc +lat_0=", A, " +lon_0=", A, " +lat_1=", A, " +lat_2=", A, " +x_0=", A, " +y_0=", A, " +ellps=", A)') &
      trim(adjustl(lat_0_s)), &
      trim(adjustl(lon_0_s)), &
      trim(adjustl(lat_1_s)), &
      trim(adjustl(lat_2_s)), &
      trim(adjustl(x_0_s)), &
      trim(adjustl(y_0_s)), &
      trim(ellps_)

  end function lcc_crs_r8

  subroutine proj_transform_r4(this, xi, yi, xo, yo)

    class(proj_type), intent(inout) :: this
    real(4), intent(in) :: xi
    real(4), intent(in) :: yi
    real(4), intent(out) :: xo
    real(4), intent(out) :: yo

    real(8) x8, y8

    call this%transform(dble(xi), dble(yi), x8, y8)
    xo = x8
    yo = y8

  end subroutine proj_transform_r4

  subroutine proj_transform_r8(this, xi, yi, xo, yo)

    class(proj_type), intent(inout) :: this
    real(8), intent(in) :: xi
    real(8), intent(in) :: yi
    real(8), intent(out) :: xo
    real(8), intent(out) :: yo

    type(PJ_COORD) pj_xi, pj_xo

    if (.not. c_associated(this%pj)) then
      write(*, *) '[Error]: Projection object is not initialized!'
      stop 1
    end if

    pj_xi = proj_coord(xi, yi, 0.0d0, 0.0d0)
    pj_xo = proj_trans(this%pj, PJ_FWD, pj_xi)
    xo = pj_xo%v(1)
    yo = pj_xo%v(2)

  end subroutine proj_transform_r8

  subroutine proj_inverse_transform_r4(this, xi, yi, xo, yo)

    class(proj_type), intent(inout) :: this
    real(4), intent(in) :: xi
    real(4), intent(in) :: yi
    real(4), intent(out) :: xo
    real(4), intent(out) :: yo

    real(8) x8, y8

    call this%inverse_transform(dble(xi), dble(yi), x8, y8)
    xo = x8
    yo = y8

  end subroutine proj_inverse_transform_r4

  subroutine proj_inverse_transform_r8(this, xi, yi, xo, yo)

    class(proj_type), intent(inout) :: this
    real(8), intent(in) :: xi
    real(8), intent(in) :: yi
    real(8), intent(out) :: xo
    real(8), intent(out) :: yo

    type(PJ_COORD) pj_xi, pj_xo

    if (.not. c_associated(this%pj)) then
      write(*, *) '[Error]: Projection object is not initialized!'
      stop 1
    end if

    pj_xi = proj_coord(xi, yi, 0.0d0, 0.0d0)
    pj_xo = proj_trans(this%pj, PJ_INV, pj_xi)
    xo = pj_xo%v(1)
    yo = pj_xo%v(2)

  end subroutine proj_inverse_transform_r8

  subroutine proj_final(this)

    type(proj_type), intent(inout) :: this

    if (c_associated(this%ctx)) call proj_context_destroy(this%ctx)
    if (c_associated(this%pj)) call proj_destroy(this%pj)

  end subroutine proj_final

  function get_proj_error_message(ierr) result(res)

    integer, intent(in) :: ierr
    character, pointer :: res(:)

    type(c_ptr) err

    err = proj_errno_string(ierr)
    call c_f_pointer(err, res, [strlen(err)])

  end function get_proj_error_message

end module proj_mod
