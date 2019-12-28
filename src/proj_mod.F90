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
    procedure :: set_src_crs => proj_set_src_crs
    procedure :: set_dst_crs => proj_set_dst_crs
    procedure :: transform => proj_transform
    final :: proj_final
  end type proj_type

  type, bind(c) :: PJ_COORD
    real(c_double) v(4)
  end type PJ_COORD

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
      import
      type(c_ptr), value :: str
    end function strlen
  end interface

contains

  subroutine proj_init(this, src_crs, dst_crs)

    class(proj_type), intent(out) :: this
    character(*), intent(in), optional :: src_crs
    character(*), intent(in), optional :: dst_crs

    this%ctx = proj_context_create()
    if (present(src_crs)) this%src_crs = src_crs
    if (present(dst_crs)) this%dst_crs = dst_crs

  end subroutine proj_init

  subroutine proj_set_src_crs(this, crs)

    class(proj_type), intent(inout) :: this
    character(*), intent(in) :: crs

    this%src_crs = crs

  end subroutine proj_set_src_crs

  subroutine proj_set_dst_crs(this, crs)

    class(proj_type), intent(inout) :: this
    character(*), intent(in) :: crs

    this%dst_crs = crs

  end subroutine proj_set_dst_crs

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

  function lcc_crs(lat_0, lon_0, lat_1, lat_2, ellps) result(res)

    real(8), intent(in) :: lat_0
    real(8), intent(in) :: lon_0
    real(8), intent(in) :: lat_1
    real(8), intent(in) :: lat_2
    character(*), intent(in), optional :: ellps
    character(256) res

    character(10) lat_0_s, lon_0_s, lat_1_s, lat_2_s
    character(256) ellps_

    if (present(ellps)) then
      ellps_ = ellps
    else
      ellps_ = 'sphere'
    end if

    write(lat_0_s, '(F10.5)') lat_0
    write(lon_0_s, '(F10.5)') lon_0
    write(lat_1_s, '(F10.5)') lat_1
    write(lat_2_s, '(F10.5)') lat_2

    write(res, '("+proj=lcc +lat_0=", A, " +lon=", A, " +lat_1=", A, " +lat_2=", A, " +ellps=", A)') &
      trim(adjustl(lat_0_s)), &
      trim(adjustl(lon_0_s)), &
      trim(adjustl(lat_1_s)), &
      trim(adjustl(lat_2_s)), &
      trim(ellps_)

  end function lcc_crs

  subroutine proj_transform(this, xi, yi, xo, yo)

    class(proj_type), intent(inout) :: this
    real(8), intent(in) :: xi
    real(8), intent(in) :: yi
    real(8), intent(out) :: xo
    real(8), intent(out) :: yo

    type(PJ_COORD) pj_xi, pj_xo

    if (.not. c_associated(this%pj)) then
      this%pj = proj_create_crs_to_crs(this%ctx, this%src_crs, this%dst_crs, c_null_ptr)
      if (proj_errno(this%pj) /= 0) then
        write(*, *) '[Error]: Failed to create pj object!'
        stop 1
      end if
    end if

    pj_xi = proj_coord(xi, yi, 0.0d0, 0.0d0)
    pj_xo = proj_trans(this%pj, PJ_FWD, pj_xi)
    xo = pj_xo%v(1)
    yo = pj_xo%v(2)

  end subroutine proj_transform

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
