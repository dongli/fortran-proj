module proj_mod

  use, intrinsic :: iso_c_binding

  implicit none

  private

  public proj_type

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

  type, bind(c) :: PJ_XYZT
    real(c_double) x
    real(c_double) y
    real(c_double) z
    real(c_double) t
  end type PJ_XYZT

  type, bind(c) :: PJ_UVWT
    real(c_double) u
    real(c_double) v
    real(c_double) w
    real(c_double) t
  end type PJ_UVWT

  type, bind(c) :: PJ_LPZT
    real(c_double) lam
    real(c_double) phi
    real(c_double) z
    real(c_double) t
  end type PJ_LPZT

  type, bind(c) :: PJ_XYZ
    real(c_double) x
    real(c_double) y
    real(c_double) z
  end type PJ_XYZ

  type, bind(c) :: PJ_UVW
    real(c_double) u
    real(c_double) v
    real(c_double) w
  end type PJ_UVW

  type, bind(c) :: PJ_LPZ
    real(c_double) lam
    real(c_double) phi
    real(c_double) z
  end type PJ_LPZ

  type, bind(c) :: PJ_XY
    real(c_double) x
    real(c_double) y
  end type PJ_XY

  type, bind(c) :: PJ_UV
    real(c_double) u
    real(c_double) v
  end type PJ_UV

  type, bind(c) :: PJ_LP
    real(c_double) lam
    real(c_double) phi
  end type PJ_LP

  type, bind(c) :: PJ_COORD
    real(c_double) v(4)
    type(PJ_XYZT) xyzt
    type(PJ_UVWT) uvwt
    type(PJ_LPZT) lpzt
    type(PJ_XYZ) xyz
    type(PJ_UVW) uvw
    type(PJ_LPZ) lpz
    type(PJ_XY) xy
    type(PJ_UV) uv
    type(PJ_LP) lp
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

    type(PJ_COORD) function proj_trans(pj, direction, coord) bind(c)
      use, intrinsic :: iso_c_binding
      import PJ_COORD
      type(c_ptr), value :: pj
      integer(c_int), value :: direction
      type(PJ_COORD), value :: coord
    end function proj_trans
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

  subroutine proj_transform(this, xi, yi, xo, yo)

    class(proj_type), intent(inout) :: this
    class(*), intent(in) :: xi
    class(*), intent(in) :: yi
    class(*), intent(out) :: xo
    class(*), intent(out) :: yo

    type(PJ_COORD) pj_xi, pj_xo

    if (.not. c_associated(this%pj)) then
      this%pj = proj_create_crs_to_crs(this%ctx, this%src_crs, this%dst_crs, c_null_ptr)
    end if

    select type (xi)
    type is (real(4))
      pj_xi%lpzt%lam = xi
    type is (real(8))
      pj_xi%lpzt%lam = xi
    end select
    select type (yi)
    type is (real(4))
      pj_xi%lpzt%phi = yi
    type is (real(8))
      pj_xi%lpzt%phi = yi
    end select
    print *, pj_xi
    pj_xo = proj_trans(this%pj, PJ_FWD, pj_xi)
    print *, pj_xo
    select type (xo)
    type is (real(4))
      xo = pj_xo%xyzt%x
    type is (real(8))
      xo = pj_xo%xyzt%x
    end select
    select type (yo)
    type is (real(4))
      yo = pj_xo%xyzt%y
    type is (real(8))
      yo = pj_xo%xyzt%y
    end select

  end subroutine proj_transform

  subroutine proj_final(this)

    type(proj_type), intent(inout) :: this

    if (c_associated(this%ctx)) call proj_context_destroy(this%ctx)
    if (c_associated(this%pj)) call proj_destroy(this%pj)

  end subroutine proj_final

end module proj_mod
