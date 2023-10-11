module xoshiro256
    ! Modules to import
    use, intrinsic :: iso_fortran_env, only: INT64
    use random
    implicit none

    public :: xoshiro256star2
    public :: xoshiro256plus2
    public :: xoshiro256plus

    type, extends(myrand) :: xoshiro256star2
    contains
    procedure, nopass :: state_size => xoshiro256_state_size
    procedure :: random_int64 => xoshiro256star2_random_int64
    end type xoshiro256star2

    type, extends(myrand) :: xoshiro256plus2
    contains
    procedure, nopass :: state_size => xoshiro256_state_size
    procedure :: random_int64 => xoshiro256plus2_random_int64
    end type xoshiro256plus2
    
    type, extends(myrand) :: xoshiro256plus
    contains
    procedure, nopass :: state_size => xoshiro256_state_size
    procedure :: random_int64 => xoshiro256plus_random_int64
    end type xoshiro256plus

contains
    pure elemental function rotl(i, shift)
        integer(INT64), intent(in) :: i
        integer, intent(in) :: shift
        integer(INT64) :: rotl
        rotl = ior(shiftl(i, shift), shiftr(i, 64 - shift))
    end function rotl

    subroutine update_state(state)
        integer(int64),intent(inout) :: state(:)
        integer(INT64) :: t

        t = shiftl(state(2), 17)

        state(3) = ieor(state(3), state(1))
        state(4) = ieor(state(4), state(2))
        state(2) = ieor(state(2), state(3))
        state(1) = ieor(state(1), state(4))

        state(3) = ieor(state(3), t)
        state(4) = rotl(state(4), 45)
    end subroutine update_state

    function xoshiro256_state_size()
        integer(4) :: xoshiro256_state_size
        xoshiro256_state_size = 4
    end function xoshiro256_state_size

    function xoshiro256star2_random_int64(self) result(res)
            class(xoshiro256star2) :: self
            integer(8) :: res
            
            res = rotl(self%state(2) * 5_INT64, 7) * 9_INT64
            call update_state(self%state)
    end function xoshiro256star2_random_int64
    function xoshiro256plus2_random_int64(self) result(res)
            class(xoshiro256plus2) :: self
            integer(8) :: res

            res=rotl(self%state(1) + self%state(4), 23) + self%state(1)
            call update_state(self%state)
    end function xoshiro256plus2_random_int64
    function xoshiro256plus_random_int64(self) result(res)
            class(xoshiro256plus) :: self
            integer(8) :: res

            res=self%state(1) + self%state(4)
            call update_state(self%state)
    end function xoshiro256plus_random_int64
end module xoshiro256