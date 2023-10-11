module SplitMix64_rng
    use, intrinsic :: iso_fortran_env, only: INT64
    use random
    implicit none
    public :: SplitMix64
    type, extends(myrand) :: SplitMix64
        contains
        procedure, nopass :: state_size => SplitMix64_state_size
        procedure :: random_int64 => SplitMix64_random_int64
    end type SplitMix64

    contains
    function SplitMix64_state_size()
        integer(4) :: SplitMix64_state_size
        SplitMix64_state_size = 1
    end function SplitMix64_state_size

    function SplitMix64_random_int64(self) result(res)
        class(SplitMix64) :: self
        integer(int64) :: res

        self%state(1)=self%state(1)+Z'9e3779b97f4a7c15'
        res=self%state(1)
        
        res = ieor(res, SHIFTR(res, 30)) * Z'bf58476d1ce4e5b9'
        res = ieor(res, SHIFTR(res, 27)) * Z'94d049bb133111eb'
        res = ieor(res, SHIFTR(res, 31))
    end function SplitMix64_random_int64
end module SplitMix64_rng
