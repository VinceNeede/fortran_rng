module pcg32_rng
    use, intrinsic :: iso_fortran_env, only: INT64, int32,&
                                stderr=>error_unit,real64,real32
    use random
    use int_to_unit_real64
    implicit none
    
    type,extends(myrand) :: pcg32
        contains
        procedure, nopass :: state_size => pcg32_state_size
        procedure :: random_int64 => pcg32_random_int64
        procedure :: set_state => pcg32_set_state
        procedure :: random_number => pcg32_random_number
        procedure :: pcg32_random_r
    end type
    contains
    function pcg32_state_size()
        integer(4) :: pcg32_state_size
        pcg32_state_size = 2
    end function pcg32_state_size

    function pcg32_random_int64(self) result(res)
            class(pcg32) :: self
            integer(8) :: res
            
            write(stderr,*) 'Do not use this function for pcg32'
            stop
            self%state=0
            res=0           
    end function pcg32_random_int64

    function pcg32_random_r(self) result(res)
        class(pcg32),target :: self
        integer(int32) :: res

        integer(int64) :: oldstate
        integer(int64),pointer :: state,inc
        integer(int32) :: xorshifted, rot
        
        state=>self%state(1)
        inc=>self%state(2)

        oldstate = state
        state = oldstate*6364136223846793005_int64 + ior(inc,1)

        xorshifted = transfer(shiftr(ieor(shiftr(oldstate,18),oldstate),27),mold=xorshifted)
        rot = transfer(shiftr(oldstate,59),mold=rot)

        res=ior(shiftr(xorshifted,rot),shiftl(xorshifted,iand(-rot,31)))
    end function pcg32_random_r

    subroutine pcg32_set_state(self, put)
        class(pcg32) :: self
        integer(int64),intent(in) :: put(:)

        integer(int64) :: initstate, initseq
        integer(int32) :: temp

        
        initstate=put(1)
        initseq=put(2)

        self%state(1)=0
        self%state(2)=ior(shiftl(initseq,1),1)
        temp=self%pcg32_random_r()
        self%state(1)=self%state(1)+initstate
        temp=self%pcg32_random_r()
    end subroutine pcg32_set_state

    function pcg32_random_number(self) result(res)
        class(pcg32) :: self
        real(real64) :: res
        res=int32_to_unit_real64(self%pcg32_random_r())
    end function
end module pcg32_rng