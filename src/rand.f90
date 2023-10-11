
module int_to_unit_real64
    use, intrinsic :: iso_fortran_env, only: INT64, real64, int32

    implicit none

    contains
    function int64_to_unit_real64(value_int64) result(res)
        integer(int64), intent(in) :: value_int64
        real(real64) :: res

        integer(4),parameter :: mantissa_size = 52
        integer(int64),parameter :: bias=shiftl(1023_int64,mantissa_size)
        integer(4),parameter :: bits=64

        res = transfer(source= ior(bias, shiftr(value_int64, bits-mantissa_size)), mold= res ) - 1.0_REAL64
    end function int64_to_unit_real64
    function int32_to_unit_real64(value_int32) result(res)
        integer(int32), intent(in) :: value_int32
        real(real64) :: res

        integer(4),parameter :: mantissa_size = 23
        integer(int32),parameter :: bias=shiftl(127_int64,mantissa_size)
        integer(4),parameter :: bits=32
        real(4) :: temp

        temp = transfer(source= ior(bias, shiftr(value_int32, bits-mantissa_size)), mold= temp ) - 1._4
        res=real(temp,real64)
    end function int32_to_unit_real64

end module int_to_unit_real64

module random
    use, intrinsic :: iso_fortran_env, only: INT64, real64,&
                                                stdin=>input_unit,&
                                                stdout=>output_unit,&
                                                stderr=>error_unit   
    use int_to_unit_real64
    implicit none

    type,abstract :: myrand
        integer(INT64),allocatable :: state(:)
        contains
        procedure :: random_seed
        procedure(state_size_base),nopass,deferred :: state_size
        procedure :: random_number
        procedure(random_int64_base), deferred :: random_int64
        procedure :: get_seed_from
        procedure :: set_state
        ! procedure(random_update_state),deferred :: update_state
        ! procedure(random_number),deferred :: random
    end type
    interface
        function random_int64_base(self) result(res)
            import :: myrand,int64
            class(myrand) :: self
            integer(int64) :: res
        end function random_int64_base
        function state_size_base()
            integer(4) :: state_size_base        
        end function state_size_base
        function random_number_base(self) result(res)
            import :: myrand
            class(myrand) :: self
            real(8) :: res
        end function random_number_base
    end interface 
    contains
    subroutine set_state(self,put)
        class(myrand) :: self
        integer(int64),intent(in):: put(:)
        self%state=put
    end subroutine set_state
    subroutine random_seed(self,size,put,get)
        class(myrand) :: self
        integer(4),intent(out),optional :: size
        integer(int64),intent(in),optional :: put(:)
        integer(int64),intent(out),optional :: get(:)

        logical :: is_size, is_put, is_get
        integer(4) :: err

        is_size=present(size)
        is_get=present(get)
        is_put=present(put)
        if((is_size .and. (is_get .xor. is_put)) .or. (is_get .and. is_put)) then   !!! wheter more than one argument is present
            print*, 'error, only one between size, put, get must be selected'
            return
        else if (is_size) then      !!! if only size is requested
            size=self%state_size()
            return
        else if (is_get) then       !!! if is asking for the current state
            get=self%state
        else if (is_put) then       !!! if wants to change the state
            call self%set_state(put)
        else                        !!! initialize the rng
            allocate(self%state(self%state_size()), stat=err)
            if (err /= 0) then
                print *, "xoshirostar2%state: Allocation request denied"
                return
            endif
        endif
        end subroutine random_seed


        function random_number(self) result(res)
            class(myrand) :: self
            real(real64) :: res
            res=int64_to_unit_real64(self%random_int64())
        end function

        subroutine get_seed_from(self,other,seed)
            class(myrand) :: self, other
            integer(int64), optional, intent(in) :: seed(:)
            integer(4) :: i

            if (.not. allocated(self%state)) call self%random_seed()
            if (.not. allocated(other%state)) call other%random_seed()
            if (present(seed)) then
                if (size(seed) .ne. other%state_size()) then
                    write(stderr,'(A,I8,A,I8)') 'the seed provided has size ', size(seed),&
                        ' while the required size is ', other%state_size()
                    stop
                endif
                call other%random_seed(put=seed)
            else
                write(stderr,*) 'seed not provided, seed already present is used'
            endif
            do i=1,self%state_size()
                self%state(i) = other%random_int64()
            enddo
        end subroutine get_seed_from
end module random
