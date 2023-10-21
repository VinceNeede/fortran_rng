program xoshiro256_example
    use random
    use xoshiro256
    use SplitMix64_rng
    implicit none
    class(myrand), pointer :: rng1, rng2
    integer(4) :: size
    integer(8), allocatable :: old_state(:), new_state(:)
    
    !!! allocate the random number generators using source to specify which generator to use
    allocate(rng1,source=xoshiro256star2())
    allocate(rng2,source=xoshiro256star2())
    
    call rng1%random_seed()                     !!! when called with no arguments the space for the state is allocated
    call rng1%random_seed(size=size)            !!! get the size of the state
    allocate(old_state(size),new_state(size))   !!! allocate space for the states
    call rng1%random_seed(get=old_state)        !!! get the current state of the generator
    new_state=1
    call rng1%random_seed(put=new_state)        !!! set the state

    !!! A best practice for the xoshiro256 is to set the state of a SplitMix64 generator
    !!! and use its output as generator
    call rng2%get_seed_from(SplitMix64(),[0_8])

    !!! get a random double precision real 
    print*, rng2%random_number()
end program xoshiro256_example