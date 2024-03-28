program lab10
    ! Alexander Shumway
    ! CDS251
    ! Lab10
    ! 03/28/2024
    
    ! Comput pi using random numbers 

    implicit none

    !declare variables here
    integer :: i, n, seed, Count
    real*8 :: x, y, pi
   
    !Ask for input Variables
    print*, 'Number of iterations'
    read(*,*) n
    print*, 'Seed'
    read(*,*) seed

    !Initialize other variables
    Count = 0
    call srand(seed)

    !Run monte carlo sim loop
    do i = 1, n
        x = rand()
        y = rand()
        if (x**2+y**2 .le. 1.d0) Count = Count + 1
    enddo
        
    pi = 4.0d0 * float(Count) / float(n)
    print*, "Pi: ", pi
   
    !clean-up

end program lab10