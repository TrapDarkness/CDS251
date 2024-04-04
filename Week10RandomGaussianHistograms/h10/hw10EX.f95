program hw10EX
    ! Alexander Shumway
    ! CDS251
    ! HW10pEX
    ! 04/03/2024
    
    ! Brownian Walk Simulation 

    implicit none

    !declare variables here
    integer :: i, n, seed, x, y
    real*4 :: rnum
    
    !initialize files
    open(43,file='Brownian.txt')

    !Ask for input Variables
    print*, 'Seed'
    read(*,*) seed

    !Initialize other variables
    call srand(seed)
    x = 0
    y = 0
    n = 20000

    !Run Brownian Loop
    do i = 1, n
        rnum = rand()
        if (rnum < 0.25) then 
            x = x + 1
        elseif (rnum < 0.5) then 
            y = y + 1
        elseif (rnum < 0.75) then 
            x = x - 1
        else
            y = y - 1
        endif
        write(43,*) x, y
    enddo
   
    !clean-up
    close(43)

end program hw10EX
