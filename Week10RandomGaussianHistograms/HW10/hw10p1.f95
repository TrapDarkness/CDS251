program hw10
    ! Alexander Shumway
    ! CDS251
    ! HW10Program1
    ! 03/28/2024
    
    ! Generate Gaussian numbers 

    implicit none

    !declare variables here
    integer :: i, n, seed
    real*8 :: BoxMuller, StDev, Mean
    
    !initialize files
    open(43,file='Bumps.txt')

   
    !Ask for input Variables
    print*, 'Seed'
    read(*,*) seed

    !Initialize other variables
    call srand(seed)

    !Run Normal Distribution loop, 13k numbers with 22 mean and 2.5 stdev
    StDev = 2.5d0
    Mean = 22.0d0
    n = 13000
    do i = 1, n
        write(43,*) BoxMuller(StDev, Mean)
    enddo
    
    !Run Normal Distribution loop, 7k numbers with 15.5 mean and 1.0 stdev
    StDev = 1.0d0
    Mean = 15.5d0
    n = 7000
    do i = 1, n
        write(43,*) BoxMuller(StDev, Mean)
    enddo
   
    !clean-up
    close(43)

end program hw10

function BoxMuller(StDev, Mean) result(rnum1)

    implicit none
    
    !Initialize working variables
    real*8 :: StDev, Mean, rnum1, rnum2, x1, x2, w

    do ! Infinite do loop until no rejection
    ! Generate 2 numbers between -1 and 1
        x1 = 2.d0 * rand() - 1.d0
        x2 = 2.d0 * rand() - 1.d0
        w = x1**2 + x2**2
        if (w .lt. 1.d0) exit ! Valid numbers
    enddo
    w = sqrt((-2.0 * log(w))/w)
    rNum1 = x1 * w
    rNum2 = x2 * w
    
    rNum1 = rNum1 * StDev + Mean

end function BoxMuller 
