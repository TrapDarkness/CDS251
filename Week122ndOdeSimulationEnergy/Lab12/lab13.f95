program lab13
    ! Alexander Shumway
    ! CDS251
    ! Lab13
    ! 04/18/2024
    
    ! Write program  that simulates a cannon ball being fired.

    implicit none

    !declare variables here
    integer :: Count
    real*4 ::  h, theta, v, Ball(4), fP(4)
    real*4, parameter :: pi = 3.14
    
   
    !Ask for input Variables
    print*, 'Step Size'
    read(*,*) h
    print*, 'Angle'
    read(*,*) theta
    print*, 'Total velocity'
    read(*,*) v

    !Initialize Files
    open(42, file="Cannon.txt")

    !Initialize other variables
    Ball(1) = 0.0
    Ball(3) = 0.0
    Ball(2) = v * cos((pi*theta)/180)
    Ball(4) = v * sin((pi*theta)/180)
    Count = 0

    !Setup Problemn
    write(42, *) Ball(1), Ball(3)

    !Run Trajectory loop
    do 
        call fPrime(Ball, fP)
        Ball = Ball + h * fP
        write(42, *) Ball(1), Ball(3)
        Count = Count + 1
        if (Ball(3) .lt. 0) exit
    enddo

    print*, "Distance Travelled: ", Ball(1), " Time Aloft: ", Count * h
    
    !clean-up
    close(42)

end program lab13

subroutine fPrime(Ball, fP)
    implicit none
    real*4 :: Ball(4), fP(4)
    fP(1) = Ball(2)
    fP(2) = 0.0
    fP(3) = Ball(4)
    fP(4) = -9.8
    end subroutine fPrime