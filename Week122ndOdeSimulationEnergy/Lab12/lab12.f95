program lab12
    ! Alexander Shumway
    ! CDS251
    ! Lab12
    ! 04/11/2024
    
    ! Write program  that simulates a cannon ball being fired.

    implicit none

    !declare variables here
    integer :: Count
    real*4 ::  h, x, y, theta, v, vx, vy
    real*4, parameter :: g = 9.8, pi = 3.14
    
   
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
    y = 0.0
    x = 0.0
    vx = v * cos((pi*theta)/180)
    vy = v * sin((pi*theta)/180)
    Count = 0

    !Setup Problemn
    write(42, *) x, y

    !Run Trajectory loop
    do 
        y = y + h * vy
        vy = vy - h * g
        x = x + h * vx
        write(42, *) x, y
        Count = Count + 1
        if (y .lt. 0) exit
    enddo

    print*, "Distance Travelled: ", x, " Time Aloft: ", Count * h
    
    !clean-up
    close(42)

end program lab12