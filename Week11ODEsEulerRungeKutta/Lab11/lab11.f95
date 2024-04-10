program lab11
    ! Alexander Shumway
    ! CDS251
    ! HW10Program1
    ! 03/28/2024
    
    ! Write program for Forward Euler’s Method

    implicit none

    !declare variables here
    integer :: i, n
    real*4 :: tFinal, h, yInit, MyFuncODE, t, y
    
   
    !Ask for input Variables
    print*, 'Final Time'
    read(*,*) tFinal
    print*, 'Step Size'
    read(*,*) h
    print*, 'Initial y'
    read(*,*) yInit

    !Initialize Files
    open(42, file="Eulers253001.txt")

    !Initialize other variables
    t = 0.0
    y = yInit

    !Setup Euler's Method
    write(42, *) t, y
    n = Int(tFinal / h)

    !Run Euler's Method loop
    do i = 1, n
        y = y + h * MyFuncODE(y,t)
        t = t + h
        write(42, *) t, y
    enddo
    
    !clean-up
    close(42)

end program lab11

function MyFuncODE(y, t) result(fyt)

    implicit none
 
    real*4 :: y, t, fyt
 
    fyt = 1 - 2 * y**2 + 5*t - t**2
       
 end function MyFuncODE