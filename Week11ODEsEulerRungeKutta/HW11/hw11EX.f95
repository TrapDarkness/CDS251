program lab11
    ! Alexander Shumway
    ! CDS251
    ! HW10Program1
    ! 03/28/2024
    
    ! Write program for Runge kutta’s Method

    implicit none

    !declare variables here
    integer :: i, n
    real*4 :: tFinal, h, yInit, MyFuncODE, t, y, y2, Tol
    character*50 :: filename
    
   
    !Ask for input Variables
    ! print*, 'Final Time'
    ! read(*,*) tFinal
    ! print*, 'Step Size'
    ! read(*,*) h
    print*, 'Initial y'
    read(*,*) yInit
    print*, 'Filename'
    read(*,*) filename

    !Initialize Files
    open(42, file=filename)

    !Initialize other variables
    t = 0.0
    tFinal = 5
    h = 0.05
    y = yInit
    Tol = 5.0

    !Setup Runge Kutta Method
    write(42, *) t, y
    n = Int(tFinal / h)

    !Run Runge Kutta Method loop
    do i = 1, n
        y2 = y
        do
            y2 = y + h * MyFuncODE(t+h, y2)
            if (abs(y2 - y) .lt. Tol) exit !ASK IN CLASS WHAT - Y shouild be?? and Tol??
        enddo
        t = t + h
        y = y2
        write(42, *) t, y
    enddo
    
    !clean-up
    close(42)

end program lab11

function MyFuncODE(y, t) result(fyt)

    implicit none
 
    real*4 :: y, t, fyt
 
    fyt = y**2 - y**3/5 - t
       
 end function MyFuncODE