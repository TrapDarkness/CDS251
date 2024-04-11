program HW11EX
    ! Alexander Shumway
    ! CDS251
    ! HW11EX
    ! 04/10/2024
    
    ! Write program for Backward Euler’s Method

    implicit none

    !declare variables here
    integer :: i, n
    real*4 :: tFinal, h, yInit, MyFuncODE, t, y, y2, Tol, y2Old
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
    Tol = 1e-4

    !Setup Backward Euler's Method
    write(42, *) t, y
    n = Int(tFinal / h)

    !Run Backward Euler's Method loop
    do i = 1, n
        y2 = y
        do
            y2Old = y2
            y2 = y + h * MyFuncODE(t+h, y2)
            if (abs(y2 - y2Old) .lt. Tol) exit ! stop once there's no longer a change between the old and new y2 value
        enddo
        t = t + h
        y = y2
        write(42, *) t, y
    enddo
    
    !clean-up
    close(42)

end program HW11EX

function MyFuncODE(y, t) result(fyt)

    implicit none
 
    real*4 :: y, t, fyt
 
    fyt = y**2 - y**3/5 - t
       
 end function MyFuncODE