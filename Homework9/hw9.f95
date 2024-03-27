program hw9
    ! Alexander Shumway
    ! CDS251
    ! Homework9
    ! 03/27/2024
    
    ! This program uses implements the Midpoint Rule 

    implicit none

    !declare variables here
    integer :: i, n
    real*4 :: a, b, h, area, simpson_area, MyFunc, MyFuncIntegral, midpoint_height, left_height, right_height, exact
   
    !Ask for input Variables
    print*, 'What is the 1st limit of integration?'
    read(*,*) a
    print*, 'What is the 2nd limit of integration?'
    read(*,*) b
    print*, 'What increment do you want?'
    read(*,*) h

    !Initialize other variables
    area = 0.0
    simpson_area = 0.0
    n = int((b - a) / h)
    exact = MyFuncIntegral(b) - MyFuncIntegral(a)

    do i = 1, n
        left_height = MyFunc(a + float(i - 1) * h)
        right_height = MyFunc(a + float(i) * h)
        midpoint_height = MyFunc(a + float(i - 1) * h + 0.5 * h)
        area = area + h * midpoint_height
        simpson_area = simpson_area + ((h / 6) * ((left_height) + 4*(midpoint_height) + (right_height)))
    enddo
        
    print*, "Midpoint Rule:"
    print*, "Exact: ", exact, " h: ", h, " Computed Area: ", area
    print*, " Absolute Error", abs(exact - area), " Relative Error", exact - area  !Print results
    print*, "Simpson's Rule:"
    print*, " Computed Area: ", simpson_area, " Absolute Error", abs(exact - simpson_area), " Relative Error", exact - simpson_area  !Print results

    !clean-up

end program hw9

function MyFunc(x) result(fx)

   implicit none

   real*4 :: x, fx

    fx = exp(-1*x) * (sin(x))**2
      
end function MyFunc

function MyFuncIntegral(x) result(intfx)

    implicit none
 
    real*4 :: x, intfx
 
    intfx = (exp(-1*x) / 10) *  (cos(2*x) - 2*sin(2*x) - 5)
       
 end function MyFuncIntegral
