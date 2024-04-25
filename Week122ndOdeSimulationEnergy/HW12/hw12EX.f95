program HW12
    ! Alexander Shumway
    ! CDS251
    ! HW12EX
    ! 04/18/2024
    
    ! Write program for Runge Kutta Method for planet simulation

    implicit none

    !declare variables here
    integer :: i, j, n
    real*8:: tFinal, h, t, PE, KE, planetState(4), k1(4), k2(4), k3(4), k4(4)
    character*50 :: filename
   
    !Ask for input Variables
    print*, 'Initial x'
    read(*,*) planetState(1)
    print*, 'Initial x velocity'
    read(*,*) planetState(2)
    print*, 'Initial y'
    read(*,*) planetState(3)
    print*, 'Initial y velocity'
    read(*,*) planetState(4)
    print*, 'Step Size'
    read(*,*) h
    print*, 'Final Time'
    read(*,*) tFinal
    print*, 'Filename'
    read(*,*) filename    

    !Initialize Files
    open(42, file=filename)
    write(42,*) '# x, y, PE, KE, Total Energy, time'

    !Initialize other variables
    t = 0.d0

    !Setup Euler's Method and output file/screen
    write(42, *) planetState(1), planetState(3), PE(planetState), KE(planetState), PE(planetState) + KE(planetState), t
    print*, KE(planetState), PE(planetState), PE(planetState) + KE(planetState)
    print*, " "
    n = Int(tFinal / h)

    !Run Runge Kutta Method loop
    do i = 1, n
        call fPrime(planetState, k1)
        call fPrime(planetState+h/2.0*k1, k2)
        call fPrime(planetState+h/2.0*k2, k3)
        call fPrime(planetState+h*k3, k4)

        do j = 1, 4
            planetState(j) = planetState(j) + (h/6) * (k1(j) + 2.d0*k2(j) + 2.d0*k3(j) + k4(j))
        enddo
        t = t + h
        write(42, *) planetState(1), planetState(3), PE(planetState), KE(planetState), PE(planetState) + KE(planetState), t
    enddo
    
    !Write Final State
    print*, KE(planetState), PE(planetState), PE(planetState) + KE(planetState)


    !clean-up
    close(42)

end program HW12

 function KE(planetState) result(kineticEnergy)

    implicit none
 
    real*8 :: planetState(4), kineticEnergy
 
    kineticEnergy = 0.5d0 * (planetState(2)**2 + planetState(4)**2)
       
 end function KE 

 function PE(planetState) result(potentialEnergy)

    implicit none
 
    real*8 :: planetState(4), potentialEnergy
 
    potentialEnergy = -1.d0 / (((planetState(1)**2.d0 + planetState(3)**2.d0))**0.5d0)
       
 end function PE 

 subroutine fPrime(Planet, fP)
    implicit none
    real*8 :: Planet(4), fP(4), r, G, M
    r = sqrt(Planet(1)**2.d0 + Planet(3)**2.d0)
    G = 1.d0
    M = 1.d0
    fP(1) = Planet(2)
    fP(2) = -G * M * Planet(1) / r**3.d0
    fP(3) = Planet(4)
    fP(4) = -G * M * Planet(3) / r**3.d0
    end subroutine fPrime

