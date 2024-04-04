program Lab1
    ! Alexander Shumway
    ! CDS 251
    ! Assignment 1
    ! 01/25/2024
    ! This program prints out even numbers 2 to 42, counts down from user input, and makes a guessing game
    implicit none
    
    integer n, countdown, guess
    !Print even numbers 2-42
    do n = 2, 42 ,2    
        print*, n
    end do
    !Blast-Off
    print*, "Please enter a positive integer number"
    read*, countdown

    do n = countdown, 1, -1    
        print*, n
    end do
    print*, "blast-off"
    !Guess 42
    do while(guess /= 42)
    print*, "Guess a number from 1 to 100!"
    read*, guess
    end do
    print*, "You Win!"

end program Lab1