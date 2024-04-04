Program CDS251Homework2
    ! Alexander Shumway
    ! CDS251
    ! Assignment 2
    ! 02/01/2024
    
    !This program shows how errors in computer arithmetic can occur
    
    implicit none
    !declare variables here
    integer n !first do loop iterator variable
    real*4 x,y  !addition variables for 10000000 adds loop
    real*4 prior_sum, current_sum, k !loop variables for extra credit
    
    !initialize addition
    x = 1.0
    y = 2.0
    !initialize infinite loop variables
    prior_sum = 0.0
    k = 1.0
    !addition loop code, adding tiny numbers 10 million times to show
    !how the math messes up eventually
    do n=0, 10000000
        x = x + 1.e-7
        y = y + 1.e-7
    end do
    
    !Print Results
    print *, "x: ", x
    print *, "y: ", y
    
    !Extra credit problem
    !I guess that 1/n will underflow and become zero 
    !Sum loop before, iterates infinitely until the sum no longer change
    !Then exits the loop and displays Results
    do while(1 /= 0)
        current_sum = prior_sum + 1/k
        !here's the check for the sum no longer changing
        if (prior_sum == current_sum) exit
        k = k + 1
        prior_sum = current_sum
    end do
    
    !Results displayed here to deduce what exactly made the loop stop. 
    print *, "Sum: ", current_sum
    print *, "Number of loops executed: ", k
    print *, "Last 1/n: ", 1/k
    !Looks like I was wrong, it appears to be because of option 3, to little precision
    
    End Program CDS251Homework2
    
    