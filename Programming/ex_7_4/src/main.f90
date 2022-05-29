program exercise_7_30
   use Environment

   implicit none
   character(*), parameter:: input_file = "../data/input.txt", output_file = "output.txt"
   integer                          :: In = 0, Out = 0, N = 0, M = 0, i = 0
   integer(I_), allocatable         :: Indexes(:)
   real(R_), allocatable            :: A(:, :)
    
   open (file = input_file, newunit = In)
      read (In, *) N, M
      allocate (A(M, N))  ! Change by default
      allocate(Indexes(N))
      read (In, *) (A(:, i), i = 1, N)
   close (In)
   
   open (file = output_file, encoding = E_, newunit = Out)
      write (Out, '('//M//'f6.2)') (A(:, i), i = 1, N)
   close (Out)
   
   Indexes = [(i, i = 1, Ubound(A, 2))]
   call sortMatrixByRows(A, Indexes)
   

   open (file = output_file, encoding = E_, newunit = Out, position='append')
      write (Out, '(/'//N//'('//M//'f6.2/))') (A(1:M, i), i = 1, N)
      write(Out, *) Indexes
   close (Out)

    contains
    subroutine sortMatrixByRows(A, Indexes)
        real(R_), intent(inout)     :: A(:, :)
        integer(I_), intent(inout)  :: Indexes(:)
        integer(I_)                 :: MaxId, i
        
        do i = 1, N          
            MaxId = MaxLoc(A(1, i:), 1) + i-1
            if(MaxId /= i) then
                Indexes([i, MaxId]) = Indexes([MaxId, i])
                A(:, [i, MaxId]) = A(:, [MaxId, i])   
            end if
        end do
        
    end subroutine sortMatrixByRows

end program exercise_7_30
