program exercise_7_2
   use Environment
   
   implicit none
   character(*), parameter:: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0
   real(R_), allocatable   :: A(:, :), C(:)
   real(R_)                :: B

   open (file = input_file, newunit = In)
      read (In, *) N
      allocate (A(N, N), C(N))
      read (In, *) (A(i, :), i = 1, N)
   close (In)
   
   open (file = output_file, encoding = E_, newunit = Out)
      write (Out, '('//N//'f6.2)') (A(i, :), i = 1, N)
   close (Out)
  
    B = NORM2(A) 
   open (file = output_file, encoding = E_, newunit = Out, position='append')
      write (Out, *) B 
   close (Out)

end program exercise_7_2
