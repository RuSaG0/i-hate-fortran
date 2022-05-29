program exercise_7_48
   use Environment
   
   implicit none
   character(*), parameter:: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0
   real(R_), allocatable   :: A(:, :), B(:)

   open (file = input_file, newunit = In)
      read (In, *) N
      allocate (A(N, N))
      read (In, *) (A(i, :), i = 1, N)
   close (In)
   
   open (file = output_file, encoding = E_, newunit = Out)
      write (Out, '('//N//'f6.2)') (A(i, :), i = 1, N)
   close (Out)
  
   B = GetPositiveNumbers(A)
   
   open (file = output_file, encoding = E_, newunit = Out, position='append')
      write (Out, *)
      write (Out, '('//Size(B)//'f6.2)') B 
   close (Out)

contains
   pure function GetPositiveNumbers(A) result(B)
      real(R_), intent(in)               :: A(:, :)
      logical                            :: Mask(Ubound(A, 1), Ubound(A, 2))
      real(R_), allocatable              :: B(:)

      Mask = A > 0   

      allocate(B(Count(Mask)))
      
      B = Pack(A, Mask)

   end function GetPositiveNumbers
end program exercise_7_48
