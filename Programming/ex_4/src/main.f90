program exercise_4
   use Environment
   
   implicit none
   character(*), parameter:: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0
   real(R_)                :: x1 = 0, x2 = 0, h = 0
   real(R_), allocatable   :: X(:), F(:) 
   
   open (file = input_file, newunit = In)
      read (In, *) x1, x2, h
   close (In)
   open (file = output_file, encoding = E_, newunit = Out)
      write (Out, '(3(a, T4, "= ", f0.4/))') "x1", x1, "x2", x2, "h", h
   close (Out)
   
   N = Int((x2-x1) / h + .5_R_) + 1
   allocate (X(N), F(N))
  
   call TabF(x1, h, X, F)
  
   open (file = output_file, encoding = E_, newunit = Out, position='append')
      write (Out, '("  X   |   f")')
      write (Out, '(f0.4, T7, "| ", f0.4)') (X(i), F(i), i = 1, N)
   close (Out)

contains
   ! Чистая функция в регулярном стиле.
   pure subroutine TabF(x1, h, X, F)
      real(R_)    x1, h, X(:), F(:)
      intent(in)  x1, h
      intent(out) X, F
      integer     i
      
      X = [(x1+h*(i-1), i = 1, Size(X))]
      F = sin(3*x) - 3*Sin(X)
   end subroutine TabF
end program exercise_4
