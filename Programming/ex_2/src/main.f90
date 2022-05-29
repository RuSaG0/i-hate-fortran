program exercise_2
   use Environment
   use IEEE_Arithmetic
   
   implicit none
   character(*), parameter:: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: x = 0, y = 0, z = 0, fval = 0

   open (file = input_file, newunit = In)
      read (In, *) x, y, z
   close (In)
   
   open (file = output_file, encoding = E_, newunit = Out)
      write (Out, "(4(a, f0.2/))") "x = ", x, "y = ", y, "z = ", z
   close (Out)
   
   fval = F(x, y, z)

   open (file = output_file, encoding = E_, newunit = Out, position='append')
      write (Out, "('f = ', f0.2)") fval
   close (Out)

contains
   pure function F(x, y, z)
      real(R_) F, x, y, z
      intent(in)  x, y, z
 
      if (x == .and. x /= z ) then
         F = z
      else if (x /= y .and. x == z ) then
         F = y
      else if (x /= y .and. y == z) then
         F = x
      end if
   end function F

end program exercise_2
