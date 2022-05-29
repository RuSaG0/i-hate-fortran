module Source_IO
   use Environment

   implicit none
   
   type Text
      character(:, CH_), allocatable   :: Str
      type(Text), pointer              :: Next  => Null()
   end type Text

contains

  function readString(InputFile) result (modifiedSubsctrings)
      type(Text), pointer        :: modifiedSubsctrings
      character(*), intent(in)   :: InputFile
      integer  :: In
      
      open (file = InputFile, encoding = E_, newunit = In)
         modifiedSubsctrings => Read_Source_Line(in)
      close (In)
   end function readString

   recursive function Read_Source_Line(in) result (modifiedSubsctrings)
      type(Text), pointer        :: modifiedSubsctrings
      integer, intent(in)        :: In
      integer, parameter         :: max_len = 150
      character(max_len, CH_)    :: str
      integer                    :: IO

      read (In, '(a)', iostat = io) str
      call Handle_IO_Status(IO, "reading line from source code")
      
      if (IO == 0) then
         allocate (modifiedSubsctrings)
         modifiedSubsctrings%Str = Trim(str)
         modifiedSubsctrings%Next => Read_Source_Line(In)
      else
         modifiedSubsctrings => Null()
      end if
   end function Read_Source_Line
   


   subroutine output(OutputFile, modifiedSubsctrings, writeType)
      character(*), intent(in)  :: OutputFile, writeType
      type(Text), intent(in)    :: modifiedSubsctrings
      integer  :: Out
     

      open (file = OutputFile, encoding = E_, position = writeType, newunit = Out)
        call Output_Source_Line(Out, modifiedSubsctrings)
        write (Out, *) "==================================================="
      close (Out)
   end subroutine output


   recursive subroutine Output_Source_Line(Out, modifiedSubsctrings)
      integer, intent(in)           :: Out
      type(Text), intent(in)        :: modifiedSubsctrings
      integer  :: IO

      write (Out, "(a)", iostat = IO) modifiedSubsctrings%Str
      call Handle_IO_Status(IO, "writing line to file")
      if (Associated(modifiedSubsctrings%next)) &
         call Output_Source_Line(Out, modifiedSubsctrings%next)
   end subroutine Output_Source_Line
end module Source_IO 
