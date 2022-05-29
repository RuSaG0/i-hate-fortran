module Group_IO
   use Environment

   implicit none
   
   integer, parameter   :: CANDIDATES_AMOUNT = 5
   integer, parameter   :: SURNAME_LENGTH = 15
   integer, parameter   :: INDEX_LENGTH = 2
   

   type CandidatesLut
      character(SURNAME_LENGTH, kind = CH_)        :: surname = ""
      character(INDEX_LENGTH, kind = CH_)          :: index = ""
      type(CandidatesLut), pointer                 :: next => Null()
   end type CandidatesLut

contains

   function readData(inputFile) result(Class_List)
      type(CandidatesLut), pointer    :: Class_List
      character(*), intent(in)        :: inputFile
      integer  In

      open (file = inputFile, encoding = E_, newunit = In)
         Class_List => readCandidate(In)
      close (In)
   end function readData


   recursive function readCandidate(In) result(candidates)
      type(CandidatesLut), pointer  :: candidates
      integer, intent(in)           :: In
      integer  IO
      character(:), allocatable  :: format
      
      allocate (candidates)
      format = '(2(a, 1x))'
      read (In, format, iostat = IO) candidates%index, candidates%surname
      call handleIO(IO, "reading line from file")
      if (IO == 0) then
          candidates%next => readCandidate(In)
      else
         deallocate (candidates)
         nullify (candidates)
      end if
   end function readCandidate


   subroutine OutputLut(outputFile, Class_List, List_Name, Position)
      character(*), intent(in)         :: outputFile, Position, List_Name
      type(CandidatesLut), intent(in)  :: Class_List
      integer  :: Out
      
      open (file = outputFile, encoding = E_, position = Position, newunit = Out)
         write (out, '(/a)') List_Name
         call OutputCandidate(Out, Class_List)
      close (Out)
   end subroutine OutputLut

   recursive subroutine OutputCandidate(Out, candidates)
      integer, intent(in)                 :: Out
      type(CandidatesLut), intent(in)     :: candidates
      
      integer                             :: IO
      character(:), allocatable           :: format

      format = '(2(a, 1x))'
      write (Out, format, iostat = IO) candidates%index, candidates%surname
      call handleIO(IO, "writing student")
      if (Associated(candidates%next)) &
         call OutputCandidate(Out, candidates%next)
   end subroutine OutputCandidate
end module Group_IO 
