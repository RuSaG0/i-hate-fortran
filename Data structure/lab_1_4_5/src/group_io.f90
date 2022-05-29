module Group_IO
   use Environment

   implicit none
   integer, parameter :: CANDIDATES_AMOUNT = 5
   integer, parameter :: SURNAME_LENGTH = 15
   integer, parameter :: DATE_LENGTH = 4
   integer, parameter :: MILITARY_LENGTH = 3


   type CandidateType
      character(SURNAME_LENGTH, kind=CH_)          :: surname = ""
      character(DATE_LENGTH, kind=CH_)             :: birthDate = ""
      character(MILITARY_LENGTH, kind=CH_)         :: isServedCharset = ""
      character(kind=CH_)                          :: registrationCharset = ""
      character(kind=CH_)                          :: genderChar = ""
      type(CandidateType), pointer                 :: next => Null()
   end type CandidateType

contains

   function readClassList(Input_File) result(Class_List)
      type(CandidateType), pointer     :: Class_List
      character(*), intent(in)         :: Input_File
      integer  In

      open (file=Input_File, encoding=E_, newunit=In)
         Class_List => readCandidate(In)
      close (In)
   end function readClassList


   recursive function readCandidate(In) result(candidate)
      type(CandidateType), pointer     :: candidate
      integer, intent(in)              :: In
      integer  IO
      character(:), allocatable        :: format
      
      allocate (candidate)
      format = '('//CANDIDATES_AMOUNT//'(a, 1x))'
      read (In, format, iostat=IO) & 
            candidate%surname, candidate%birthDate, candidate%isServedCharset, & 
            candidate%registrationCharset, candidate%genderChar

      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
          candidate%next => readCandidate(In)
      else
         deallocate (candidate)
         nullify (candidate)
      end if
   end function readCandidate


   subroutine outputData(Output_File, Class_List, opDescription, writeType)
      character(*), intent(in)         :: Output_File, writeType, opDescription
      type(CandidateType), intent(in)  :: Class_List
      integer  :: Out
      
      open (file=Output_File, encoding=E_, position=writeType, newunit=Out)
         write (out, '(/a)') opDescription
         call outputCandidate(Out, Class_List)
      close (Out)
   end subroutine outputData

   recursive subroutine outputCandidate(Out, candidate)
      integer, intent(in)                 :: Out
      type(CandidateType), intent(in)     :: candidate
      
      integer  :: IO
      character(:), allocatable           :: format

      format = '('//CANDIDATES_AMOUNT//'(a, 1x))'
      write (Out, format, iostat=IO) &
             candidate%surname, candidate%birthDate, candidate%isServedCharset, & 
             candidate%registrationCharset, candidate%genderChar

      call Handle_IO_status(IO, "writing student")
      if (Associated(candidate%next)) &
         call outputCandidate(Out, candidate%next)
   end subroutine outputCandidate
end module Group_IO 
