module Group_Process
   use Environment
   use Group_IO

   implicit none

contains

   pure recursive function getLastSurname (candidates) result (lastSurname)
      character(SURNAME_LENGTH, kind = CH_)  :: lastSurname
      type(CandidatesLut), intent(in)        :: candidates 
      
      if (Associated(candidates%next)) then
       lastSurname = getLastSurname(candidates%next)
      else
       lastSurname = candidates%surname
      end if
   end function getLastSurname
   
   
   recursive subroutine deleteCandidates(candidates, lastSurname)
      
      type(CandidatesLut), pointer                       :: candidates
      character(SURNAME_LENGTH, kind = CH_), intent(in)  :: lastSurname

      if (Associated(candidates)) then
         if (candidates%surname == lastSurname) then
            call deleteItem(candidates)
            call deleteCandidates (candidates, lastSurname) 
         else
            call deleteCandidates (candidates%next, lastSurname)
         end if
      end if

   end subroutine deleteCandidates
 
 pure  recursive subroutine deleteItem(candidate)
      type(CandidatesLut), pointer  :: candidate
      type(CandidatesLut), pointer  :: tmp
      
            tmp => candidate
            candidate => candidate%next
            deallocate(tmp)

   end subroutine deleteItem
    
end module Group_Process
   

