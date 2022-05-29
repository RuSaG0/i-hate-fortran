program reference_lab_3_4
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable                      :: inputFile, outputFile

   type(CandidatesLut), pointer                   :: candidatesList => Null() 
   character(SURNAME_LENGTH, kind = CH_)          :: lastSurname 

   inputFile  = "../data/class.txt"
   outputFile = "output.txt"
   
   candidatesList => readData(inputFile)

   if (Associated(candidatesList)) then
      call OutputLut(outputFile, candidatesList, "sourceList:", "rewind")
      
      lastSurname = getLastSurname(candidatesList)
      call deleteCandidates(candidatesList, lastSurname)
      
   if (Associated(candidatesList)) &
         call OutputLut(outputFile, candidatesList, "modifiedList:", "append")
   end if
end program reference_lab_3_4
