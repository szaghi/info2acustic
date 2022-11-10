!< Definition of info2acustic file object.
module info2acustic_file_object
!< Definition of info2acustic file object.
use, intrinsic :: iso_fortran_env, only : stdout => output_unit, &
                                          stderr => error_unit,  &
                                          I4P    => int32,       &
                                          R8P    => real64
use info2acustic_block_object, only : block_object
implicit none

private
public :: file_object

type :: file_object
   !< Definition of info2acustic file object.
   integer(I4P)                    :: nb=0        !< Blocks number.
   type(block_object), allocatable :: blocks(:)   !< Blocks.
   character(:),       allocatable :: file_name   !< File name.
   integer(I4P)                    :: file_unit=0 !< File unit.
   contains
      ! public methods
      procedure, pass(self) :: initialize     !< Initialize block.
      procedure, pass(self) :: load_from_file !< Load from ICEM info file.
      procedure, pass(self) :: write_to_file  !< Write to acustic file.
endtype file_object

contains
   subroutine initialize(self, file_name)
   !< Initialize block.
   class(file_object), intent(inout) :: self      !< File.
   character(*),       intent(in)    :: file_name !< File name.
   character(999)                    :: line      !< File line parser string.

   if (allocated(self%blocks)) deallocate(self%blocks)
   self%nb = 0
   self%file_name = trim(adjustl(file_name))
   open(newunit=self%file_unit, file=self%file_name, action='READ')
   do
      read(self%file_unit,'(A)', end=1, err=1) line
      if (index(line,"domain")>0) self%nb = self%nb + 1
   enddo
   1 close(self%file_unit)
   if (self%nb > 0) allocate(self%blocks(self%nb))
   call self%load_from_file
   endsubroutine initialize

   subroutine load_from_file(self)
   !< Load from ICEM info file.
   class(file_object), intent(inout) :: self !< File.
   character(999)                    :: line !< File line parser string.
   integer(I4P)                      :: b    !< Counter.

   if (self%nb > 0) then
      open(newunit=self%file_unit, file=self%file_name, action='READ')
      do b=1, self%nb
         read(self%file_unit,'(A)', end=1, err=1) line
         if (index(line,"domain")>0) then
            call self%blocks(b)%initialize(line=line)
            call self%blocks(b)%load_from_file(file_info=self%file_unit)
         endif
      enddo
      1 close(self%file_unit)
   endif
   endsubroutine load_from_file

   subroutine write_to_file(self, file_name)
   !< Write to acustic file.
   class(file_object), intent(inout) :: self      !< File.
   character(*),       intent(in)    :: file_name !< Acustic file name.
   integer(I4P)                      :: b         !< Counter.

   if (self%nb > 0) then
      open(newunit=self%file_unit, file=trim(adjustl(file_name)), action='WRITE')
      do b=1, self%nb
         call self%blocks(b)%write_to_file(file_acustic=self%file_unit)
      enddo
      1 close(self%file_unit)
   endif
   endsubroutine write_to_file
endmodule info2acustic_file_object
