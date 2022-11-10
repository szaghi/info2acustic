!< Definition of (ICEM multi-block info) block object.
module info2acustic_block_object
!< Definition of (ICEM multi-block info) block object.
use, intrinsic :: iso_fortran_env, only : stdout => output_unit, &
                                          stderr => error_unit,  &
                                          I4P    => int32,       &
                                          R8P    => real64
implicit none

private
public :: block_object

type :: block_object
   !< Definition of (ICEM multi-block info) block object.
   integer(I4P)           :: ni=0         !< Number of cells in I direction.
   integer(I4P)           :: nj=0         !< Number of cells in J direction.
   integer(I4P)           :: nk=0         !< Number of cells in K direction.
   real(R8P), allocatable :: xyz(:,:,:,:) !< Nodes coordinates [3,ni+1,nj+1,nk+1].
   real(R8P), allocatable :: uvw(:,:,:,:) !< Velocity components [3,ni+1,nj+1,nk+1].
   real(R8P), allocatable ::   p(  :,:,:) !< Pressure [ni+1,nj+1,nk+1].
   contains
      ! public methods
      procedure, pass(self) :: initialize     !< Initialize block.
      procedure, pass(self) :: load_from_file !< Load from ICEM info file.
      procedure, pass(self) :: write_to_file  !< Write to acustic file.
endtype block_object

contains
   subroutine initialize(self, line)
   !< Initialize block.
   class(block_object), intent(inout) :: self      !< Block.
   character(*),        intent(in)    :: line      !< File line parsed string.

   read(line(index(line," "):),*)self%ni, self%nj, self%nk
   self%ni = self%ni - 1
   self%nj = self%nj - 1
   self%nk = self%nk - 1

   if (allocated(self%xyz)) deallocate(self%xyz)
   allocate(self%xyz(3,self%ni+1,self%nj+1,self%nk+1))
   if (allocated(self%uvw)) deallocate(self%uvw)
   allocate(self%uvw(3,self%ni+1,self%nj+1,self%nk+1))
   if (allocated(self%p)) deallocate(self%p)
   allocate(self%p(self%ni+1,self%nj+1,self%nk+1))
   endsubroutine initialize

   subroutine load_from_file(self, file_info)
   !< Load from ICEM info file.
   class(block_object), intent(inout) :: self      !< Block.
   integer(I4P),        intent(in)    :: file_info !< INFO file handler.
   integer(I4P)                       :: i, j, k   !< Counter.

   do i=1, self%ni+1
      do j=1, self%nj+1
         do k=1, self%nk+1
            read(file_info,*) self%xyz(1:3,i,j,k)
         enddo
      enddo
   enddo
   endsubroutine load_from_file

   subroutine write_to_file(self, file_acustic)
   !< Write to acustic file.
   class(block_object), intent(in) :: self         !< Block.
   integer(I4P),        intent(in) :: file_acustic !< Acustic file handler.
   integer(I4P)                    :: i, j, k      !< Counter.

   do i=1, self%ni+1
      do j=1, self%nj+1
         do k=1, self%nk+1
            write(file_acustic, '(3(E23.15,1X))') self%xyz(1:3,i,j,k)
         enddo
      enddo
   enddo
   endsubroutine write_to_file
endmodule info2acustic_block_object
