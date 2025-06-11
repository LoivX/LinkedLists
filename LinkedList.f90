!! Linked list class. Implementation derived from Brainerd et al.

module linkedlistclass

  implicit none

  integer, private, parameter :: dp = selected_real_kind(15)
  
  type, private :: node
     real(8), allocatable, dimension(:)  :: value
     type(node), pointer :: next => null()
     type(node), pointer :: prev => null()
  end type node

  type, public :: LinkedList
     private
     type(node), pointer :: first
     type(node), pointer :: last
     integer             :: size = 0
  end type LinkedList

  private

  public :: &
       new,&
       inserthead,&
       inserttail,&
       pophead,&
       poptail,&
       size,&
       elements,&
       empty,&
       clear,&
       delete

  interface new
     module procedure newlist
  end interface

  interface inserthead
     module procedure inserthead
  end interface
  interface inserttail
     module procedure inserttail
  end interface

  interface pophead
     module procedure pophead
  end interface
  interface poptail
     module procedure poptail
  end interface

  interface size
     module procedure sizelist
  end interface
  
  interface empty
     module procedure emptylist
  end interface empty
  
  interface elements
     module procedure elementslist
  end interface elements

  interface delete
     module procedure deletelist
  end interface

  interface clear
     module procedure deletelist
  end interface

contains

  !!Constructor
  subroutine newlist(this, val)
    type(LinkedList),intent(out) :: this
    real(8), dimension(:), intent(in), optional :: val
    allocate(this % first)
    this % last => this % first
    nullify(this % first % next)
    nullify(this % first % prev)
    if (present(val)) then
      allocate(this % first % value(size(val)))
      this % first % value = val
      this % size = 1
    end if
  end subroutine newlist

  !!Constructor with initial array of values
  !subroutine newlistwitharray(this, vals)
  !  type(LinkedList),intent(out) :: this
  !  real(8), dimension(:), allocatable, intent(in) :: vals(:)
  !  integer :: i
  !  call newlist(this)
  !  do i = 1, size(vals)
  !    call inserttail(this, vals(i))
  !  end do
  !end subroutine newlistwitharray

  !!Insert an element into list
  subroutine inserthead(this,val)
    type(LinkedList),intent(inout)              :: this
    real(8), dimension(:), intent(in)           :: val    
    type(node),            pointer              :: new_node
    if (.not.associated(this % first)) then
      call newlist(this, val)
    else
      allocate(new_node)
      nullify(new_node % next)
      nullify(new_node % prev)
      allocate(new_node%value(size(val)))
      new_node%value = val 
      if(this % size == 0) then
        this % last => new_node
      else
        new_node % next => this % first
      end if     
      this % first % prev => new_node
      this % first => new_node
      this % size = this % size + 1
    end if
  end subroutine inserthead

  !!Insert an element at the end of the list
  subroutine inserttail(this, val)
   type(LinkedList),intent(inout)              :: this
    real(8), dimension(:), intent(in)           :: val    
    type(node),            pointer              :: new_node
    if(.not.associated(this%last)) then
      call newlist(this, val)
    else
      allocate(new_node)
      nullify(new_node % next)
      nullify(new_node % prev)
      allocate(new_node%value(size(val)))
      new_node % value = val
      if(this % size == 0) then
        this % first => new_node
      else
        new_node % prev => this % last
      end if
      this % last % next => new_node
      this % last => new_node
      this % size = this % size + 1
    end if
  end subroutine inserttail

  !!Remove an element from the head of the list
  subroutine pophead(this, val)
    type(LinkedList),intent(inout)              :: this
    real(8), dimension(:), intent(out)           :: val   
    if (this%size == 0) then
      !print*, 'ERROR: Impossible to remove an element: empty list'
      Return
    end if
    val = this % first % value
    if(.not.associated(this%first%next)) then
      nullify(this % first)
    else 
      this % first => this % first % next
      this % first % prev => null()
    end if
    this % size = this % size - 1
  end subroutine pophead

  subroutine poptail(this, val)
    type(LinkedList),intent(inout)              :: this
    real(8), dimension(:), intent(inout)           :: val   
    if (this%size == 0) then
      !print*, 'ERROR: Impossible to remove an element: empty list'
      Return
    end if
    val = this % last % value
    if(.not.associated(this%last%prev)) then
      nullify(this%last)
    else
      this % last => this % last % prev
      this % last % next => null()
    end if
        this % size = this % size - 1
  end subroutine poptail

  !!Number of entries
  integer function sizelist(this)
    type(LinkedList),intent(in) :: this
    sizelist = this % size
  end function sizelist
  
  !!Ask if linked list is empty
  logical function emptylist(this)
    type(LinkedList),intent(in) :: this
    emptylist = .not.associated(this % first)
  end function emptylist

  !!Destructor
  subroutine deletelist(this)
    type(LinkedList),intent(inout) :: this
    type(node),            pointer       :: temp_ptr,trail_ptr
     trail_ptr => this % first
     do while (associated(trail_ptr))
       temp_ptr => trail_ptr % next
       deallocate(trail_ptr)
       trail_ptr => temp_ptr
     end do
     nullify(this%first)
     nullify(this%last)
    !Set total items to 0
    this % size = 0
  end subroutine deletelist

  !!Return an allocated integer pointer containing the elements
  !!in the linked list.
  subroutine elementslist(this, vals)
    type(LinkedList), intent(inout) :: this
    real(8), dimension(:,:), intent(inout)   :: vals
    type(node),             pointer       :: temp_ptr
    integer                               :: i
    temp_ptr => this % first
    i=1
    do while (associated(temp_ptr))
      if(allocated(temp_ptr % value)) then
        vals(i,:) = temp_ptr % value(:)
        temp_ptr => temp_ptr % next
        i = i + 1
      else
        return
      end if
    end do
  end subroutine elementslist

end module linkedlistclass

