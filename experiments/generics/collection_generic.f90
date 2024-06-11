! collection_generic.f90 --
!     Define the generic collection class:
!     - it functions as a specification for actual classes
!
!     Note:
!     This source file is meant to be included
!

    type, abstract :: collection
        logical                                  :: initialised = .false.  ! State: not initialised, initialised (data), has filter, has next
        logical                                  :: has_filter  = .false.
        logical                                  :: next_item   = .false.
        type(data_type)                          :: item
        procedure(filter), pointer, nopass       :: acceptable  => null()
    contains
        procedure                                :: has_next   => has_next_generic
        procedure                                :: get        => get_generic
        procedure                                :: set_filter => set_filter_generic
        procedure(get_next_item), deferred, pass :: get_next
    end type collection

    abstract interface
        logical function filter( item )
            import                      :: data_type
            type(data_type), intent(in) :: item
        end function filter

        subroutine get_next_item( this, item, retrieved )
            import                           :: collection, data_type
            class(collection), intent(inout) :: this
            type(data_type), intent(inout)   :: item
            logical, intent(out)             :: retrieved
        end subroutine
    end interface
