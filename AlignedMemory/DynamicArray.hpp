/*
ANN: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef ANN_DYNAMICARRAY_HPP
#define ANN_DYNAMICARRAY_HPP

#include <cstddef>
#include <iterator>
#include <memory>
#include <memory_resource>
#include <new>
#include <ranges>
#include <type_traits>
#include <utility>

#include "../TemplateManipulation.hpp"
#include "../Type.hpp"
#include "AlignedAllocator.hpp"

namespace ann {

template<typename ValueType, size_t align>
struct AlignedPtr;

// template<typename ValueType, size_t alignment = alignof(ValueType)>
// struct DynamicArray;

// template<typename ValueType, size_t alignment>
// void swap(DynamicArray<ValueType, alignment> arrA, DynamicArray<ValueType, alignment> arrB);

inline constexpr struct uninit_tag_t {} uninit_tag;

template<typename Allocator>
using alloc_ptr_t = typename std::allocator_traits<Allocator>::pointer;

template<typename Allocator>
using alloc_size_t = typename std::allocator_traits<Allocator>::size_type;

template<typename Allocator>
constexpr alloc_ptr_t<Allocator>
aligned_allocate(Allocator& alloc, alloc_size_t<Allocator> number_of_elements, std::align_val_t alignment) = delete;

template<typename ValueType>
constexpr alloc_ptr_t<std::pmr::polymorphic_allocator<ValueType>>
aligned_allocate(std::pmr::polymorphic_allocator<ValueType>& alloc,
                 alloc_size_t<std::pmr::polymorphic_allocator<ValueType>> number_of_elements,
                 std::align_val_t alignment){
    return static_cast<ValueType*>(alloc.allocate_bytes(number_of_elements * sizeof(ValueType), size_cast(alignment)));
}

template<typename Allocator>
constexpr void aligned_deallocate(
    Allocator& alloc, alloc_ptr_t<Allocator> returning_memory, alloc_size_t<Allocator> number_of_elements,
    std::align_val_t alignment) = delete;

template<typename ValueType>
constexpr void
aligned_deallocate(std::pmr::polymorphic_allocator<ValueType>& alloc,
                   alloc_ptr_t<std::pmr::polymorphic_allocator<ValueType>> returning_memory,
                   alloc_size_t<std::pmr::polymorphic_allocator<ValueType>> number_of_elements,
                   std::align_val_t alignment){
    alloc.deallocate_bytes(returning_memory, number_of_elements * sizeof(ValueType), size_cast(alignment));
}

namespace internal {
template<typename Type>
concept nonconst = !std::is_const_v<Type>;

template<typename Allocator, typename Type>
concept allocator_for = std::same_as<Type, typename std::allocator_traits<Allocator>::value_type>;

template<typename Allocator>
concept call_aligned_allocate = requires(Allocator&& alloc) {
    typename std::allocator_traits<Allocator>::pointer;
    { aligned_allocate(alloc, std::size_t{}, std::align_val_t{}) } -> std::same_as<typename std::allocator_traits<Allocator>::pointer>;
};
} // namespace internal



template<typename ObjToConstruct, typename Alloc, typename... Types>
concept constructible_by_alloc = std::is_constructible_v<ObjToConstruct, Types...> || std::is_constructible_v<ObjToConstruct, Types..., Alloc> || std::is_constructible_v<std::allocator_arg_t, Alloc, ObjToConstruct, Types...>;

template<typename ObjToConstruct, typename Alloc, typename... Types>
    requires constructible_by_alloc<ObjToConstruct, Alloc, Types...>
consteval bool detect_noexcept_construct() {
    using construct_tuple =
        decltype(std::uses_allocator_construction_args<ObjToConstruct>(std::declval<Alloc>(), std::declval<Types>()...));
    using tuple_types = extract_pack_t<construct_tuple>;
    using front_bound_trait = front_bind_t<std::is_nothrow_constructible, ObjToConstruct>;
    using applied_trait = typename decltype(apply_pack(front_bound_trait{}, tuple_types{}))::type;
    return applied_trait::value;
}

template<typename ObjToConstruct, typename Alloc, typename... Types>

consteval bool detect_noexcept_construct() {
    return false;
}

// TODO: consider lifting the guarantee that other owns nothing after move assign; that allows for branchless moves.
//       Add in a check for equal array size for copy assign of nothrow copy assignable types.

template<
    internal::nonconst ValueType, internal::allocator_for<ValueType> Allocator = aligned_allocator<ValueType, align_val_of<ValueType>>,
    std::align_val_t align = align_val_of<ValueType>>
    requires(allocator_alignment_v<Allocator> >= align || internal::call_aligned_allocate<Allocator>)

struct dynamic_array {
    using value_type = ValueType;
    using reference = ValueType&;
    using const_reference = const ValueType&;
    using allocator_type = Allocator;
    using alloc_traits = std::allocator_traits<allocator_type>;

    using pointer = typename alloc_traits::pointer;
    using const_pointer = typename alloc_traits::const_pointer;

    using iterator = pointer;
    using const_iterator = std::add_const_t<pointer>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    using size_type = typename alloc_traits::size_type;
    using difference_type = typename alloc_traits::difference_type;

    static constexpr std::align_val_t alignment = align;

  private:
    static constexpr bool uses_alloc = std::uses_allocator_v<value_type, allocator_type>;
    inline static constexpr auto allocate = [](allocator_type& alloc, size_type number_of_elements) -> pointer {
        if(number_of_elements == 0) return nullptr;

        if constexpr (allocator_alignment_v<allocator_type> >= alignment) {
            return alloc_traits::allocate(alloc, number_of_elements);
        } else {
            return aligned_allocate(alloc, number_of_elements, alignment);
        }
    };

    inline static constexpr auto deallocate = [](allocator_type& alloc, pointer returning_memory,
                                                 size_type number_of_elements) noexcept -> void {
        if(!returning_memory) return;

        if constexpr (allocator_alignment_v<allocator_type> >= alignment) {
            alloc_traits::deallocate(alloc, returning_memory, number_of_elements);
        } else {
            aligned_deallocate(alloc, returning_memory, number_of_elements, alignment);
        }
    };

    template<typename... Types>
    inline static constexpr bool noexcept_construct = detect_noexcept_construct<value_type, allocator_type, Types...>();

    inline static constexpr auto construct =
        []<typename... Types>(allocator_type& alloc, pointer ptr, Types&&... args) noexcept(noexcept_construct<Types...>) requires constructible_by_alloc<value_type, allocator_type, Types...> {
        alloc_traits::construct(alloc, ptr, std::forward<Types>(args)...);
    };

    inline static constexpr auto bind_construct = []<typename... Types>(allocator_type& alloc, Types&&... args) noexcept{
        return [&](pointer ptr) noexcept(noexcept_construct<Types...>){
            alloc_traits::construct(alloc, ptr, std::forward<Types>(args)...);
        };
    };
    
    
    inline static constexpr auto bind_copy_construct = [](allocator_type & alloc) noexcept -> auto {
        return [&alloc]<typename IterOther>(pointer to, IterOther&& from) noexcept(noexcept_construct<std::iter_reference_t<IterOther>>)
            requires constructible_by_alloc<value_type, allocator_type, const value_type&> { 
                construct(alloc, to, *from); 
        };
    };

    inline static constexpr auto bind_move_construct = [](allocator_type & alloc) noexcept -> auto {
        return [&alloc](pointer to, auto&& from) noexcept(noexcept_construct<value_type&&>) 
            requires constructible_by_alloc<value_type, allocator_type, value_type&&> { 
                construct(alloc, to, std::move(*from)); 
        };
    };
    
    [[no_unique_address]] Allocator alloc{};
    pointer array_begin{ nullptr };
    size_type array_size{ 0 };

  public:
    dynamic_array() requires std::is_default_constructible_v<Allocator>
    = default;

    dynamic_array(const allocator_type& alloc) noexcept : alloc{ alloc } {}

    dynamic_array(size_type number_of_elements, const allocator_type& new_alloc = {})
        : alloc{ new_alloc }, array_begin{ allocate(alloc, number_of_elements) }, array_size{ number_of_elements } {

        //auto initalizer = [&]() noexcept(std::is_nothrow_default_constructible_v<value_type>) -> void {
        //    std::uninitialized_value_construct(array_begin, array_begin + array_size);
        //};
        initalize(bind_construct(alloc));
    }

    // clang-format off
    template<std::ranges::sized_range OtherRange>
        requires (nnd::is_not<dynamic_array, OtherRange> && std::constructible_from<value_type, std::ranges::range_reference_t<OtherRange>>)
    dynamic_array(OtherRange&& range_to_copy, const allocator_type& new_alloc = {}) :
        alloc{ new_alloc },
        array_begin{ allocate(alloc, std::ranges::size(range_to_copy)) }, 
        array_size{ std::ranges::size(range_to_copy) } {

            initalize(bind_copy_construct(alloc), std::ranges::begin(range_to_copy));

    }

    template<std::input_iterator RangeBegin, std::sized_sentinel_for<RangeBegin> RangeEnd>
        requires std::constructible_from<value_type, std::iter_reference_t<RangeBegin>>
    dynamic_array(RangeBegin&& copy_start, RangeEnd&& copy_end, const allocator_type& new_alloc = {}) : 
        alloc{ new_alloc }, 
        array_begin{ allocate(alloc, copy_end - copy_start) }, 
        array_size{ copy_end - copy_start } {

            initalize(bind_copy_construct(alloc), std::forward<RangeBegin>(copy_start));

    }

    //I'll most likely need to just replace this with something that uses a tuple of args.
    template<std::invocable<> Functor>
       requires (std::same_as<std::invoke_result_t<Functor>, value_type> || std::constructible_from<value_type, std::invoke_result_t<Functor>>)
    dynamic_array(Functor&& functor, size_type number_of_elements, const allocator_type& new_alloc = {}):
        alloc{ new_alloc },
        array_begin{ allocate(alloc, number_of_elements) }, 
        array_size{ number_of_elements } {

            auto initalizer = [&](pointer construct_location) noexcept(noexcept(new (construct_location) value_type{functor()}))-> void{
                new (construct_location) value_type{std::forward<Functor>(functor)()};
            };
            initalize(initalizer);
    }
        // clang-format on
        dynamic_array(const dynamic_array& other) requires std::is_copy_constructible_v<value_type>
            : alloc{ alloc_traits::select_on_container_copy_construction(other.alloc) }, array_begin{ allocate(alloc, other.array_size) },
              array_size{ other.array_size } {
            auto initalizer = [&](pointer this_side, pointer other_side) noexcept(std::is_nothrow_copy_constructible_v<value_type>) -> void {
                std::uninitialized_copy(other.array_begin, other.array_begin + array_size, array_begin);
            };
            initalize(bind_copy_construct(alloc), other.begin());
        }

        dynamic_array(const dynamic_array& other, const allocator_type& copy_alloc) requires std::is_copy_constructible_v<value_type>
            : alloc{ copy_alloc }, array_begin{ allocate(alloc, other.array_size) }, array_size{ other.array_size } {
            auto initalizer = [&]() noexcept(std::is_nothrow_copy_constructible_v<value_type>) -> void {

                std::uninitialized_copy(other.array_begin, other.array_begin + array_size, array_begin);
            };
            initalize(bind_copy_construct(alloc), other.begin());
        }

        dynamic_array(dynamic_array&& other) noexcept
            : alloc{ std::move(other.alloc) }, array_begin{ other.array_begin }, array_size{ other.array_size } {
            other.array_begin = nullptr;
            other.array_size = 0;
        }

      private:
        template<std::invocable<pointer> Functor>
        void initalize(Functor&& initalizer) noexcept requires(std::is_nothrow_invocable_v<Functor, pointer>) {
            pointer current = begin();
            for (; current != end(); current += 1) {
                initalizer(current);
            }
        }

        template<typename Functor, typename OtherIter>
            requires std::invocable<Functor, pointer, OtherIter>
        void initalize(Functor&& initalizer, OtherIter&& begin_other) noexcept requires(std::is_nothrow_invocable_v<Functor, pointer, OtherIter>) {
            pointer current = begin();
            for (; current != end(); current += 1, begin_other += 1) {
                initalizer(current, std::forward<OtherIter>(begin_other));
            }
        }

        template<std::invocable<pointer> Functor>
        void initalize(Functor&& initalizer)  requires(!std::is_nothrow_invocable_v<Functor, pointer>) {
            pointer current = begin();
            try {
                for (; current != end(); current += 1) {
                    initalizer(current);
                }
            } catch (...) {
                for (; current != begin(); current -= 1) {
                    (current - 1)->~value_type();
                }
                deallocate(alloc, array_begin, array_size);
                array_begin = nullptr;
                array_size = 0;
                throw;
            }
        }

        template<typename Functor, typename OtherIter>
            requires std::invocable<Functor, pointer, OtherIter>
        void initalize(Functor&& initalizer, OtherIter&& begin_other) requires(!std::is_nothrow_invocable_v<Functor, pointer, OtherIter>) {
            pointer current = begin();
            try {
                for (; current != end(); current += 1, begin_other += 1) {
                    initalizer(current, std::forward<OtherIter>(begin_other));
                }
            } catch (...) {
                for (; current != begin(); current -= 1) {
                    (current - 1)->~value_type();
                }
                deallocate(alloc, array_begin, array_size);
                array_begin = nullptr;
                array_size = 0;
                throw;
            }
        }

        void initalize(std::invocable<> auto&& initalizer) noexcept requires(noexcept(initalizer)) { initalizer(); }

        void initalize(std::invocable<> auto&& initalizer) noexcept requires(!noexcept(initalizer)) {
            try {
                initalizer();
            } catch (...) {
                deallocate(alloc, array_begin, array_size);
                array_begin = nullptr;
                array_size = 0;
                throw;
            }
        }

      public :

          ~dynamic_array() {
            if (array_begin != nullptr) {
                if constexpr (!std::is_trivially_destructible_v<value_type>) {
                    std::destroy(array_begin, array_begin + array_size);
                }
                deallocate(alloc, array_begin, array_size);
            }
        }

      private:
        static constexpr bool alloc_always_equal = alloc_traits::is_always_equal::value;
        static constexpr bool alloc_prop_copyassign = alloc_traits::propagate_on_container_copy_assignment::value && !alloc_always_equal;
        static constexpr bool alloc_prop_moveassign = alloc_traits::propagate_on_container_move_assignment::value && !alloc_always_equal;

        static constexpr bool nonprop_nothrow_element_move =
            !(alloc_always_equal || alloc_prop_moveassign) && std::is_nothrow_move_constructible_v<value_type>;

        void no_throw_move(dynamic_array& other) noexcept {
            deallocate(alloc, array_begin, array_size);
            array_begin = std::exchange(other.array_begin, nullptr);
            array_size = std::exchange(other.array_size, 0);
        }
        //TODO: fix this to put constructions through alloc_traits
        void element_copy(const dynamic_array& other, pointer new_array, std::invocable<> auto&& cleanup) noexcept requires
            std::is_nothrow_copy_constructible_v<value_type> {
            std::uninitialized_copy(other.array_begin, other.array_begin + other.array_size, new_array);
            // cleanup intentionally unused; we're the cleanupless overload.
        }

        void element_copy(const dynamic_array& other, pointer new_array, std::invocable<> auto&& cleanup) {
            try {
                std::uninitialized_copy(other.array_begin, other.array_begin + other.array_size, new_array);
            } catch (...) {
                cleanup();
                throw;
            }
        }

        pointer element_copy(const dynamic_array& other, allocator_type& alloc_to_use) noexcept requires std::is_nothrow_copy_constructible_v<value_type>{
            
            if (other.array_size == 0) return nullptr;

            pointer new_array = allocate(alloc_to_use, other.array_size);
            auto copy = bind_copy_construct(alloc_to_use);

            pointer new_array_itr = new_array;
            pointer other_itr = other.array_begin;

            for (; new_array_itr != (new_array + other.array_size); ++new_array_itr, ++other_itr ){
                copy(new_array_itr, other_itr);
            }

            return new_array;
        }

        pointer element_copy(const dynamic_array& other, allocator_type& alloc_to_use){
            
            if (other.array_size == 0) return nullptr;

            pointer new_array = allocate(alloc_to_use, other.array_size);
            auto copy = bind_copy_construct(alloc_to_use);

            pointer new_array_itr = new_array;
            pointer other_itr = other.array_begin;

            try{
                for (; new_array_itr != (new_array + other.array_size); ++new_array_itr, ++other_itr ){
                    copy(new_array_itr, other_itr);
                }
            } catch(...){
                for(; new_array_itr>=new_array; --new_array_itr){
                    new_array_itr->~value_type();
                }
                deallocate(alloc_to_use, new_array, other.array_size);
                throw;
            }

            return new_array;
        }

      public:
        dynamic_array& operator=(const dynamic_array& other) requires (alloc_prop_copyassign) {
            //pointer new_array = allocate(other.alloc, other.array_size);
            allocator_type new_alloc = other.alloc;
            pointer new_array = element_copy(other, new_alloc);

            deallocate(std::exchange(alloc,       new_alloc        ),
                       std::exchange(array_begin, new_array        ),
                       std::exchange(array_size,  other.array_size ));

            return *this;
        }

        dynamic_array& operator=(const dynamic_array& other) {

            //pointer new_array = allocate(alloc, other.array_size);

            //element_copy(other, new_array, [&]() { deallocate(alloc, new_array, other.array_size); });

            pointer new_array = element_copy(other, alloc);

            deallocate(alloc, 
                       std::exchange(array_begin, new_array),
                       std::exchange(array_size,  other.array_size ));

            return *this;
        }

        dynamic_array& operator=(dynamic_array&& other) noexcept requires alloc_always_equal {

            [[unlikely]] if (this == &other) { return *this; }

            no_throw_move(other);
            return *this;
        }

        dynamic_array& operator=(dynamic_array&& other) noexcept requires alloc_prop_moveassign {

            [[unlikely]] if (this == &other) { return *this; }

            no_throw_move(other);
            alloc = other.alloc;
            return *this;
        }

        dynamic_array& operator=(dynamic_array&& other) requires nonprop_nothrow_element_move {

            [[unlikely]] if (this == &other) { return *this; }

            if (alloc == other.alloc) {
                no_throw_move(other);
                return *this;
            }

            pointer new_array = allocate(alloc, other.array_size);
            deallocate(alloc, array_begin, array_size);
            array_begin = new_array;
            array_size = other.array_size;
            initalize(bind_move_construct(alloc));

            deallocate(other.alloc, other.array_begin, other.array_size);
            other.array_begin = nullptr;
            other.array_size = 0;

            return *this;
        }

        dynamic_array& operator=(dynamic_array&& other) {
            [[unlikely]] if (this == &other) { return *this; }

            if (alloc == other.alloc) {
                no_throw_move(other);
                return *this;
            }

            // sigh, gotta do this the hard way

            //pointer new_array = allocate(alloc, other.array_size);

            //element_copy(other, [&]() { deallocate(alloc, new_array, other.array_size); });

            pointer new_array = element_copy(other, alloc);

            deallocate(alloc, array_begin, array_size);
            array_begin = new_array;
            array_size = other.array_size;

            // intentional design decision: even though it'd be free to make this equivalent to a copy,
            // this is a move. Give the same guarentees; make other own nothing.

            deallocate(other.alloc, other.array_begin, other.array_size);
            other.array_begin = nullptr;
            other.array_size = 0;

            return *this;
        }

        // swap

        constexpr size_type size() const { return array_size; }

        constexpr pointer data() { return std::assume_aligned<static_cast<size_t>(alignment)>(array_begin); }

        constexpr const pointer data() const { return std::assume_aligned<static_cast<size_t>(alignment)>(array_begin); }

        constexpr iterator begin() { return std::assume_aligned<static_cast<size_t>(alignment)>(array_begin); }

        constexpr iterator end() { return array_begin + array_size; }

        constexpr reference operator[](size_t index) { return array_begin[index]; }

        constexpr const_iterator begin() const { return std::assume_aligned<static_cast<size_t>(alignment)>(array_begin); }

        constexpr const_iterator end() const { return array_begin + array_size; }

        constexpr const_reference operator[](size_t index) const { return array_begin[index]; }

        constexpr allocator_type get_allocator() { return alloc; }

        constexpr void clear() {
            if (array_begin != nullptr) {
                if constexpr (!std::is_trivially_destructible_v<value_type>) {
                    std::destroy(array_begin, array_begin + array_size);
                }
                deallocate(alloc, array_begin, array_size);
                array_begin = nullptr;
                array_size = 0;
            }
        }
};

// template<typename ValueType, size_t align>
// struct AlignedPtr;
template<typename ValueType, std::align_val_t align = default_align>
using aligned_array = dynamic_array<ValueType, aligned_allocator<ValueType, align>, align>;

namespace pmr{
template<typename ValueType, std::align_val_t align = default_align>
using aligned_array = dynamic_array<ValueType, std::pmr::polymorphic_allocator<ValueType>, align>;
}

} // namespace ann

#endif