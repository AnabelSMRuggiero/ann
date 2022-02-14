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

#include <ranges>
#include <cstddef>
#include <iterator>
#include <memory>
#include <new>
#include <type_traits>
#include <utility>

#include "../TemplateManipulation.hpp"
#include "AlignedAllocator.hpp"

namespace ann {

template<typename ValueType, size_t align>
struct AlignedPtr;

// template<typename ValueType, size_t alignment = alignof(ValueType)>
// struct DynamicArray;

// template<typename ValueType, size_t alignment>
// void swap(DynamicArray<ValueType, alignment> arrA, DynamicArray<ValueType, alignment> arrB);

inline constexpr struct uninit_tag_t {
} uninit_tag;

template<typename Allocator>
using alloc_ptr_t = typename std::allocator_traits<Allocator>::pointer;

template<typename Allocator>
using alloc_size_t = typename std::allocator_traits<Allocator>::size_type;

template<typename Allocator>
constexpr alloc_ptr_t<Allocator>
aligned_allocate(Allocator&& alloc, alloc_size_t<Allocator> number_of_elements, std::align_val_t alignment) = delete;

template<typename Allocator>
constexpr void aligned_deallocate(
    Allocator&& alloc, alloc_ptr_t<Allocator> returning_memory, alloc_size_t<Allocator> number_of_elements,
    std::align_val_t alignment) = delete;

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

template<typename Type>
constexpr std::align_val_t align_val_of = static_cast<std::align_val_t>(alignof(Type));

template<typename ObjToConstruct, typename Alloc, typename... Types>
consteval bool detect_noexcept_construct() {
    using construct_tuple =
        decltype(std::uses_allocator_construction_args<ObjToConstruct>(std::declval<Alloc>(), std::declval<Types>()...));
    using tuple_types = extract_pack_t<construct_tuple>;
    using front_bound_trait = front_bind_t<std::is_nothrow_constructible, ObjToConstruct>;
    using applied_trait = typename decltype(apply_pack(front_bound_trait{}, tuple_types{}))::type;
    return applied_trait::value;
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
        if constexpr (allocator_alignment_v<allocator_type> >= alignment) {
            return alloc_traits::allocate(alloc, number_of_elements);
        } else {
            return aligned_allocate(alloc, number_of_elements, alignment);
        }
    };

    inline static constexpr auto deallocate = [](allocator_type& alloc, pointer returning_memory, size_type number_of_elements) -> void {
        if constexpr (allocator_alignment_v<allocator_type> >= alignment) {
            alloc_traits::deallocate(alloc, returning_memory, number_of_elements);
        } else {
            aligned_deallocate(alloc, returning_memory, number_of_elements, alignment);
        }
    };

    template<typename... Types>
    inline static constexpr bool noexcept_construct = detect_noexcept_construct<value_type, allocator_type, Types...>();

    inline static constexpr auto construct =
        []<typename... Types>(pointer ptr, allocator_type& alloc, Types&&... args) noexcept(noexcept_construct<Types...>) {
        std::uninitialized_construct_using_allocator(ptr, alloc, std::forward<Types>(args)...);
    };

    inline static constexpr auto bind_copy_construct = [](allocator_type & alloc) noexcept(true) -> auto {
        return [&alloc](pointer to, pointer from) noexcept(noexcept_construct<reference>) { construct(to, alloc, *from); };
    };

    inline static constexpr auto bind_move_construct = [](allocator_type & alloc) noexcept(true) -> auto {
        return [&alloc](pointer to, pointer from) noexcept(noexcept_construct<value_type&&>) { construct(to, alloc, std::move(*from)); };
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

        auto initalizer = [&]() noexcept(std::is_nothrow_default_constructible_v<value_type>) -> void {
            std::uninitialized_default_construct(array_begin, array_begin + array_size);
        };
        initalize(initalizer);
    }

    // clang-format off
    template<std::ranges::sized_range OtherRange>
        requires (!std::same_as<dynamic_array, OtherRange> && std::constructible_from<value_type, std::ranges::range_reference_t<OtherRange>>)
    dynamic_array(OtherRange&& range_to_copy, const allocator_type& new_alloc = {}) :
        alloc{ new_alloc },
        array_begin{ allocate(alloc, std::ranges::size(range_to_copy)) }, 
        array_size{ std::ranges::size(range_to_copy) } {

            auto initalizer = [&](){
                std::ranges::uninitialized_copy(std::ranges::begin(std::forward<OtherRange>(range_to_copy)), 
                                                std::ranges::begin(std::forward<OtherRange>(range_to_copy)), 
                                                array_begin,
                                                array_begin+array_size);
            };
            initalize(initalizer);
    }

    template<std::input_iterator RangeBegin, std::sized_sentinel_for<RangeBegin> RangeEnd>
        requires std::constructible_from<value_type, std::iter_reference_t<RangeBegin>>
    dynamic_array(RangeBegin&& copy_start, RangeEnd&& copy_end, const allocator_type& new_alloc = {}) : 
        alloc{ new_alloc }, 
        array_begin{ allocate(alloc, copy_end - copy_start) }, 
        array_size{ copy_end - copy_start } {

            auto initalizer = [&](){
                std::ranges::uninitialized_copy(std::forward<RangeBegin>(copy_start), 
                                                std::forward<RangeEnd>(copy_end), 
                                                array_begin,
                                                array_begin+array_size);
            };
            initalize(initalizer);
    }
    // clang-format on
    dynamic_array(const dynamic_array& other)
        : alloc{ alloc_traits::select_on_container_copy_construction(other.alloc) }, array_begin{ allocate(alloc, other.array_size) },
            array_size{ other.array_size } {
        auto initalizer = [&]() noexcept(std::is_nothrow_copy_constructible_v<value_type>) -> void {
            std::uninitialized_copy(other.array_begin, other.array_begin + array_size, array_size);
        };
        initalize(initalizer);
    }

    dynamic_array(const dynamic_array& other, const allocator_type& copy_alloc)
        : alloc{ copy_alloc }, array_begin{ allocate(alloc, other.array_size) }, array_size{ other.array_size } {
        auto initalizer = [&]() noexcept(std::is_nothrow_copy_constructible_v<value_type>) -> void {
            std::uninitialized_copy(other.array_begin, other.array_begin + array_size, array_size);
        };
        initalize(initalizer);
    }

    dynamic_array(dynamic_array&& other) noexcept
        : alloc{ std::move(other.alloc) }, array_begin{ other.array_begin }, array_size{ other.array_size } {
        other.array_begin = nullptr;
        other.array_size = 0;
    }

    private:
    void initalize(std::invocable<> auto&& initalizer) noexcept requires(noexcept(initalizer)) { initalizer(); }

    void initalize(std::invocable<> auto initalizer) noexcept requires(!noexcept(initalizer)) {
        try {
            initalizer();
        } catch (...) {
            deallocate(array_begin, array_size);
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
        array_begin = other.array_begin;
        array_size = other.array_size;
        other.array_begin = nullptr;
        other.array_size = 0;
    }

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

    public:
    dynamic_array& operator=(const dynamic_array& other) requires alloc_prop_copyassign {
        pointer new_array = allocate(other.alloc, other.array_size);

        element_copy(other, new_array, [&]() { deallocate(other.alloc, new_array, other.array_size); });

        deallocate(alloc, array_begin, array_size);
        alloc = other.alloc;
        array_begin = new_array;
        array_size = other.array_size;

        return *this;
    }

    dynamic_array& operator=(const dynamic_array& other) {

        pointer new_array = allocate(alloc, other.array_size);

        element_copy(other, new_array, [&]() { deallocate(alloc, new_array, other.array_size); });

        deallocate(alloc, array_begin, array_size);
        array_begin = new_array;
        array_size = other.array_size;

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
        std::uninitialized_move(other.array_begin, other.array_begin + array_size, array_begin);

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

        pointer new_array = allocate(alloc, other.array_size);

        element_copy(other, [&]() { deallocate(alloc, new_array, other.array_size); });

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
// template<typename ValueType>
// using AlignedArray = DynamicArray<ValueType, 32>;

} // namespace ann

#endif