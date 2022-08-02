/*
ANN: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef ANN_ALIGNEDALLOCATOR_HPP
#define ANN_ALIGNEDALLOCATOR_HPP

#include <concepts>
#include <cstddef>
#include <memory>
#include <new>
#include <type_traits>

namespace ann {

inline namespace udl {

constexpr std::align_val_t operator""_a(unsigned long long alignment) { return static_cast<std::align_val_t>(alignment); }

} // namespace udl

inline constexpr std::align_val_t align_cast(std::size_t alignment){
    return std::align_val_t{alignment};
}

inline constexpr std::size_t size_cast(std::align_val_t alignment){
    return static_cast<std::size_t>(alignment);
}

template<std::align_val_t alignment>
using alignment_constant = std::integral_constant<std::align_val_t, alignment>;

template<typename Type>
constexpr std::align_val_t align_val_of = static_cast<std::align_val_t>(alignof(Type));

inline constexpr std::align_val_t default_align = 64_a;

template<typename Type, std::align_val_t align>
struct aligned_allocator {

    using value_type = Type;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using propagate_on_container_move_assignment = std::true_type;

    template<typename RebindType>
    struct rebind {
        using other = aligned_allocator<RebindType, align>;
    };

    static constexpr std::align_val_t alignment = align;

    aligned_allocator() = default;

    constexpr aligned_allocator(const aligned_allocator&) noexcept = default;
    template<typename Other>
    constexpr aligned_allocator(const aligned_allocator<Other, align>&) noexcept {}

    constexpr aligned_allocator& operator=(const aligned_allocator&) noexcept = default;

    constexpr aligned_allocator(aligned_allocator&&) noexcept = default;
    template<typename Other>
    constexpr aligned_allocator(aligned_allocator<Other, align>&&) noexcept {}

    constexpr aligned_allocator& operator=(aligned_allocator&&) noexcept = default;

    ~aligned_allocator() = default;

    [[nodiscard]] Type* allocate(std::size_t numberOfElements, std::align_val_t allocAlign) {
        allocAlign = std::max(allocAlign, alignment);
        void* allocatedMemory = ::operator new(numberOfElements * sizeof(Type), allocAlign);
        return std::assume_aligned<static_cast<std::size_t>(alignment)>(static_cast<Type*>(allocatedMemory));
    }

    [[nodiscard]] Type* allocate(std::size_t numberOfElements) {
        void* allocatedMemory = ::operator new(numberOfElements * sizeof(Type), alignment);
        return std::assume_aligned<static_cast<std::size_t>(alignment)>(static_cast<Type*>(allocatedMemory));
    }

    void deallocate(Type* returningMemory, std::size_t numberOfElements, std::align_val_t allocAlign) noexcept {
        allocAlign = std::max(allocAlign, alignment);
        ::operator delete(returningMemory, numberOfElements * sizeof(Type), allocAlign);
    }

    void deallocate(Type* returningMemory, std::size_t numberOfElements) noexcept {
        ::operator delete(returningMemory, numberOfElements * sizeof(Type), alignment);
    }
};

template<typename LHSType, typename RHSType, std::align_val_t align>
constexpr bool operator==(const aligned_allocator<LHSType, align>&, const aligned_allocator<RHSType, align>&) noexcept {
    return true;
}

template<typename LHSType, std::align_val_t lhs_align, typename RHSType, std::align_val_t rhs_align>
    requires (lhs_align != rhs_align)
constexpr bool operator==(const aligned_allocator<LHSType, lhs_align>&, const aligned_allocator<RHSType, rhs_align>&) noexcept{
    return false;
}

template<typename Allocator>
struct allocator_alignment;

template< typename Allocator >
concept alloc_has_alignment = requires{ {Allocator::alignment} -> std::convertible_to<std::align_val_t>; };

template<typename Allocator>
    requires (!alloc_has_alignment<Allocator>)
struct allocator_alignment<Allocator>{
    using type = alignment_constant<align_val_of<std::max_align_t>>;
    static constexpr std::align_val_t value = align_val_of<std::max_align_t>;
};

template<alloc_has_alignment Allocator>
struct allocator_alignment<Allocator>{
    using type = std::integral_constant<std::align_val_t, Allocator::alignment>;
    static constexpr std::align_val_t value = Allocator::alignment;
};

template<typename Type>
constexpr std::align_val_t allocator_alignment_v = allocator_alignment<Type>::value;


} // namespace ann

#endif