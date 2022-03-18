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
#include <vector>

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

    ~aligned_allocator() = default;

    Type* allocate(std::size_t numberOfElements, std::align_val_t allocAlign) {
        allocAlign = std::max(allocAlign, alignment);
        void* allocatedMemory = ::operator new(numberOfElements * sizeof(Type), allocAlign);
        return std::assume_aligned<static_cast<std::size_t>(alignment)>(static_cast<Type*>(allocatedMemory));
    }

    Type* allocate(std::size_t numberOfElements) {
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
template<typename Type>
struct allocator_alignment;

template<typename Type, std::align_val_t align>
struct allocator_alignment<aligned_allocator<Type, align>>{
    using type = std::integral_constant<std::align_val_t, align>;
    static constexpr std::align_val_t value = align;
};

template<typename Type>
constexpr std::align_val_t allocator_alignment_v = allocator_alignment<Type>::value;

template<typename Allocator>
concept base_allocator_functionality = requires(Allocator alloc) {
    typename std::allocator_traits<Allocator>;
    {
        std::allocator_traits<Allocator>::allocate(alloc, typename std::allocator_traits<Allocator>::size_type{})
        } -> std::same_as<typename std::allocator_traits<Allocator>::pointer>;
    std::allocator_traits<Allocator>::deallocate(
        alloc, typename std::allocator_traits<Allocator>::pointer{}, typename std::allocator_traits<Allocator>::size_type{});
};

template<typename Alloc>
concept stateless_allocator = base_allocator_functionality<Alloc> && typename std::allocator_traits<Alloc>::is_always_equal{};

template<base_allocator_functionality Allocator>
struct allocator_deleter {
    using alloc_traits = typename std::allocator_traits<Allocator>;
    using pointer = typename alloc_traits::pointer;
    using size_type = typename alloc_traits::size_type;

    void operator()(pointer returningMemory) { alloc_traits::deallocate(alloc, returningMemory, memorySize); }

    [[no_unique_address]] Allocator alloc;
    size_type memorySize;
};

} // namespace ann

#endif