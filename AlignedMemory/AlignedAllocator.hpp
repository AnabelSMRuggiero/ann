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

#include <new>
#include <cstddef>
#include <memory>
#include <type_traits>
#include <concepts>

std::allocator<int> test{};

namespace ann{

template<typename Type, std::align_val_t align>
struct aligned_allocator{

  
    using value_type = Type;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using propagate_on_container_move_assignment = std::true_type;
  
    static constexpr std::align_val_t alignment = align;

    Type* allocate(std::size_t numberOfElements, std::align_val_t allocAlign = alignment){
        void* allocatedMemory = ::operator new(numberOfElements*sizeof(Type), allocAlign);
        return static_cast<Type*>(allocatedMemory);
    }

    void deallocate(Type* returningMemory, std::size_t numberOfElements, std::align_val_t allocAlign = alignment){
        ::operator delete(returningMemory, numberOfElements*sizeof(Type), allocAlign);
    }

};

template<typename Allocator>
concept base_allocator_functionality = requires(Allocator alloc){
    typename std::allocator_traits<Allocator>;
    {std::allocator_traits<Allocator>::allocate(alloc, typename std::allocator_traits<Allocator>::size_type{})} ->
        std::same_as<typename std::allocator_traits<Allocator>::pointer>;
    std::allocator_traits<Allocator>::deallocate(alloc, 
        typename std::allocator_traits<Allocator>::pointer{},
        typename std::allocator_traits<Allocator>::size_type{});
};

template<typename Alloc>
concept stateless_allocator = base_allocator_functionality<Alloc> && typename std::allocator_traits<Alloc>::is_always_equal{};

template<base_allocator_functionality Allocator>
struct allocator_deleter{
    using alloc_traits = typename std::allocator_traits<Allocator>;
    using pointer = typename alloc_traits::pointer;
    using size_type = typename alloc_traits::size_type;

    void operator()(pointer returningMemory){
        alloc_traits::deallocate(alloc, returningMemory, memorySize);
    }
    
    [[no_unique_address]] Allocator alloc;
    size_type memorySize;

};

// Due allocator shenangans, could have different semantics.
template<typename Type, base_allocator_functionality Alloc>
    requires std::same_as<Type, typename std::allocator_traits<Alloc>::value_type>
struct allocated_unique;

template<typename Type, stateless_allocator Alloc>
struct allocated_unique<Type, Alloc>{
    using deleter = allocator_deleter<Alloc>;
    using pointer = typename deleter::pointer;
    
    pointer release() noexcept {
        return impl.release();
    }
    
    void reset(pointer ptr = pointer()) noexcept requires(!std::is_array_v<Type>) {
        impl.reset(ptr);
    }

    //array overloads of reset

    void swap(allocated_unique& other) noexcept{
        std::ranges::swap(impl, other.impl);
    }

    pointer get() noexcept {
        return impl.get()
    }
    deleter& get_deleter() noexcept {
        impl.get_deleter();
    }
    const deleter& get_deleter() const noexcept{
        impl.get_deleter();
    }

    operator bool(){
        return bool(impl);
    }

    std::add_lvalue_reference_t<Type> operator*() const noexcept(noexcept(*std::declval<pointer>()))
        requires (!std::is_array_v<Type>){
        return *impl;
    }
    pointer operator->() const noexcept requires (!std::is_array_v<Type>){
        return get();
    }

    Type& operator[](std::size_t index) requires std::is_array_v<Type>{
        return impl[index];
    }
    

    private:
    std::unique_ptr<Type, deleter> impl;
};



}

#endif