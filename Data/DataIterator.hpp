/*
NNDescent.cpp: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef NND_DATAITERATOR_HPP
#define NND_DATAITERATOR_HPP

#include <cstddef>
#include <new>

#include "../AlignedMemory/AlignedAllocator.hpp"
#include "../Type.hpp"
#include "../SIMD.hpp"

namespace ann{

template<typename ElementType, std::align_val_t align>
struct data_iterator{
    using value_type = nnd::AlignedSpan<ElementType, static_cast<std::size_t>(align)>;
    using difference_type = std::ptrdiff_t;
    using reference = nnd::AlignedSpan<ElementType, static_cast<std::size_t>(align)>;
    using vector_view = ann::vector_span<value_type, ann::defaultInstructionSet, static_cast<std::size_t>(align)>;
    using const_vector_view = ann::vector_span<const value_type, ann::defaultInstructionSet, static_cast<std::size_t>(align)>;

    static constexpr std::align_val_t alignment = align;

    constexpr data_iterator(const size_t arraySize, const size_t viewSize, ElementType* arrayStart): arraySize(arraySize), viewSize(viewSize), arrayStart(arrayStart) {}

    private:
    size_t arraySize;
    size_t viewSize;
    ElementType* arrayStart;

    public:
    constexpr data_iterator& operator++(){
        arrayStart += arraySize;
        return *this;
    }

    constexpr data_iterator operator++(int){
        data_iterator copy = *this;
        arrayStart += arraySize;
        return copy;
    }

    constexpr data_iterator& operator--(){
        arrayStart -= arraySize;
        return *this;
    }

    constexpr data_iterator operator--(int){
        data_iterator copy = *this;
        arrayStart -= arraySize;
        return copy;
    }

    constexpr data_iterator operator+(std::ptrdiff_t inc) const {
        data_iterator copy{arraySize, viewSize, arrayStart + (arraySize * inc)};
        return copy;
    }

    constexpr data_iterator operator-(std::ptrdiff_t inc) const {
        data_iterator copy{arraySize, viewSize, arrayStart - (arraySize * inc)};
        return copy;
    }

    constexpr std::ptrdiff_t operator-(const data_iterator& other) const{
        return (arrayStart - other.arrayStart)/arraySize;
    }
    
    constexpr bool operator==(const data_iterator& other) const {
        return arrayStart == other.arrayStart;
    }
    
    constexpr reference operator*() const{
        return reference{nnd::MakeAlignedPtr(arrayStart, *this), viewSize};
    }

    constexpr reference operator[](size_t i) const {
        return *(*this + i);
    }

    constexpr data_iterator& operator+=(std::ptrdiff_t inc){
        *this = *this + inc;
        return *this;
    }

    constexpr data_iterator& operator-=(std::ptrdiff_t inc){
        *this = *this - inc;
        return *this;
    }

    constexpr auto operator<=>(const data_iterator& rhs) const {
        return arrayStart<=> rhs.arrayStart;
    }
};

template<typename ValueType, std::align_val_t align>
data_iterator<ValueType, align> operator+(std::ptrdiff_t inc, const data_iterator<ValueType, align>& iter){
    return iter + inc;
}

template<typename ValueType, std::align_val_t align>
data_iterator<ValueType, align> operator-(std::ptrdiff_t inc, const data_iterator<ValueType, align>& iter){
    return iter - inc;
}

}

#endif