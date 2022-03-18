/*
ANN: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef ANN_ALIGNEDSPAN_HPP
#define ANN_ALIGNEDSPAN_HPP

#include <cstddef>
#include <iterator>
#include <memory>
#include <new>
#include <ranges>
#include <span>
#include <type_traits>

#include "AlignedMemory/AlignedAllocator.hpp"
#include "DynamicArray.hpp"

namespace ann {


template<typename ValueType, std::align_val_t align>
struct aligned_ptr;

template<typename ValueType, std::align_val_t align>
aligned_ptr<ValueType, align> make_aligned_ptr_helper(ValueType* ptr);

template<typename ValueType, std::align_val_t align>
struct aligned_ptr{
    using value_type = ValueType;
    //using reference_type 
    friend aligned_ptr make_aligned_ptr_helper<ValueType, align>(value_type* ptr);
    static constexpr std::align_val_t alignment = align;

    aligned_ptr(const aligned_ptr&) = default;

    aligned_ptr(const aligned_ptr<std::remove_const_t<value_type>, align>& other) requires std::is_const_v<value_type>:
        ptr{static_cast<value_type*>(other)}
        {}

    private:
    value_type* ptr;
    aligned_ptr() = default;

    


    aligned_ptr(value_type* ptr): ptr(ptr) {};
    public:

    explicit operator value_type*() const{
        return ptr;
    }


};

template<typename ValueType, std::align_val_t align>
aligned_ptr<ValueType, align> make_aligned_ptr_helper(ValueType* ptr){
    return aligned_ptr<ValueType, align>{ptr};
}

template<typename ValueType, typename AlignedContainer, std::align_val_t alignment>
ValueType* make_aligned_ptr(ValueType* ptr, AlignedContainer&&, alignment_constant<alignment>) = delete;


template<typename ValueType, typename AlignedContainer>
ValueType* make_aligned_ptr(ValueType* ptr, AlignedContainer&&, alignment_constant<align_val_of<ValueType>> = {}){
    return ptr;
}

template<typename ValueType, typename AlignedRange, std::align_val_t alignment = AlignedRange::alignment>
    requires (std::align_val_t{AlignedRange::alignment} > align_val_of<ValueType> 
             && alignment > align_val_of<ValueType>)
aligned_ptr<ValueType, AlignedRange::alignment> make_aligned_ptr(ValueType* ptr, AlignedRange&&, alignment_constant<alignment> = {}){
    return make_aligned_ptr_helper<ValueType, alignment>(ptr);
}


template<typename Type>
struct is_aligned_contiguous_range : std::false_type {};


template<std::ranges::contiguous_range ArrayLike>
    requires (static_cast<std::size_t>(ArrayLike::alignment) > alignof(std::ranges::range_value_t<ArrayLike>))
struct is_aligned_contiguous_range<ArrayLike> : std::true_type {};

template<typename Range, std::align_val_t min_alignment>
concept aligned_range = std::ranges::contiguous_range<Range> && (static_cast<std::align_val_t>(std::remove_reference_t<Range>::alignment) >= min_alignment);

template<typename Type>
constexpr bool is_aligned_contiguous_range_v = is_aligned_contiguous_range<Type>::value;



template<typename ElementType, std::align_val_t align = default_align>
struct aligned_span{

    using element_type = ElementType;
    using value_type = std::remove_cv_t<ElementType>;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using pointer = ElementType*;
    using const_pointer = const ElementType;
    using reference = ElementType&;
    using const_reference = const ElementType&;
    using iterator = pointer; // Yeah, I'm kinda lazy. I'll whip up an actual iterator if need be.
    using reverse_iterator = std::reverse_iterator<iterator>;
    static constexpr std::align_val_t alignment = align;


    private:
    ElementType* underlying_data;
    size_t extent;

    
    public:

    aligned_span() = default;

    aligned_span(aligned_span&&) = default;
    aligned_span& operator=(aligned_span&&) = default;

    aligned_span(const aligned_span&) = default;
    aligned_span& operator=(const aligned_span&) = default;

    template<aligned_range<alignment> Range>
        requires (std::same_as<value_type, std::ranges::range_value_t<Range>> && std::ranges::sized_range<Range>)
    constexpr aligned_span(Range&& dataToView): underlying_data(std::ranges::data(dataToView)), extent(std::ranges::size(dataToView)){};

    constexpr aligned_span(const aligned_span<std::remove_const_t<ElementType>, alignment>& spanToCopy) requires std::is_const_v<ElementType>: underlying_data(spanToCopy.begin()), extent(spanToCopy.size()){};

    constexpr aligned_span(aligned_ptr<ElementType, alignment> spanBegin, const size_t extent): underlying_data(static_cast<ElementType*>(spanBegin)), extent(extent){};

    constexpr pointer data() const { return std::assume_aligned<alignment>(underlying_data); }

    constexpr iterator begin() const { return data(); }

    constexpr iterator end() const { return underlying_data + extent; }

    constexpr reverse_iterator rbegin() const { return underlying_data + extent; }

    constexpr reverse_iterator rend() const { return data(); }

    constexpr reference operator[](size_t index) const { return underlying_data[index]; }

    constexpr size_type size() const { return extent; }

    constexpr size_type size_bytes() const { return extent*sizeof(element_type); }

    constexpr bool empty() const { return extent != 0; }
    
    template< std::size_t Count > 
    constexpr aligned_span first(){
        return { make_aligned_ptr(underlying_data, *this), Count };
    }
    
    constexpr aligned_span first(std::size_t count){
        return { make_aligned_ptr(underlying_data, *this), count };
    }

    template< std::size_t Count >
    constexpr std::span<element_type, Count> last(){
        return {underlying_data + (extent - Count), Count };
    }

    constexpr std::span<element_type> last(std::size_t count){
        return {underlying_data + (extent - count), count };
    }

    template< std::size_t Offset, std::size_t Count = std::dynamic_extent>
    constexpr auto subspan(){
        constexpr std::align_val_t new_alignment = std::min(alignment, align_cast(sizeof(element_type) * Offset));
        std::size_t span_size = [&]{
            if constexpr (Count == std::dynamic_extent){
                return extent - Offset;
            }
            return Count;
        }();
        return aligned_span{make_aligned_ptr(data(), *this, new_alignment), span_size};
    }
    
    constexpr std::span<element_type> subspan(size_type offset, size_type count = std::dynamic_extent){
        size_type span_size = (count == std::dynamic_extent) ? extent - offset : count;
        return {data(), span_size};
    }
    
};

}


template <typename ElementType, std::align_val_t alignment>
inline constexpr bool std::ranges::enable_borrowed_range<ann::aligned_span<ElementType, alignment>> = true;

#endif