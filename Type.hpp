/*
NNDescent.cpp: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef NND_GENERALTYPE_HPP
#define NND_GENERALTYPE_HPP

#include <iterator>
#include <vector>
#include <concepts>
#include <memory>
#include <ranges>
#include <algorithm>
#include <functional>
#include <unordered_map>
#include <utility>
#include <span>
#include <new>
#include <type_traits>
#include <cstdint>
#include <memory_resource>
#include <cassert>

namespace nnd{



template<std::integral IndexType, std::integral OtherIndex = IndexType>
struct IntegralPairHasher{

    size_t operator()(const std::pair<IndexType, OtherIndex>& pair) const noexcept{
        return std::hash<IndexType>()(size_t(pair.first)*634018663193ul ^ std::hash<IndexType>()(pair.second)*354019652443ul);
    }

};


template<typename ValueType, size_t align>
struct AlignedPtr;



//template<typename ValueType, size_t align>
//struct AlignedPtr;
//template<typename ValueType>
//using AlignedArray = DynamicArray<ValueType, 32>;


template<typename ValueType, size_t align>
struct AlignedPtr;

template<typename ValueType, size_t align>
AlignedPtr<ValueType, align> MakeAlignedPtrHelper(ValueType* ptr);

template<typename ValueType, std::size_t align>
struct AlignedPtr{
    using value_type = ValueType;
    //using reference_type 
    friend AlignedPtr MakeAlignedPtrHelper<ValueType, align>(ValueType* ptr);
    static constexpr size_t alignment = align;

    AlignedPtr(const AlignedPtr&) = default;

    AlignedPtr(const AlignedPtr<std::remove_const_t<ValueType>, align>& other) requires std::is_const_v<ValueType>:
        ptr{static_cast<ValueType*>(other)}
        {}

    private:
    ValueType* ptr;
    AlignedPtr() = default;

    


    AlignedPtr(ValueType* ptr): ptr(ptr) {};
    public:

    explicit operator ValueType*() const{
        return ptr;
    }


};

template<typename ValueType, std::size_t align>
AlignedPtr<ValueType, align> MakeAlignedPtrHelper(ValueType* ptr){
    return AlignedPtr<ValueType, align>{ptr};
}

template<typename ValueType, typename AlignedContainer>
ValueType* MakeAlignedPtr(ValueType* ptr, AlignedContainer&){
    return ptr;
}

template<typename ValueType, typename AlignedContainer>
    requires (std::align_val_t{AlignedContainer::alignment} >= std::align_val_t{alignof(ValueType)})
AlignedPtr<ValueType, static_cast<size_t>(AlignedContainer::alignment)> MakeAlignedPtr(ValueType* ptr, AlignedContainer&){
    return MakeAlignedPtrHelper<ValueType, static_cast<std::size_t>(AlignedContainer::alignment)>(ptr);
}


template<typename Type, typename OtherType>
concept is_not = !std::same_as<std::remove_cvref_t<Type>, std::remove_cvref_t<OtherType>>;

template<typename Type>
struct is_aligned_contiguous_range : std::false_type {};


template<std::ranges::contiguous_range ArrayLike>
    requires (static_cast<std::size_t>(ArrayLike::alignment) > alignof(std::ranges::range_value_t<ArrayLike>))
struct is_aligned_contiguous_range<ArrayLike> : std::true_type {};

template<typename Range, std::align_val_t min_alignment>
concept aligned_range = std::ranges::contiguous_range<Range> && (static_cast<std::align_val_t>(std::remove_reference_t<Range>::alignment) >= min_alignment);

template<typename Type>
constexpr bool is_aligned_contiguous_range_v = is_aligned_contiguous_range<Type>::value;



template<typename ElementType, std::size_t align=64>
struct AlignedSpan{

    using value_type = std::remove_cv_t<ElementType>;
    static constexpr std::size_t alignment{align};


    private:
    ElementType* data;
    size_t extent;

    
    public:

    AlignedSpan() = default;

    AlignedSpan(AlignedSpan&&) = default;
    AlignedSpan& operator=(AlignedSpan&&) = default;

    AlignedSpan(const AlignedSpan&) = default;
    AlignedSpan& operator=(const AlignedSpan&) = default;

    template<aligned_range<static_cast<std::align_val_t>(alignment)> Range>
        requires (std::same_as<value_type, std::ranges::range_value_t<Range>> && std::ranges::sized_range<Range>)
    AlignedSpan(Range&& dataToView): data(std::ranges::data(dataToView)), extent(std::ranges::size(dataToView)){};

    //template<is_not<AlignedSpan> ConvertableToElement>
    AlignedSpan(const AlignedSpan<std::remove_const_t<ElementType>, alignment>& spanToCopy) requires std::is_const_v<ElementType>: data(spanToCopy.begin()), extent(spanToCopy.size()){};

    
    AlignedSpan(AlignedPtr<ElementType, alignment> spanBegin, const size_t extent): data(static_cast<ElementType*>(spanBegin)), extent(extent){};

    ElementType* begin() const { return std::assume_aligned<alignment>(data); }

    ElementType* end() const { return data + extent; }

    ElementType& operator[](size_t index) const { return data[index]; };

    size_t size() const { return extent; };
    
    template<typename Alloc = std::allocator<ElementType>>
    explicit operator std::vector<ElementType, Alloc>(){
        std::vector<std::remove_cv_t<ElementType>, Alloc> retVec(this->size());
        std::copy(this->begin(), this->end(), retVec.begin());

        return retVec;
    }

};

template<std::ranges::contiguous_range Container>
struct DefaultDataView{ using ViewType = std::span<const typename Container::value_type>; };

template<std::ranges::contiguous_range Container>
    requires (is_aligned_contiguous_range_v<Container>)
struct DefaultDataView<Container>{ using ViewType = AlignedSpan<const typename Container::value_type, static_cast<std::size_t>(Container::alignment)>; };


//template<typename ElementType, size_t align>
//struct DefaultDataView<DynamicArray<ElementType, align>>{ using ViewType = AlignedSpan<const ElementType, align>; };


//template<typename ElementType>//, size_t align>
//struct is_aligned_contiguous_range<DynamicArray<ElementType, 32>> : std::true_type {};







template<std::ranges::range TopRange, std::ranges::range BotRange>
struct ZipSentinel{
    using TopSentinel = std::ranges::sentinel_t<TopRange>;
    using BotSentinel = std::ranges::sentinel_t<BotRange>;

    TopSentinel topSent;
    BotSentinel botSent;

    ZipSentinel(TopRange& topRange, BotRange& botRange): topSent(std::ranges::end(topRange)), botSent(std::ranges::end(botRange)) {};

    

    bool operator==(ZipSentinel& other){
        return (topSent == other.topSent) && (botSent == other.botSent);
    }
    /*
    bool operator==(ZipIterator<TopRange, BotRange>& other){
        return (topSent == other.topItr) && (botSent == other.botItr);
    }
    */
};

template<std::ranges::range TopRange, std::ranges::range BotRange>
struct ZipIterator{
    using TopIterator = std::ranges::iterator_t<TopRange>;
    using TopValue = std::ranges::range_value_t<TopRange>;
    using TopRef = std::ranges::range_reference_t<TopRange>;

    using BotIterator = std::ranges::iterator_t<BotRange>;    
    using BotValue = std::ranges::range_value_t<BotRange>;
    using BotRef = std::ranges::range_reference_t<BotRange>;

    TopIterator topItr;
    BotIterator botItr;

    ZipIterator(TopRange& topRange, BotRange& botRange): topItr(std::ranges::begin(topRange)), botItr(std::ranges::begin(botRange)) {};

    std::pair<TopRef, BotRef> operator*(){
        /*
        if constexpr (std::is_const_v<TopRange> && std::is_const_v<BotRange>){
            return std::pair<std::reference_wrapper<const TopValue>, std::reference_wrapper<const BotValue>> (*topItr, *botItr);

        } else if (std::is_const_v<TopRange>){
            return std::pair<std::reference_wrapper<const TopValue>, std::reference_wrapper<BotValue>> (*topItr, *botItr);

        } else if (std::is_const_v<BotRange>){
            return std::make_pair<std::reference_wrapper<TopValue>, std::reference_wrapper<const BotValue>>(*topItr, *botItr);

        } else {
            return std::pair<std::reference_wrapper<TopValue>, std::reference_wrapper<BotValue>> (*topItr, *botItr);
        }
        */
        return std::make_pair(std::reference_wrapper(*topItr), std::reference_wrapper(*botItr));
    }

    ZipIterator& operator++(){
        ++topItr;
        ++botItr;
        return *this;
    }

    ZipIterator operator++(int){
        ZipIterator copy = *this;
        operator++();
        return copy;
    }

    ZipIterator& operator--(){
        --topItr;
        --botItr;
        return *this;
    }

    ZipIterator operator--(int){
        ZipIterator copy = *this;
        operator--();
        return copy;
    }

    bool operator==(ZipIterator& other){
        return (topItr == other.topItr) && (botItr == other.botItr);
    }

    bool operator==(ZipSentinel<TopRange, BotRange>& other){
        return (topItr == other.topSent) && (botItr == other.botSent);
    }
};

template<std::ranges::range TopRange, std::ranges::range BotRange>
struct ZipRange{
    TopRange& topRange;
    BotRange& botRange;

    ZipRange(TopRange& top, BotRange& bot): topRange(top), botRange(bot) {};

    ZipIterator<TopRange, BotRange> begin(){
        return ZipIterator(topRange, botRange);
    }

    ZipSentinel<TopRange, BotRange> end(){
        return ZipSentinel(topRange, botRange);
    }
};


template<typename DataType>
concept TriviallyCopyable = std::is_trivially_copyable_v<DataType>;

}

template <typename ValueType, std::size_t alignment>
inline constexpr bool std::ranges::enable_borrowed_range<nnd::AlignedSpan<ValueType, alignment>> = true;


#endif