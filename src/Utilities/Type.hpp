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

namespace nnd{



template<std::integral IndexType>
struct IntegralPairHasher{

    size_t operator()(const std::pair<IndexType, IndexType>& pair) const noexcept{
        return std::hash<IndexType>()(size_t(pair.first)*634018663193ul ^ std::hash<IndexType>()(pair.second)*354019652443ul);
    }

};

template<typename DistType>
using DistanceCache = std::unordered_map<std::pair<size_t, size_t>, DistType, IntegralPairHasher<size_t>>;

template<typename ValueType, size_t align>
struct AlignedPtr;

template<typename ValueType, size_t align=32>
struct AlignedArray{
    using value_type = ValueType;
    static const size_t alignment = align;
    private:

    struct AlignedDeleter{

        void operator()(ValueType* arrayToDelete) {operator delete[](arrayToDelete, std::align_val_t(alignment)); };

    };

    std::unique_ptr<ValueType[], AlignedDeleter> data;
    size_t capacity;

    public:

    AlignedArray() = default;

    AlignedArray(size_t size): data(static_cast<ValueType*>(operator new[](size*sizeof(ValueType), std::align_val_t(alignment))), AlignedDeleter()), capacity(size) {
        std::uninitialized_value_construct(this->begin(), this->end());
    };

    AlignedArray(const AlignedArray& other): data(static_cast<ValueType*>(operator new[](other.capacity*sizeof(ValueType), std::align_val_t(alignment))), AlignedDeleter()), capacity(other.capacity) {
        std::uninitialized_copy(other.begin(), other.end(), this->begin());
    };

    AlignedArray(AlignedArray&& rhs) = default;

    ~AlignedArray(){
        if(data){
            for(auto& element: *this){
                //~element();
                element.~ValueType();
            }
        }
    }

    AlignedArray& operator=(AlignedArray&& other) = default;

    AlignedArray& operator=(const AlignedArray& other) = default;

    size_t size() const { return capacity; }

    ValueType* begin() { return std::assume_aligned<alignment>(data.get()); }

    ValueType* end() { return data.get() + capacity; }

    ValueType& operator[](size_t index) { return data[index]; }

    const ValueType* begin() const { return data.get(); }

    const ValueType* end() const { return data.get() + capacity; }

    const ValueType& operator[](size_t index) const{ return data[index]; }

    AlignedPtr<ValueType, align> GetAlignedPtr(size_t entriesToJump);

    AlignedPtr<const ValueType, align> GetAlignedPtr(size_t entriesToJump) const;
};

template<typename ValueType, size_t align>
struct AlignedPtr{
    
    //template<typename ArrayType>
    friend AlignedArray<std::remove_cv_t<ValueType>, align>;


    AlignedPtr& operator+=(const std::ptrdiff_t amount){
        ptr = ptr + amount * entriesToMove;
        return *this;
    }

    operator ValueType*() const{
        return ptr;
    }

    private:
    
    ValueType* ptr;
    size_t entriesToMove;

    AlignedPtr(ValueType* ptr, size_t entriesToMove): ptr(ptr), entriesToMove(entriesToMove){}
};

template<typename ValueType, size_t align>
AlignedPtr<ValueType, align> AlignedArray<ValueType, align>::GetAlignedPtr(size_t entriesToJump){
    return AlignedPtr<ValueType, align>(begin(), entriesToJump);
}

template<typename ValueType, size_t align>
AlignedPtr<const ValueType, align> AlignedArray<ValueType, align>::GetAlignedPtr(size_t entriesToJump) const{
    return AlignedPtr<const ValueType, align>(begin(), entriesToJump);
}

template<typename ElementType, size_t align=32>
struct AlignedSpan{

    using value_type = std::remove_cv_t<ElementType>;
    static const size_t alignment = align;
    private:
    ElementType* data;
    size_t extent;

    public:

    template<typename ConvertableToElement>
    AlignedSpan(const AlignedArray<ConvertableToElement, alignment>& dataToView): data(dataToView.begin()), extent(dataToView.size()){};

    template<typename ConvertableToElement>
    AlignedSpan(const AlignedSpan<ConvertableToElement, alignment>& spanToCopy): data(spanToCopy.data), extent(spanToCopy.extent){};

    template<typename ConvertableToElement>
    AlignedSpan(const AlignedPtr<ConvertableToElement, alignment> spanBegin, const size_t extent): data(spanBegin), extent(extent){};

    ElementType* begin() const { return std::assume_aligned<alignment>(data); }

    ElementType* end() const { return data + extent; }

    ElementType& operator[](size_t index) const { return data[index]; };

    size_t size() const { return extent; };

};


template<std::ranges::contiguous_range Container>
struct DefaultDataView{ using ViewType = std::span<const typename Container::value_type>; };

template<typename ElementType, size_t align>
struct DefaultDataView<AlignedArray<ElementType, align>>{ using ViewType = AlignedSpan<const ElementType, align>; };

template<typename Type>
struct IsAlignedArray : std::false_type {};

template<typename ElementType, size_t align>
struct IsAlignedArray<AlignedArray<ElementType, align>> : std::true_type {};

template<typename Type>
static constexpr bool isAlignedArray_v = IsAlignedArray<Type>::value;

template<typename DataTypeA, typename DataTypeB, typename RetType=double>
using SpaceMetric = RetType (*)(const DataTypeA&, const DataTypeB&);


template<typename DataTypeA, typename DataTypeB, typename RetType=std::vector<double>>
using BatchMetric = RetType (*)(const std::vector<DataTypeA>&, const DataTypeB&);

struct BlockIndecies{
    // The block a data point exists in
    size_t blockNumber;
    // The index within that block
    size_t dataIndex;

};

struct MetaGraphIndex{
    uint32_t metaGraphIndex;
    uint16_t blockNumber;
    uint16_t dataIndex;
};

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


template<typename Type, typename OtherType>
concept IsNot = !std::same_as<Type, OtherType>;

struct SplittingHeurisitcs{
    uint32_t splitThreshold = 80;
    uint32_t childThreshold = 32;
    uint32_t maxTreeSize = 130;
    float maxSplitFraction = 0.0f;
};

template<typename ElementType>
struct OffsetSpan{
    using value_type = ElementType;
    using iterator = typename std::span<ElementType>::iterator;
    using const_iterator = const typename std::span<ElementType>::iterator;

    std::span<ElementType> arrView;
    size_t indexOffset;

    OffsetSpan() = default;

    OffsetSpan(ElementType* arr, size_t arrSize, size_t indexOffset): 
        arrView(arr, arrSize), 
        indexOffset(indexOffset){}

    OffsetSpan(std::span<ElementType> arrView, size_t indexOffset): arrView(arrView), indexOffset(indexOffset) {}

    

    size_t Offset(){
        return indexOffset;
    }

    size_t StopIndex(){
        return indexOffset + arrView.size();
    }

    ElementType& operator[](size_t i){
        return arrView[i-indexOffset];
    }

    const ElementType& operator[](size_t i) const{
        return arrView[i - indexOffset];
    }

    iterator begin(){
        return arrView.begin();
    }

    iterator end(){
        return arrView.end();
    }

    const_iterator begin() const{
        return arrView.begin();
    }

    const_iterator end() const{
        return arrView.end();
    }

    const_iterator cbegin() const{
        return arrView.begin();
    }

    const_iterator cend() const{
        return arrView.end();
    }

    size_t size() const{
        return arrView.size();
    }

};


}


#endif