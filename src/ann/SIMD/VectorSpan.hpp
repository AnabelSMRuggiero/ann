/*
NNDescent.cpp: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef NND_VECTORSPAN_HPP
#define NND_VECTORSPAN_HPP

#include "../Type.hpp"
#include "../AlignedMemory/DynamicArray.hpp"
#include "SIMDVector.hpp"
#include <type_traits>

namespace ann {

template <typename ElementType, InstructionSet instructions, size_t align>
struct vector_span_iterator {
    using value_type = ElementType;
    using difference_type = std::ptrdiff_t;
    using reference = VectorReference<ElementType, instructions, align>;

    using vector_type = VectorIntrinsic_t<ElementType, instructions>;

    static constexpr size_t alignment = align;
    static constexpr InstructionSet instructionSet = instructions;
    static constexpr size_t elementSize = vectorWidth<instructionSet>;
    static constexpr size_t elementsPerVector = elementSize / sizeof(ElementType);

    vector_span_iterator(ElementType *rangeStart) : currentElement(rangeStart) {}

  private:
    ElementType *currentElement;

  public:
    vector_span_iterator &operator++() {
        currentElement += elementsPerVector;
        return *this;
    }

    vector_span_iterator operator++(int) {
        vector_span_iterator copy = *this;
        currentElement += elementsPerVector;
        return copy;
    }

    vector_span_iterator &operator--() {
        currentElement -= elementsPerVector;
        return *this;
    }

    vector_span_iterator operator--(int) {
        vector_span_iterator copy = *this;
        currentElement -= elementsPerVector;
        return copy;
    }

    vector_span_iterator operator+(std::ptrdiff_t inc) {
        vector_span_iterator copy{ currentElement + (elementsPerVector * inc) };
        return copy;
    }

    vector_span_iterator operator-(std::ptrdiff_t inc) {
        vector_span_iterator copy{ currentElement - (elementsPerVector * inc) };
        return copy;
    }

    std::ptrdiff_t operator-(vector_span_iterator other) { return (currentElement - other.currentElement) / elementsPerVector; }

    bool operator==(vector_span_iterator other) { return currentElement == other.currentElement; }

    reference operator*() { return reference{ nnd::MakeAlignedPtr(currentElement, *this) }; }

    reference operator[](size_t i) { return *(*this + i); }

    vector_span_iterator &operator+=(std::ptrdiff_t inc) {
        *this = *this + inc;
        return *this;
    }

    vector_span_iterator &operator-=(std::ptrdiff_t inc) {
        *this = *this - inc;
        return *this;
    }

    auto operator<=>(vector_span_iterator &rhs) { return currentElement <=> rhs.currentElement; }
};

template <typename ElementType, InstructionSet instructions = defaultInstructionSet, size_t align = 64>
struct vector_span {

    using value_type = std::remove_cv_t<ElementType>;
    using iterator = vector_span_iterator<ElementType, instructions, align>;

    using reference = VectorReference<ElementType, instructions, align>;
    using const_reference = VectorReference<const ElementType, instructions, align>;

    using excess_type = nnd::AlignedSpan<ElementType, align>;

    static constexpr size_t alignment = align;
    static constexpr InstructionSet instructionSet = instructions;

  private:
    ElementType *data;
    size_t fullExtent;

    static constexpr size_t elementSize = vectorWidth<instructionSet>;
    static constexpr size_t elementsPerVector = elementSize / sizeof(ElementType);

  public:
    vector_span() = default;

    vector_span(vector_span &&) = default;
    vector_span &operator=(vector_span &&) = default;

    vector_span(const vector_span &) = default;
    vector_span &operator=(const vector_span &) = default;

    vector_span(ann::aligned_array<std::remove_const_t<ElementType>, static_cast<std::align_val_t>(alignment)> &dataToView) requires(!std::is_const_v<ElementType>)
        : data(dataToView.begin()), fullExtent(dataToView.size()){};

    vector_span(const ann::aligned_array<std::remove_const_t<ElementType>, static_cast<std::align_val_t>(alignment)> &dataToView) requires std::is_const_v<ElementType>
        : data(dataToView.begin()), fullExtent(dataToView.size()){};

    vector_span(nnd::AlignedSpan<ElementType, alignment> &dataToView) requires(!std::is_const_v<ElementType>)
        : data(dataToView.begin()), fullExtent(dataToView.size()){};

    template<typename OtherElement>
        requires (std::same_as<std::remove_const_t<OtherElement>, std::remove_const_t<ElementType>>)
    vector_span(const nnd::AlignedSpan<OtherElement, alignment> &dataToView) requires std::is_const_v<ElementType>
        : data(dataToView.begin()), fullExtent(dataToView.size()){};

    vector_span(
        const vector_span<std::remove_cv_t<ElementType>, instructionSet, alignment> &spanToCopy) requires std::is_const_v<ElementType>
        : data(spanToCopy.begin()), fullExtent(spanToCopy.size()){};

    template <typename ConvertableToElement>
    vector_span(const nnd::AlignedPtr<ConvertableToElement, alignment> spanBegin, const size_t extent)
        : data(static_cast<ElementType *>(spanBegin)), fullExtent(extent){};

    iterator begin() const { return std::assume_aligned<alignment>(data); }

    iterator end() const { return data + fullExtent - fullExtent % elementsPerVector; }

    reference operator[](size_t index) { return reference{ data + index * elementsPerVector }; };

    const_reference operator[](size_t index) const { return const_reference{ data + index * elementsPerVector }; };

    size_t size() const { return fullExtent / elementsPerVector; };

    nnd::AlignedSpan<ElementType, alignment> excess() {
        return nnd::AlignedSpan<ElementType, alignment>{ nnd::MakeAlignedPtr(data + fullExtent - fullExtent % elementsPerVector, *this),
                                                         fullExtent % elementsPerVector };
    }

    nnd::AlignedSpan<const ElementType, alignment> excess() const {
        return nnd::AlignedSpan<const ElementType, alignment>{
            nnd::MakeAlignedPtr(data + fullExtent - fullExtent % elementsPerVector, *this), fullExtent % elementsPerVector
        };
    }

    explicit operator nnd::AlignedSpan<const ElementType, alignment>() const {
        return nnd::AlignedSpan<const ElementType, alignment>{ nnd::MakeAlignedPtr(data, *this), fullExtent };
    }
};

template<typename ElementType, std::size_t alignment>
vector_span(nnd::AlignedSpan<ElementType, alignment>) -> vector_span<ElementType, defaultInstructionSet, alignment>;

} // namespace ann

template <typename ValueType, ann::InstructionSet instructions, std::size_t alignment>
inline constexpr bool std::ranges::enable_borrowed_range<ann::vector_span<ValueType, instructions, alignment>> = true;

#endif