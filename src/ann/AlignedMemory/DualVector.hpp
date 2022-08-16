/*
NNDescent.cpp: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef ANN_DUALVECTOR_HPP
#define ANN_DUALVECTOR_HPP

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <initializer_list>
#include <iterator>
#include <limits>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>

#include <filesystem>
#include <vector>

#include "ContainerHelpers.hpp"



namespace ann{

template<typename FirstType, typename SecondType>
struct layout_order{
    using first = std::conditional_t<alignof(FirstType) >= alignof(SecondType),
                                     FirstType,
                                     SecondType>;
    using second = std::conditional_t<alignof(FirstType) >= alignof(SecondType),
                                      SecondType,
                                      FirstType>;

    static constexpr bool reversed = alignof(FirstType) < alignof(SecondType);

};

template<typename Allocator>
struct bind_allocator{
    using allocator_type = Allocator;
    using alloc_traits = std::allocator_traits<Allocator>;

    private:
    template<typename Pointer>
    using element = typename std::pointer_traits<Pointer>::element_type;
    public:

    template<typename Pointer, typename... Types>
    static constexpr bool noexcept_construct = detect_noexcept_construct<element<Pointer>, allocator_type, Types...>();

    inline static constexpr auto construct =
        []<typename Pointer, typename... Types>(allocator_type& alloc, Pointer ptr, Types&&... args) noexcept(noexcept_construct<Pointer, Types...>) requires constructible_by_alloc<element<Pointer>, allocator_type, Types...> {
        alloc_traits::construct(alloc, ptr, std::forward<Types>(args)...);
    };

    inline static constexpr auto bind_construct = []<typename... Types>(allocator_type& alloc, Types&&... args) noexcept{
        return [&] <typename Pointer, typename... InnerTypes> (Pointer ptr, InnerTypes&&... inner_args) noexcept(noexcept_construct<Pointer, Types...>){
            alloc_traits::construct(alloc, ptr, std::forward<Types>(args)..., std::forward<InnerTypes>(inner_args)...);
        };
    };

    inline static constexpr auto bind_destroy = []<typename... Types>(allocator_type& alloc) noexcept{
        return [&] <typename Pointer> (Pointer ptr) noexcept{
            alloc_traits::destroy(alloc, ptr);
        };
    };
    
    
    inline static constexpr auto bind_copy_construct = [](allocator_type & alloc) noexcept -> auto {
        return [&alloc]<typename Pointer, typename IterOther>(Pointer to, IterOther&& from) noexcept(noexcept_construct<Pointer, std::iter_reference_t<IterOther>>)
            requires constructible_by_alloc<element<Pointer>, allocator_type, const element<Pointer>&> { 
                construct(alloc, to, *from); 
        };
    };

    inline static constexpr auto bind_move_construct = [](allocator_type & alloc) noexcept -> auto {
        return [&alloc]<typename Pointer, typename IterOther>(Pointer to, IterOther&& from) noexcept(noexcept_construct<Pointer, std::iter_rvalue_reference_t<IterOther>>) 
            requires constructible_by_alloc<element<Pointer>, allocator_type, std::iter_rvalue_reference_t<IterOther>> { 
                construct(alloc, to, std::ranges::iter_move(from)); 
        };
    };

    template<typename Range>
    inline static constexpr auto bind_forward_construct = [](allocator_type & alloc) noexcept -> auto {
        return [&alloc] <typename Pointer> (Pointer to, auto&& from)
                    noexcept(noexcept_construct<Pointer, range_forward_reference<Range>>) 
                    requires constructible_by_alloc<element<Pointer>, allocator_type, range_forward_reference<Range>> { 
            construct(alloc, to, iter_forward_like<Range>(from)); 
        };
    };
    
};
/*
template<typename OtherIter, typename Pointer, typename Alloc, auto Binder>
constexpr bool bound_alloc_operation_check = requires(OtherIter&& iter, Pointer&& pointer, Alloc&& alloc){
    Binder(alloc)(pointer, iter);
};
*/
template<typename OtherIter, typename Pointer, typename Alloc>
concept bound_copyable = requires(OtherIter&& iter, Pointer&& pointer, Alloc&& alloc){
    typename bind_allocator<Alloc>;
    bind_allocator<Alloc>::bind_copy_construct(alloc)(pointer, iter);
};

template<typename OtherType, typename ValueType, typename Alloc>
concept alloc_constructable = requires(OtherType&& other_object, Alloc&& allocator, ValueType* ptr){
    typename std::allocator_traits<Alloc>;
    std::allocator_traits<Alloc>::construct(allocator, ptr, std::forward<OtherType>(other_object));
};

template<std::input_iterator InIter, std::sentinel_for<InIter> InSent, typename OutPtr, typename Alloc>
auto uninitialized_alloc_copy(InIter in_iter, InSent in_sent, OutPtr out_ptr, Alloc& allocator){
    OutPtr out_begin = out_ptr;
    auto bound_copy = bind_allocator<Alloc>::bind_copy_construct(allocator);
    try{
        for (; in_iter != in_sent; ++out_ptr, ++in_iter ){
            bound_copy(out_ptr, in_iter);
        }
    } catch(...){
        auto bound_destroy = bind_allocator<Alloc>::bind_destroy(allocator);
        while( out_ptr!=out_begin){
            bound_destroy(--out_ptr);
        }
        throw;
    }
    return std::ranges::in_out_result<InIter, OutPtr>{in_iter, out_ptr};
};

template<std::input_iterator InIter, std::sentinel_for<InIter> InSent, typename OutPtr, typename Alloc>
    requires std::is_trivially_copyable_v<std::iter_value_t<OutPtr>>
auto uninitialized_alloc_copy(InIter in_iter, InSent in_sent, OutPtr out_ptr, Alloc& allocator){
    *out_ptr = *in_iter;
    static_assert(std::indirectly_writable<OutPtr, std::iter_reference_t<InIter>>);
    std::ranges::copy(in_iter, in_sent, out_ptr);
};

template<std::input_iterator InIter, std::sentinel_for<InIter> InSent, typename OutPtr, typename Alloc>
auto uninitialized_alloc_move(InIter in_iter, InSent in_sent, OutPtr out_ptr, Alloc& allocator){
    OutPtr out_begin = out_ptr;
    auto bound_move = bind_allocator<Alloc>::bind_move_construct(allocator);
    try{
        for (; in_iter != in_sent; ++out_ptr, ++in_iter ){
            bound_move(out_ptr, in_iter);
        }
    } catch(...){
        auto bound_destroy = bind_allocator<Alloc>::bind_destroy(allocator);
        while( out_ptr!=out_begin){
            bound_destroy(--out_ptr);
        }
        throw;
    }
    return std::ranges::in_out_result<InIter, OutPtr>{in_iter, out_ptr};
};

template<std::input_iterator InIter, std::sentinel_for<InIter> InSent, typename OutPtr, typename Alloc>
auto uninitialized_alloc_construct(InIter begin, InSent end, Alloc& allocator){
    InIter original_begin = begin;
    auto bound_construct = bind_allocator<Alloc>::bind_construct(allocator);
    try{
        for (; begin != end; ++begin){
            bound_construct(begin);
        }
    } catch(...){
        auto bound_destroy = bind_allocator<Alloc>::bind_destroy(allocator);
        while( begin!=original_begin){
            bound_destroy(--begin);
        }
        throw;
    }
    return begin;
};

template<typename Alloc, typename Value>
decltype(auto) move_if_safe(Value& value){
    if constexpr (detect_noexcept_construct<std::remove_cvref_t<Value>, Alloc, Value>()){
        return std::forward<Value>(value);
    } else {
        return static_cast<std::remove_reference_t<Value>&>(value);
    }
}

template<typename Pointer, typename Alloc>
auto alloc_destroy(Pointer begin, Pointer end, Alloc& alloc){
    auto bound_destroy = bind_allocator<Alloc>::bind_destroy(alloc);
    for(; begin != end; ++begin){
        bound_destroy(begin);
    }
}


template<typename Type>
struct is_pair_imp : std::false_type{};

template<typename FirstType, typename SecondType>
struct is_pair_imp<std::pair<FirstType, SecondType>> : std::true_type{};

template<typename Type>
concept is_pair = is_pair_imp<std::remove_cvref_t<Type>>::value;

template<is_pair PairRef>
    requires std::is_reference_v<PairRef>
struct construct_wrapper{
    construct_wrapper(PairRef pair_in): pair{pair_in} {}
    PairRef pair;
}; 
template<typename Pair>
construct_wrapper(Pair&&)->construct_wrapper<Pair&&>;

template<typename PairRef>
    requires std::is_reference_v<PairRef>
struct assign_wrapper{
    assign_wrapper(PairRef pair_in): pair{pair_in}{}
    PairRef pair;
};
template<typename Pair>
assign_wrapper(Pair&&)->assign_wrapper<Pair&&>;

template<typename FirstType, typename SecondType>
    requires std::is_reference_v<FirstType> && std::is_reference_v<SecondType>
struct dual_vector_reference{
    FirstType first;
    SecondType second;

    private:
    using first_value = std::remove_reference_t<FirstType>;
    using second_value = std::remove_reference_t<SecondType>;

    static constexpr bool is_non_const = !std::is_const_v<first_value> &&
                                         !std::is_const_v<second_value>;
    public:

    constexpr dual_vector_reference(FirstType&& first_ref, SecondType&& second_ref):
        first{std::forward<FirstType>(first_ref)},
        second{std::forward<SecondType>(second_ref)}{}

    constexpr dual_vector_reference(const dual_vector_reference&) = default;
    
    constexpr dual_vector_reference(const dual_vector_reference<std::remove_cvref_t<FirstType>&, std::remove_cvref_t<SecondType>&>& other) 
        requires(!is_non_const):
        first{other.first},
        second{other.second} {}

    constexpr dual_vector_reference(const dual_vector_reference<std::remove_cvref_t<FirstType>&&, std::remove_cvref_t<SecondType>&&>& other) 
        requires(!is_non_const):
        first{other.first},
        second{other.second} {}

    template<
        std::convertible_to<first_value> FirstOther,
        std::convertible_to<second_value> SecondOther
    >
    constexpr dual_vector_reference(std::pair<FirstOther, SecondOther>&& other_pair):
        first{other_pair.first},
        second{other_pair.second}{}

    template<
        std::convertible_to<first_value> FirstOther,
        std::convertible_to<second_value> SecondOther
    >
    constexpr dual_vector_reference(const std::pair<FirstOther, SecondOther>& other_pair) requires(!is_non_const):
        first{other_pair.first},
        second{other_pair.second}{}

    template<
        std::convertible_to<first_value> FirstOther,
        std::convertible_to<second_value> SecondOther
    >
    constexpr dual_vector_reference(std::pair<FirstOther, SecondOther>& other_pair):
        first{other_pair.first},
        second{other_pair.second}{}
    
    constexpr dual_vector_reference& operator=(assign_wrapper<const dual_vector_reference&> other_reference) {
        first = std::forward<FirstType>(other_reference.pair.first);
        second = std::forward<SecondType>(other_reference.pair.second);
        return *this;
    }

    constexpr const dual_vector_reference& operator=(assign_wrapper<const dual_vector_reference&> other_reference) const {
        first = std::forward<FirstType>(other_reference.pair.first);
        second = std::forward<SecondType>(other_reference.pair.second);
        return *this;
    }

    template<
        std::convertible_to<first_value> FirstOther,
        std::convertible_to<second_value> SecondOther
    >
    constexpr const dual_vector_reference& operator=(assign_wrapper<const dual_vector_reference<FirstOther&&, SecondOther&&>&> other_reference) const {
        first = std::forward<FirstOther>(other_reference.pair.first);
        second = std::forward<SecondOther>(other_reference.pair.second);
        return *this;
    }

    
    /*
    template<is_pair Pair>
    constexpr dual_vector_reference& operator=(assign_wrapper<Pair&&> wrapped){
        do_pair_assign(std::forward<Pair>(wrapped.pair));
        return *this;
    }
    */
    
   
    template<
        std::convertible_to<first_value> FirstOther,
        std::convertible_to<second_value> SecondOther
    >
    constexpr const dual_vector_reference& operator=(std::pair<FirstOther, SecondOther>&& other_pair) const {
        first = std::forward<FirstOther>(other_pair.first);
        second = std::forward<SecondOther>(other_pair.second);
        return *this;
    }
    
    template<
        typename FirstOther,
        typename SecondOther
    >
        requires(std::assignable_from<FirstType, FirstOther> && std::assignable_from<SecondType, SecondOther>)
    constexpr const dual_vector_reference& operator=(const std::pair<FirstOther, SecondOther>& other_pair) const {
        first = other_pair.first;
        second = other_pair.second;
        return *this;
    }
    
    public:

    operator std::pair<first_value, second_value>() const {
        return {std::forward<FirstType>(first), std::forward<SecondType>(second)};
    }

};


template<
    typename LHSFirst,
    typename LHSSecond,
    std::three_way_comparable_with<LHSFirst> RHSFirst,
    std::three_way_comparable_with<LHSFirst> RHSSecond
>
auto operator<=>(const dual_vector_reference<LHSFirst, LHSSecond>& lhs, const std::pair<RHSFirst, RHSSecond>& rhs)
    -> std::common_type_t<decltype(lhs.first <=> rhs.first), decltype(lhs.second <=> rhs.second)> {
    
    return (lhs.first <=> rhs.first != 0) ? lhs.first <=> rhs.first : lhs.second <=> rhs.second;
}

template<
    typename LHSFirst,
    typename LHSSecond,
    std::three_way_comparable_with<LHSFirst> RHSFirst,
    std::three_way_comparable_with<LHSFirst> RHSSecond
>
auto operator<=>(const std::pair<LHSFirst, LHSSecond>& lhs, const dual_vector_reference<RHSFirst, RHSSecond>& rhs)
    -> std::common_type_t<decltype(lhs.first <=> rhs.first), decltype(lhs.second <=> rhs.second)> {
    
    return (lhs.first <=> rhs.first != 0) ? lhs.first <=> rhs.first : lhs.second <=> rhs.second;
}

template<
    typename LHSFirst,
    typename LHSSecond,
    std::three_way_comparable_with<LHSFirst> RHSFirst,
    std::three_way_comparable_with<LHSFirst> RHSSecond
>
auto operator<=>(const dual_vector_reference<LHSFirst, LHSSecond>& lhs, const dual_vector_reference<RHSFirst, RHSSecond>& rhs)
    -> std::common_type_t<decltype(lhs.first <=> rhs.first), decltype(lhs.second <=> rhs.second)> {
    
    return (lhs.first <=> rhs.first != 0) ? lhs.first <=> rhs.first : lhs.second <=> rhs.second;
}

template<
    typename LHSFirst,
    typename LHSSecond,
    std::equality_comparable_with<LHSFirst> RHSFirst,
    std::equality_comparable_with<LHSFirst> RHSSecond
>
bool operator==(const dual_vector_reference<LHSFirst, LHSSecond>& lhs, const std::pair<RHSFirst, RHSSecond>& rhs){
    
    return lhs.first == rhs.first && lhs.second == rhs.second;
}

template<
    typename LHSFirst,
    typename LHSSecond,
    std::equality_comparable_with<LHSFirst> RHSFirst,
    std::equality_comparable_with<LHSFirst> RHSSecond
>
bool operator==(const std::pair<LHSFirst, LHSSecond>& lhs, const dual_vector_reference<RHSFirst, RHSSecond>& rhs){
    
    return lhs.first == rhs.first && lhs.second == rhs.second;
}

template<
    typename LHSFirst,
    typename LHSSecond,
    std::equality_comparable_with<LHSFirst> RHSFirst,
    std::equality_comparable_with<LHSFirst> RHSSecond
>
bool operator==(const dual_vector_reference<LHSFirst, LHSSecond>& lhs, const dual_vector_reference<RHSFirst, RHSSecond>& rhs){
    
    return lhs.first == rhs.first && lhs.second == rhs.second;
}

static_assert(std::equality_comparable<dual_vector_reference<float&, double&>>);

template<typename Type>
using preserve_rref = std::conditional_t<std::is_rvalue_reference_v<Type>, Type&&, Type&>;

template<typename FirstType, typename SecondType>
dual_vector_reference(std::pair<FirstType, SecondType>&) -> dual_vector_reference<preserve_rref<FirstType>, preserve_rref<SecondType&>>;

template<typename FirstType, typename SecondType>
dual_vector_reference(const std::pair<FirstType, SecondType>&) -> dual_vector_reference<const FirstType&, const SecondType&>;

template<typename FirstType, typename SecondType>
dual_vector_reference(std::pair<FirstType, SecondType>&&) -> dual_vector_reference<FirstType&&, SecondType&&>;



template<typename FirstPointer, typename SecondPointer>
struct dual_vector_iterator{
    private:

    using first_pointer = FirstPointer;
    using second_pointer = SecondPointer;

    template<typename Pointer>
    using element = typename std::pointer_traits<Pointer>::element_type;

    using first = element<first_pointer>;
    using second = element<second_pointer>;


    static constexpr bool is_const_iter = std::is_const_v<first> && std::is_const_v<second>;

    using nonconst_first_pointer = typename std::pointer_traits<FirstPointer>::template rebind<std::remove_const_t<first>>;
    using nonconst_second_pointer = typename std::pointer_traits<SecondPointer>::template rebind<std::remove_const_t<second>>;
    
    using nonconst_iter = dual_vector_iterator<nonconst_first_pointer, nonconst_second_pointer>;

    template<typename Independent, typename Dependent>
    using make_dependent = std::conditional_t<true, Independent, Dependent>;

    public:
    
    using value_type = std::pair<first, second>;
    using difference_type = std::ptrdiff_t;
    using reference = dual_vector_reference<first&, second&>;
    using const_reference = dual_vector_reference<const first&, const second&>;

    first_pointer first_ptr;
    second_pointer second_ptr;

    public:

    constexpr dual_vector_iterator() = default;

    constexpr dual_vector_iterator(first_pointer first, second_pointer second):
        first_ptr{first},
        second_ptr{second}{}

    template<std::same_as<nonconst_iter> OtherIter, typename = std::enable_if_t<is_const_iter, make_dependent<nonconst_iter, OtherIter>>>
    constexpr dual_vector_iterator(OtherIter other):
        first_ptr{other.first_ptr},
        second_ptr{other.second_ptr} {}

    constexpr dual_vector_iterator& operator++(){
        ++first_ptr;
        ++second_ptr;
        return *this;
    }

    constexpr dual_vector_iterator operator++(int){
        dual_vector_iterator copy = *this;
        ++*this;
        return copy;
    }

    constexpr dual_vector_iterator& operator--(){
        --first_ptr;
        --second_ptr;
        return *this;
    }

    constexpr dual_vector_iterator operator--(int){
        dual_vector_iterator copy = *this;
        --*this;
        return copy;
    }

    constexpr dual_vector_iterator operator+(std::ptrdiff_t inc) const {
        dual_vector_iterator copy{first_ptr + inc, second_ptr + inc};
        return copy;
    }

    constexpr dual_vector_iterator operator-(std::ptrdiff_t inc) const {
        dual_vector_iterator copy{first_ptr - inc, second_ptr - inc};
        return copy;
    }

    constexpr std::ptrdiff_t operator-(const dual_vector_iterator& other) const{
        return first_ptr - other.first_ptr;
    }
    
    constexpr bool operator==(const dual_vector_iterator& other) const {
        return first_ptr == other.first_ptr;
    }
    
    constexpr reference operator*() const{
        return reference{*first_ptr, *second_ptr};
    }

    constexpr reference operator[](size_t i) const {
        return *(*this + i);
    }

    constexpr dual_vector_iterator& operator+=(std::ptrdiff_t inc){
        *this = *this + inc;
        return *this;
    }

    constexpr dual_vector_iterator& operator-=(std::ptrdiff_t inc){
        *this = *this - inc;
        return *this;
    }

    constexpr auto operator<=>(const dual_vector_iterator& rhs) const {
        return first_ptr <=> rhs.first_ptr;
    }

};

template<typename FirstPointer, typename SecondPointer>
dual_vector_iterator<FirstPointer, SecondPointer> operator+(std::ptrdiff_t inc, const dual_vector_iterator<FirstPointer, SecondPointer>& iter){
    return iter + inc;
}

template<typename FirstPointer, typename SecondPointer>
dual_vector_iterator<FirstPointer, SecondPointer> operator-(std::ptrdiff_t inc, const dual_vector_iterator<FirstPointer, SecondPointer>& iter){
    return iter - inc;
}

template<typename FirstPointer, typename SecondPointer>
auto iter_move(const dual_vector_iterator<FirstPointer, SecondPointer>& iter){
    using first_element = std::pointer_traits<FirstPointer>::element_type;
    using second_element = std::pointer_traits<SecondPointer>::element_type;
    using first_reference = std::conditional_t<std::is_const_v<first_element>, const first_element&, first_element&&>;
    using second_reference = std::conditional_t<std::is_const_v<second_element>, const second_element&, second_element&&>;

    auto lref = *iter;
    return dual_vector_reference<first_reference, second_reference>(std::move(lref.first), std::move(lref.second));
}

template<typename FirstType, typename SecondType, typename Allocator = std::allocator<std::pair<FirstType, SecondType>>>
struct dual_vector{
    using value_type = std::pair<FirstType, SecondType>;
    using reference = dual_vector_reference<FirstType&, SecondType&>;
    using const_reference =  dual_vector_reference<const FirstType&, const SecondType&>;
    using allocator_type = Allocator;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;

    

    private:
    using alloc = typename std::allocator_traits<allocator_type>::template rebind_alloc<std::byte>;
    using alloc_traits = std::allocator_traits<alloc>;
    using pointer = typename alloc_traits::pointer;

    using layout = layout_order<FirstType, SecondType>;
    using first = typename layout::first;
    using alloc_first = typename std::allocator_traits<allocator_type>::template rebind_alloc<first>;
    using alloc_traits_first = std::allocator_traits<alloc_first>;
    using pointer_first = typename alloc_traits_first::pointer;
    using const_pointer_first = typename alloc_traits_first::const_pointer;

    using second = typename layout::second;
    using alloc_second = typename std::allocator_traits<allocator_type>::template rebind_alloc<second>;
    using alloc_traits_second = std::allocator_traits<alloc_second>;
    using pointer_second = typename alloc_traits_second::pointer;
    using const_pointer_second = typename alloc_traits_second::const_pointer;

    using bind = bind_allocator<alloc_first>;

    public:
    using iterator = std::conditional_t<layout::reversed, dual_vector_iterator<pointer_second, pointer_first>,
                                                          dual_vector_iterator<pointer_first, pointer_second>>;
    using const_iterator = std::conditional_t<layout::reversed, dual_vector_iterator<const_pointer_second, const_pointer_first>,
                                                                dual_vector_iterator<const_pointer_first, const_pointer_second>>;
    private:

    static constexpr std::size_t num_arrays = 2;

    static constexpr std::size_t sum_of_sizes = sizeof(FirstType) + sizeof(SecondType);

    static constexpr auto adjust_alloc_size = [](std::size_t size){ return size * sum_of_sizes/sizeof(typename layout::first) + 1; };

    static constexpr auto allocate = [](alloc_first& allocator, std::size_t size)->pointer_first{
        std::size_t array_elements = dual_vector::adjust_alloc_size(size);
        auto array_ptr = alloc_traits_first::allocate(allocator, array_elements);
        // In the context of language rules, we really need an array of bytes, but the allocator for the first type
        // returns a pointer to an array of first. We need to use the allocator for the first type to get alignment
        // for the overall buffer correct.
        std::byte* buffer_ptr = new ((void*) std::to_address(array_ptr)) std::byte[array_elements * sizeof(FirstType)];
        //first* first_array = new(buffer_ptr) first[]
        return array_ptr;
    };

    static constexpr auto deallocate = [](alloc_first& allocator, pointer_first buffer, std::size_t size)->void{
        alloc_traits_first::deallocate(allocator, buffer, dual_vector::adjust_alloc_size(size));
    };

    template<typename Value, typename... Types>
    inline static constexpr bool noexcept_construct = detect_noexcept_construct<Value, allocator_type, Types...>();

    static pointer_second to_begin_second(pointer_first begin_first, size_type buffer_capacity){
        first* end_of_first = std::to_address(begin_first) + buffer_capacity;
        second (* array_pointer)[] = std::launder((second(*)[]) end_of_first);
        return (second*) *array_pointer;
    }
    public:

    constexpr dual_vector() = default;

    constexpr dual_vector(std::size_t size_in, const allocator_type& alloc = {}): 
        allocator{alloc},
        buffer{allocate(alloc, size_in)},
        buffer_capacity{size_in},
        array_size{size_in} {
            initalize(bind::bind_construct(allocator));
    }

    constexpr dual_vector(std::size_t size_in, std::convertible_to<value_type> auto&& pair_like, const allocator_type& alloc = {}): 
        allocator{alloc},
        buffer{allocate(allocator, size_in)},
        buffer_capacity{size_in},
        array_size{size_in} {
            const auto& [first, second] = pair_like;
            if constexpr (layout::reversed){
                initalize(bind::bind_construct(allocator, second), bind::bind_construct(allocator, first));
            }else{
                initalize(bind::bind_construct(allocator, first), bind::bind_construct(allocator, second));
            }
    }

    constexpr dual_vector(std::initializer_list<value_type> init, const allocator_type& alloc = {}):
        allocator{alloc},
        buffer{allocate(allocator, init.size())},
        buffer_capacity{init.size()},
        array_size{init.size()}{
            uninitialized_alloc_copy(init.begin(), init.end(), begin(), allocator);
    } 

    constexpr dual_vector(const dual_vector& other):
        allocator{alloc_traits_first::select_on_container_copy_construction(other.allocator)},
        buffer{allocate(allocator, other.array_size)},
        buffer_capacity{other.array_size},
        array_size{other.array_size} {}

    constexpr dual_vector(dual_vector&& other) noexcept:
        allocator{other.allocator},
        buffer{std::exchange(other.buffer, nullptr)},
        buffer_capacity{std::exchange(other.buffer_capacity, 0)},
        array_size{std::exchange(other.array_size, 0)} {}


    constexpr ~dual_vector(){
        if(buffer != nullptr){
            destroy_elements();
            deallocate(allocator, buffer, buffer_capacity);
        }
    }

    private:
    /*
    template<std::invocable<pointer> Functor>
        requires(std::invocable<Functor, pointer_first> 
                 and std::invocable<Functor, pointer_second>
                 and noexcept(initializer(std::declval<pointer_first>()) 
                              and initializer(std::declval<pointer_second>())))
    void initalize(Functor&& initalizer) noexcept {



        auto first_current = begin<0>();
        auto second_current = begin<1>();

        
        for (; first_current != end<0>(); ++first_current) {
            initalizer(first_current);
        }
        for (; second_current != end<1>(); ++second_current) {
            initalizer(second_current);
        }
    }
    */
    
    template<typename Functor>
        requires (std::invocable<Functor&, pointer_first> and std::invocable<Functor&, pointer_second>)
    void initalize(Functor&& initializer)   {
        initalize(initializer, initializer);
    }

    template<typename FirstFunctor, typename SecondFunctor>
        requires (std::invocable<FirstFunctor, pointer_first> and std::invocable<SecondFunctor, pointer_second>)
    void initalize(FirstFunctor&& first_initializer, SecondFunctor&& second_initializer)   {


        auto first_begin = begin_first();
        auto first_current = first_begin;
        auto first_end = end_first();
        //auto second_current = begin<1>();

        try {
            for (; first_current != first_end; ++first_current) {
                first_initializer(first_current);
            }
            auto second_begin = begin_second();
            auto second_current = second_begin;
            auto second_end = end_second();
            try{
                for (; second_current != second_end; ++second_current) {
                    second_initializer(second_current);
                }
            } catch(...){
                for (; second_current != second_begin; second_current -= 1) {
                    alloc_traits_first::destroy(allocator, second_current - 1);
                }
                throw;
            }
        } catch (...) {
            for (; first_current != first_begin; first_current -= 1) {
                alloc_traits_first::destroy(allocator, first_current - 1);
            }
            deallocate(allocator, buffer, buffer_capacity);
            buffer = nullptr;
            array_size = 0;
            throw;
        }
    }

    void destroy_elements(){
        alloc_destroy(begin_first(), end_first(), allocator);
        alloc_destroy(begin_second(), end_second(), allocator);
    }
    /*
    template<std::size_t index, Functor>
    void initalize_array(Functor&& initalizer) {
        static_assert(index < num_arrays);
        auto begin = begin_internal<index>();
        auto current = begin;
        auto end = end_internal<index>();
        try {
            for (; current != end<index>(); ++current) {
                initalizer(current);
            }
            if constexpr (index+1<num_arrays){
                initalize_array(std::forward<Functor>(initalizer));
            }
        } catch (...) {
            for (; current != begin<index>(); current -= 1) {
                alloc_traits_first::destroy(allocator, current - 1);
            }
        }
    }
    */
    pointer_first begin_first(){
        return buffer;
    }

    const_pointer_first begin_first() const {
        return buffer;
    }

    pointer_first end_first(){
        return buffer + array_size;
    }

    const_pointer_first end_first() const {
        return buffer + array_size;
    }

    pointer_second begin_second(){
        return to_begin_second(buffer, buffer_capacity);
    }

    const_pointer_second begin_second() const {
        return to_begin_second(buffer, buffer_capacity);
    }

    pointer_second end_second(){
        return begin_second() + array_size;
    }
    
    const_pointer_second end_second() const {
        return begin_second() + array_size;
    }

    static constexpr bool alloc_always_equal = alloc_traits::is_always_equal::value;
    static constexpr bool alloc_prop_copyassign = alloc_traits::propagate_on_container_copy_assignment::value && !alloc_always_equal;
    static constexpr bool alloc_prop_moveassign = alloc_traits::propagate_on_container_move_assignment::value && !alloc_always_equal;

    static constexpr bool nonprop_nothrow_element_move =
            !(alloc_always_equal || alloc_prop_moveassign) && std::is_nothrow_move_constructible_v<value_type>;

    static constexpr bool copy_over = std::is_nothrow_copy_constructible_v<first> && std::is_nothrow_copy_constructible_v<second>
                                    && std::is_nothrow_copy_assignable_v<first> && std::is_nothrow_copy_assignable_v<second>;
    template<typename Type>
    static constexpr bool element_no_throw_move = std::uses_allocator_v<Type, allocator_type> ? std::is_nothrow_constructible_v<Type, Type&&, const allocator_type&>
                                                                                              : std::is_nothrow_move_constructible_v<Type> 
                                                  && std::is_nothrow_move_assignable_v<Type>;

    static constexpr bool move_over = element_no_throw_move<first> && element_no_throw_move<second>;
                                                //&& std::is_nothrow_move_constructible_v<second> && std::is_nothrow_move_assignable_v<second>;
    template<bool propagate>
        requires propagate
    alloc_first alloc_assign_select(const dual_vector& other){
        return other.allocator;
    }
    template<bool propagate>
        requires (!propagate)
    alloc_first alloc_assign_select(const dual_vector& other){
        return allocator;
    }
    pointer_first new_copy_assign(alloc_first& new_alloc, const dual_vector& other){
        pointer_first new_buffer = allocate(new_alloc, other.array_size);
        pointer_first first_begin = new_buffer;
        pointer_second second_begin = (second *) (std::to_address(new_buffer) + other.array_size);
        try{
            std::tie(std::ignore, new_buffer) = uninitialized_alloc_copy(other.begin_first(), other.end_first(), new_buffer, new_alloc);
            uninitialized_alloc_copy(other.begin_second(), other.end_second(), second_begin, new_alloc);
        } catch(...){
            auto bound_destroy = bind::bind_destroy(new_alloc);
            while(new_buffer != first_begin){
                bound_destroy(--new_buffer);
            }
            deallocate(new_alloc, first_begin, other.array_size);
            throw;
        }
        return first_begin; 
    }

    void do_copy_assign(alloc_first& new_alloc, const dual_vector& other){
        pointer_first new_buffer = new_copy_assign(new_alloc, other);
        destroy_elements();
        deallocate(allocator, buffer, buffer_capacity);
        allocator = new_alloc;
        buffer = new_buffer;
        array_size = other.array_size;
        buffer_capacity = other.array_size;
    }

    template<typename Pointer>
    void copy_over_array(Pointer from_begin, Pointer from_end, Pointer to_begin, Pointer to_end) requires std::is_trivially_copyable_v<std::iter_value_t<Pointer>>{
        std::memcpy(to_begin, from_begin, from_end-from_begin);
    }

    template<typename Pointer>
    void copy_over_array(Pointer from_begin, Pointer from_end, Pointer to_begin, Pointer to_end) {
        std::size_t from_size = from_end - from_begin;
        std::size_t to_size = to_end - to_begin;
        if (from_size > to_size){
            std::copy(from_begin, from_begin + to_size, to_begin);
            std::uninitialized_copy(from_begin + to_size, from_end, to_end);

        } else {
            std::copy(from_begin, from_begin + from_size, to_begin);
            std::destroy(to_begin+from_size, to_end);
        }
    }

    void copy_over_buffer(const dual_vector& other){
        copy_over_array(other.begin_first(), other.end_first(), begin_first(), end_first());
        copy_over_array(other.begin_second(), other.end_second(), begin_second(), end_second());
    }

    template<typename Pointer>
    void move_over_array(Pointer from_begin, Pointer from_end, Pointer to_begin, Pointer to_end) requires std::is_trivially_copyable_v<std::iter_value_t<Pointer>>{
        std::memcpy(to_begin, from_begin, from_end-from_begin);
    }

    template<typename Pointer>
    void move_over_array(Pointer from_begin, Pointer from_end, Pointer to_begin, Pointer to_end) {
        std::size_t from_size = from_end - from_begin;
        std::size_t to_size = to_end - to_begin;
        if (from_size > to_size){
            std::move(from_begin, from_begin + to_size, to_begin);
            uninitialized_alloc_move(from_begin + to_size, from_end, to_end, allocator);

        } else {
            std::move(from_begin, from_begin + from_size, to_begin);
            std::destroy(to_begin+from_size, to_end);
        }
    }

    void move_over_buffer(dual_vector& other){
        move_over_array(other.begin_first(), other.end_first(), begin_first(), end_first());
        move_over_array(other.begin_second(), other.end_second(), begin_second(), end_second());
    }

    template<typename Type>
    static constexpr bool copy_relocate = !std::is_nothrow_move_constructible_v<Type> && std::is_copy_constructible_v<Type>;

    void relocate(size_type new_capacity){
        pointer_first new_buffer = allocate(allocator, new_capacity);
        pointer_first constructed_first = new_buffer;
        pointer_second constructed_second = to_begin_second(new_buffer, new_capacity);
        try{
            if constexpr (copy_relocate<first>){
                constructed_first = std::uninitialized_copy(begin_first(), end_first(), new_buffer);
            }
            if constexpr (copy_relocate<second>){
                constructed_second = std::uninitialized_copy(begin_second(), end_second(), to_begin_second(new_buffer, new_capacity));
            }
            if constexpr (!copy_relocate<first>){
                constructed_first = std::uninitialized_move(begin_first(), end_first(), new_buffer);
            }
            if constexpr (!copy_relocate<second>){
                constructed_second = std::uninitialized_move(begin_second(), end_second(), to_begin_second(new_buffer, new_capacity));
            }
        }
        catch(...){
            alloc_destroy(new_buffer, constructed_first, allocator);
            alloc_destroy(to_begin_second(new_buffer, new_capacity), constructed_second, allocator);
            deallocate(allocator, new_buffer, new_capacity);
            throw;
        }
        destroy_elements();
        deallocate(allocator, buffer, buffer_capacity);
        buffer = new_buffer;
        buffer_capacity = new_capacity;
    }
    
    template<typename ValueType, typename OtherType>
    static constexpr bool move_insert = std::is_rvalue_reference_v<OtherType&&> && noexcept_construct<ValueType, OtherType&&>;

    template<std::convertible_to<first> FirstArg, std::convertible_to<second> SecondArg>
    void insert_at(pointer_first first_location, FirstArg&& first_arg, pointer_second second_location, SecondArg&& second_arg){
        auto copy_construct = bind::bind_copy_construct(allocator);
        if constexpr (!move_insert<first, FirstArg>){
            copy_construct(first_location, first_arg);
        }
        if constexpr (!move_insert<second, SecondArg>){
            try{
                copy_construct(second_location, second_arg);
            } catch(...){
                if constexpr(!move_insert<first, FirstArg>){
                    alloc_traits_first::destroy(allocator, first_location);
                }
                throw;
            }
        }
        auto forward_construct = bind::bind_construct(allocator);
        if constexpr (move_insert<first, FirstArg>){
            forward_construct(first_location, std::forward<FirstArg>(first_arg));
        }
        if constexpr (move_insert<second, SecondArg>){
            forward_construct(second_location, std::forward<SecondArg>(second_arg));
        }
    }

    template<std::convertible_to<first> FirstArg, std::convertible_to<second> SecondArg>
    void insert_relocate(size_type new_capacity, size_type insert_index, FirstArg&& first_arg, SecondArg&& second_arg){
        pointer_first new_buffer = allocate(allocator, new_capacity);
        pointer_first constructed_first = new_buffer;
        pointer_second constructed_second = to_begin_second(new_buffer, new_capacity);
        bool first_inserted = false;
        bool second_inserted = false;
        try{
            if constexpr (copy_relocate<first>){
                constructed_first = std::uninitialized_copy(begin_first(), begin_first() + insert_index, new_buffer);

                constructed_first = std::uninitialized_copy(begin_first() + insert_index, end_first(), new_buffer + insert_index + 1);
            }
            if constexpr (copy_relocate<second>){
                constructed_second = std::uninitialized_copy(begin_second(), begin_second() + insert_index, to_begin_second(new_buffer, new_capacity));

                constructed_second = std::uninitialized_copy(begin_second() + insert_index, end_second(), to_begin_second(new_buffer, new_capacity) + insert_index + 1);
            }
            insert_at(new_buffer + insert_index, std::forward<FirstArg>(first_arg), to_begin_second(new_buffer, new_capacity) + insert_index, std::forward<SecondArg>(second_arg));
            if constexpr (!copy_relocate<first>){
                constructed_first = std::uninitialized_move(begin_first(), end_first(), new_buffer);
            }
            if constexpr (!copy_relocate<second>){
                constructed_second = std::uninitialized_move(begin_second(), end_second(), to_begin_second(new_buffer, new_capacity));
            }
        }
        catch(...){
            if (constructed_first-new_buffer > insert_index){
                alloc_destroy(new_buffer + insert_index + 1, constructed_first, allocator);
                constructed_first = new_buffer + insert_index;
            }
            alloc_destroy(new_buffer, constructed_first, allocator);
            auto new_begin_second = to_begin_second(new_buffer, new_capacity);
            if (constructed_second-new_begin_second > insert_index){
                alloc_destroy(new_begin_second + insert_index + 1, constructed_second, allocator);
                constructed_second = new_begin_second + insert_index;
            }
            alloc_destroy(new_begin_second, constructed_second, allocator);
            
            //alloc_destroy(new_buffer, constructed_first, allocator);
            //alloc_destroy(to_second_begin(new_buffer, new_capacity), constructed_second, allocator);
            deallocate(allocator, new_buffer, new_capacity);
            throw;
        }
        destroy_elements();
        deallocate(allocator, buffer, buffer_capacity);
        buffer = new_buffer;
        buffer_capacity = new_capacity;
        ++array_size;
    }

    public:
    dual_vector& operator=(const dual_vector& other) {
        if (this != &other){
            alloc_first new_alloc = alloc_assign_select<alloc_prop_copyassign>(other);
            do_copy_assign(new_alloc, other);
        }
        return *this;
    }

    dual_vector& operator=(const dual_vector& other) requires copy_over {
        if(this != &other){
            if (buffer_capacity < other.array_size){
                alloc_first new_alloc = alloc_assign_select<alloc_prop_copyassign>(other);
                do_copy_assign(new_alloc, other);
            } else {
                alloc_first new_alloc = alloc_assign_select<alloc_prop_copyassign>(other);
                if (new_alloc == allocator){
                    copy_over_buffer(other);
                    array_size = other.array_size;
                } else {
                    do_copy_assign(new_alloc, other);
                }
            }
        }
        return *this;
    }

    dual_vector& operator=(dual_vector&& other) noexcept requires (alloc_always_equal || alloc_prop_moveassign) {
        alloc_first old_allocator = allocator;
        pointer_first old_buffer = three_way_exchange(buffer, other.buffer, nullptr); // old_buffer == nullptr on self-assign
        size_type old_size = three_way_exchange(array_size, other.array_size, 0);
        size_type old_capacity = three_way_exchange(buffer_capacity, other.buffer_capacity, 0);
        
        deallocate(old_allocator, old_buffer, buffer_capacity);
        allocator = alloc_assign_select<alloc_prop_moveassign>(other);
        return *this;
    }
    
    dual_vector& operator=(dual_vector&& other) requires (move_over) {
        if (allocator == other.allocator){
            pointer_first old_buffer = three_way_exchange(buffer, other.buffer, nullptr); // old_buffer == nullptr on self-assign
            size_type old_size = three_way_exchange(array_size, other.array_size, 0);
            size_type old_capacity = three_way_exchange(buffer_capacity, other.buffer_capacity, 0);
            alloc_destroy(old_buffer, old_buffer+old_size, allocator);
            alloc_destroy(to_begin_second(old_buffer, old_capacity), to_begin_second(old_buffer, old_capacity)+old_size, allocator);
            deallocate(allocator, old_buffer, buffer_capacity);
        } else {
            if (buffer_capacity < other.array_size){
                pointer_first new_buffer = allocate(allocator, other.array_size);
                uninitialized_alloc_move(begin_first(), end_first(), new_buffer, allocator);
                uninitialized_alloc_move(begin_second(), end_second(), (second *)(std::to_address(new_buffer)+other.array_size), allocator);
                destroy_elements();
                deallocate(allocator, buffer, buffer_capacity);
                buffer = new_buffer;
                array_size = other.array_size;
                buffer_capacity = other.array_size;
            }

            move_over_buffer(other);
        }
        return *this;
    }

    dual_vector& operator=(dual_vector&& other){
        if (allocator == other.allocator){
            pointer_first old_buffer = three_way_exchange(buffer, other.buffer, nullptr); // old_buffer == nullptr on self-assign
            size_type old_size = three_way_exchange(array_size, other.array_size, 0);
            size_type old_capacity = three_way_exchange(buffer_capacity, other.buffer_capacity, 0);
            
            deallocate(allocator, old_buffer, buffer_capacity);
        } else {
            
            pointer_first new_buffer = allocate(allocator, other.array_size);
            uninitialized_alloc_move(begin_first(), end_first(), new_buffer, allocator);
            uninitialized_alloc_move(begin_second(), end_second(), (second *)(std::to_address(new_buffer)+other.array_size), allocator);
            deallocate(allocator, buffer, buffer_capacity);
            buffer = new_buffer;
            array_size = other.array_size;
            buffer_capacity = other.array_size;
            

            //move_over_buffer(other);
        }
        return *this;
    }

    allocator_type get_allocator(){
        return allocator;
    }

    reference operator[](size_type index){
        return begin()[index];
    }

    const_reference operator[](size_type index) const {
        return begin()[index];
    }

    reference front(){
        return *begin();
    }

    const_reference front() const {
        return *begin();
    }

    reference back(){
        return *--end();
    }

    const_reference back() const {
        return *--end();
    }

    iterator begin() noexcept{
        if constexpr (layout::reversed){
            return {begin_second(), begin_first()};
        } else {
            return {begin_first(), begin_second()};
        }
    }

    const_iterator begin() const noexcept {
        if constexpr (layout::reversed){
            return {begin_second(), begin_first()};
        } else {
            return {begin_first(), begin_second()};
        }
    }

    iterator end() noexcept{
        if constexpr (layout::reversed){
            return {end_second(), end_first()};
        } else {
            return {end_first(), end_second()};
        }
    }

    const_iterator end() const noexcept {
        if constexpr (layout::reversed){
            return {end_second(), end_first()};
        } else {
            return {end_first(), end_second()};
        }
    }

    bool empty() const noexcept{
        return array_size == 0;
    }

    size_type size() const noexcept {
        return array_size;
    }

    difference_type max_size() const noexcept {
        return std::numeric_limits<difference_type>::max();
    }

    void reserve(std::size_t new_capacity){
        if(new_capacity > buffer_capacity){
            relocate(new_capacity);
        }
    }

    size_type capacity() noexcept{
        return buffer_capacity;
    }

    void shrink_to_fit(){
        if(buffer_capacity > array_size){
            relocate(array_size);
        }
    }

    void clear() noexcept{
        destroy_elements();
        array_size = 0;
    }
    private:

                      // args denote the gap as a half-open range
    void shift_elements(size_type gap_begin, size_type gap_end) requires (std::is_move_constructible_v<first> && std::is_move_constructible_v<second>) {
        size_type shift_amount = gap_end - gap_begin;
        auto move_construct = bind::bind_move_construct(allocator);
        auto destroy = bind::bind_destroy(allocator);

        auto shift_imp = [&](auto shift_begin, auto shift_end){
            auto shift_destination = shift_end - 1 + shift_amount;

            while(shift_end != shift_begin){
                move_construct(shift_destination, --shift_end);
                destroy(shift_end);
                --shift_destination;
            }
        };
        shift_imp(begin_first() + gap_begin, end_first());
        shift_imp(begin_second() + gap_begin, end_second());
    }

     void shift_elements(size_type gap_begin, size_type gap_end) {
        size_type shift_amount = gap_end - gap_begin;
        auto shift_construct = bind::bind_copy_construct(allocator);
        auto destroy = bind::bind_destroy(allocator);

        auto shift_imp = [&](auto shift_begin, auto shift_end){
            auto original_end = shift_end;
            auto shift_destination = shift_end - 1 + shift_amount;
            try{
                while(shift_end != shift_begin){
                    copy_construct(shift_destination, --shift_end);
                    destroy(shift_end);
                    --shift_destination;
                }
            }catch(...){
                alloc_destroy(++shift_destination, original_end+shift_amount, allocator);
                uninitialized_alloc_construct(++shift_end, std::min(shift_destination, original_end), allocator);

                throw;
            }
        };
        shift_imp(begin_first() + gap_begin, end_first());
        try{
            shift_imp(begin_second() + gap_begin, end_second());
        } catch(...){
            uninitialized_alloc_construct(begin_first()+gap_begin, begin_first()+gap_end);
            alloc_destroy(end_first(), end_first()+(gap_end-gap_begin));
            throw;
        }
    }

    void unshift_elements(size_type gap_begin, size_type gap_end) requires (std::is_nothrow_move_constructible_v<first> && std::is_nothrow_move_constructible_v<second>) {
        size_type shift_amount = gap_end - gap_begin;
        auto move_construct = bind::bind_move_construct(allocator);
        auto destroy = bind::bind_destroy(allocator);

        auto unshift_imp = [&](auto shift_begin, auto shift_end){
            auto shift_destination = shift_begin - shift_amount;

            while(shift_end != shift_begin){
                move_construct(shift_destination, shift_begin);
                destroy(shift_begin);
                ++shift_begin;
                ++shift_destination;
            }
        };
        unshift_imp(begin_first() + gap_end, end_first()+shift_amount);
        unshift_imp(begin_second() + gap_end, end_second()+shift_amount);
    }

    template<typename FirstArg, typename SecondArg>
    void insert_imp(size_type index, FirstArg&& first_arg, SecondArg&& second_arg){
        if(array_size < buffer_capacity){
            shift_elements(index, index + 1);
            try{
                insert_at(begin_first() + index, std::forward<FirstArg>(first_arg), begin_second() + index, std::forward<SecondArg>(second_arg));
            } catch(...){
                if constexpr (std::is_nothrow_move_constructible_v<first> && std::is_nothrow_move_constructible_v<second>){
                    unshift_elements(index, index + 1);
                } else {
                    alloc_traits_first::construct(allocator, begin_first()+index);
                    alloc_traits_first::construct(allocator, begin_second()+index);
                    alloc_traits_first::destroy(allocator, end_first());
                    alloc_traits_first::destroy(allocator, end_second());
                }
                throw;
            }
            ++array_size;
        } else {
            insert_relocate(buffer_capacity + buffer_capacity/2 + 2, index, std::forward<FirstArg>(first_arg), std::forward<SecondArg>(second_arg));
        }
    }

    public:
    template<alloc_constructable<FirstType, allocator_type> FirstArg, alloc_constructable<FirstType, allocator_type> SecondArg>
        requires (!layout::reversed)
    iterator insert(const_iterator position, FirstArg&& first_arg, SecondArg&& second_arg){
        auto index = position.first_ptr - buffer;
        insert_imp(index, std::forward<FirstArg>(first_arg), std::forward<SecondArg>(second_arg));
        return begin() + index;
    }
    template<alloc_constructable<FirstType, allocator_type> FirstArg, alloc_constructable<FirstType, allocator_type> SecondArg>
    iterator insert(const_iterator position, FirstArg&& first_arg, SecondArg&& second_arg){
        auto index = position.second_ptr - buffer;
        insert_imp(index, std::forward<SecondArg>(second_arg), std::forward<FirstArg>(first_arg));
        return begin() + index;
    }

    iterator erase(const_iterator pos){
        auto index = pos - begin();
        std::move(begin_first()+index+1, end_first(), begin_first()+index);
        std::move(begin_second()+index+1, end_second(), begin_second()+index);
        alloc_traits_first::destroy(allocator, --end_first());
        alloc_traits_first::destroy(allocator, --end_second());
        --array_size;
        return begin()+index;
    }

    iterator erase(const_iterator first, const_iterator end){
        if (first == end){
            return end;
        }

        auto begin_index = first-begin();
        auto end_index = end-begin();
        pointer_first destroy_first = std::rotate(begin_first()+begin_index, begin_first()+end_index, end_first());
        pointer_second destroy_second = std::rotate(begin_second()+begin_index, begin_second()+end_index, end_second());

        alloc_destroy(destroy_first, end_first(), allocator);
        alloc_destroy(destroy_second, end_second(), allocator);
        array_size = destroy_first - begin_first();
        return begin() + begin_index;
    }
    private:
    template<typename FirstArg, typename SecondArg>
        requires std::convertible_to<std::remove_cvref_t<FirstArg>, first> && std::convertible_to<std::remove_cvref_t<SecondArg>, second>
    void push_back_imp(FirstArg&& first_arg, SecondArg&& second_arg){
        auto construct = bind::bind_construct(allocator);

        if (array_size == buffer_capacity){
            relocate(buffer_capacity + buffer_capacity/2 + 2);
        }

        if constexpr (!element_no_throw_move<SecondArg>){
            construct(end_second(), second_arg);
            try{
                construct(end_first(), move_if_safe<alloc_first, FirstArg>(first_arg));
            } catch(...){
                auto destroy = bind::bind_destroy(allocator);
                destroy(end_second());
                throw;
            }
        } else {
            construct(end_first(), move_if_safe<alloc_first, FirstArg>(first_arg));
            try{
                construct(end_second(), move_if_safe<alloc_first, SecondArg>(second_arg));
            } catch (...){
                auto destroy = bind::bind_destroy(allocator);
                destroy(end_first());
                throw;
            }
        }
        ++array_size;
    }

    void check_relocate(size_type target_size){
        if (buffer_capacity < target_size){
            relocate(std::max(buffer_capacity + buffer_capacity/2 + 2, target_size));
        }
    }
    public:
    template<typename FirstArg, typename SecondArg>
        requires std::convertible_to<std::remove_cvref_t<FirstArg>, FirstType> && std::convertible_to<std::remove_cvref_t<SecondArg>, SecondType>
    void push_back(FirstArg&& first_arg, SecondArg&& second_arg){
        if constexpr(layout::reversed){
            push_back_imp(std::forward<SecondArg>(second_arg), std::forward<FirstArg>(first_arg));
        } else {
            push_back_imp(std::forward<FirstArg>(first_arg), std::forward<SecondArg>(second_arg));
        }
    }

    template<std::convertible_to<value_type> PairLike> 
    void push_back(PairLike&& pair_like){
        auto&& [first_arg, second_arg] = std::forward<PairLike>(pair_like);
        push_back(std::forward<decltype(first_arg)>(first_arg), std::forward<decltype(second_arg)>(second_arg));
    }
    //reference emplace_back(...)
    void resize(size_type new_size){
        if(new_size< array_size){
            alloc_destroy(begin_first() + new_size, end_first(), allocator);
            alloc_destroy(begin_second() + new_size, end_second(), allocator);
            array_size = new_size;
            return;
        }
        if(new_size>array_size){
            check_relocate(new_size);
            uninitialized_alloc_construct(end_first(), begin_first()+new_size, allocator);
            uninitialized_alloc_construct(end_second(), begin_second()+new_size, allocator);
            array_size = new_size;
            return;
        }
    }

    //void resize(size_type new_size, ValueType value)
    void swap(dual_vector& other){
        using std::ranges::swap;
        if constexpr (alloc_traits::propagate_on_container_swap::value){
            swap(allocator, other.allocator);
        }
        swap(buffer, other.buffer);
        swap(buffer_capacity, other.buffer_capacity);
        swap(array_size, other.array_size);

    }

    private:
    [[no_unique_address]] alloc_first allocator;
    pointer_first buffer;
    size_type buffer_capacity;
    size_type array_size;
};

}

template<typename LHSFirstRef, typename LHSSecondRef, typename RHSFirst, typename RHSSecond, template<typename> typename FirstQual, template<typename> typename SecondQual>
struct std::basic_common_reference<ann::dual_vector_reference<LHSFirstRef, LHSSecondRef>, std::pair<RHSFirst, RHSSecond>, FirstQual, SecondQual>{
    using qualified_pair = SecondQual<std::pair<RHSFirst, RHSSecond>>;
    //static constexpr bool pair_is_const = std::is_const_v<std::remove_reference_t<SecondQual<std::pair<RHSFirst, RHSSecond>>>;

    //template<typename Type>
    //using prop_const = std::conditional_t<pair_is_const, const 

    using first_rhs_ref  = std::conditional_t<std::is_reference_v<RHSFirst>, RHSFirst, SecondQual<RHSFirst>>;
    using second_rhs_ref = std::conditional_t<std::is_reference_v<RHSSecond>, RHSSecond, SecondQual<RHSSecond>>;
    using type = ann::dual_vector_reference<
                    std::common_reference_t<LHSFirstRef, first_rhs_ref>,
                    std::common_reference_t<LHSSecondRef, second_rhs_ref>
                >;
};

template<typename LHSFirst, typename LHSSecond, typename RHSFirstRef, typename RHSSecondRef, template<typename> typename FirstQual, template<typename> typename SecondQual>
struct std::basic_common_reference<std::pair<LHSFirst, LHSSecond>, ann::dual_vector_reference<RHSFirstRef, RHSSecondRef>, FirstQual, SecondQual> :
    std::basic_common_reference<ann::dual_vector_reference<RHSFirstRef, RHSSecondRef>, std::pair<LHSFirst, LHSSecond>, SecondQual, FirstQual> {};

template<typename LHSFirstRef, typename LHSSecondRef, typename RHSFirstRef, typename RHSSecondRef, template<typename> typename FirstQual, template<typename> typename SecondQual>
struct std::basic_common_reference<ann::dual_vector_reference<LHSFirstRef, LHSSecondRef>, ann::dual_vector_reference<RHSFirstRef, RHSSecondRef>, FirstQual, SecondQual>{
    using type = ann::dual_vector_reference<
                    std::common_reference_t<LHSFirstRef, RHSSecondRef>&&,
                    std::common_reference_t<LHSSecondRef, RHSSecondRef>&&
                >;
};




#endif