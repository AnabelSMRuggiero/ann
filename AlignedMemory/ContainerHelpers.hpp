#ifndef ANN_CONTAINERHELPERS_HPP
#define ANN_CONTAINERHELPERS_HPP

#include <iterator>
#include <memory>
#include <ranges>
#include <type_traits>
#include <utility>

#include "../TemplateManipulation.hpp"

namespace ann{

template<typename ObjToConstruct, typename Alloc, typename... Types>
concept constructible_by_alloc = std::is_constructible_v<ObjToConstruct, Types...> || std::is_constructible_v<ObjToConstruct, Types..., Alloc> || std::is_constructible_v<std::allocator_arg_t, Alloc, ObjToConstruct, Types...>;

template<typename ObjToConstruct, typename Alloc, typename... Types>
    requires constructible_by_alloc<ObjToConstruct, Alloc, Types...>
consteval bool detect_noexcept_construct() {
    using construct_tuple =
        decltype(std::uses_allocator_construction_args<ObjToConstruct>(std::declval<Alloc>(), std::declval<Types>()...));
    using tuple_types = extract_pack_t<construct_tuple>;
    using front_bound_trait = front_bind_t<std::is_nothrow_constructible, ObjToConstruct>;
    using applied_trait = typename decltype(apply_pack(front_bound_trait{}, tuple_types{}))::type;
    return applied_trait::value;
}

template<typename ObjToConstruct, typename Alloc, typename... Types>
consteval bool detect_noexcept_construct() {
    return false;
}


template<typename Range>
using range_forward_reference = std::conditional_t<!std::is_lvalue_reference_v<Range> && !std::ranges::enable_borrowed_range<Range>,
                                                 std::ranges::range_rvalue_reference_t<Range>,
                                                 std::ranges::range_reference_t<Range>>;

template<typename ForwardLike, typename Type>
using forward_like_t = std::conditional_t<std::is_lvalue_reference_v<ForwardLike>, 
                                        std::remove_reference_t<Type>&,
                                        std::remove_reference_t<Type>&&>;

template<typename ForwardLikeRange, typename Iter>
decltype(auto) iter_forward_like(Iter&& iter){
    if constexpr(!std::is_lvalue_reference_v<ForwardLikeRange> && !std::ranges::enable_borrowed_range<ForwardLikeRange>){
        return std::ranges::iter_move(std::forward<Iter>(iter));
    }else{
        return *iter;
    }
}

template<typename Type, std::convertible_to<Type> Value>
decltype(auto) three_way_exchange(Type& first, Type& second, Value&& assign_to_second){
    return std::exchange(first, std::exchange(second, std::forward<Value>(assign_to_second)));
}

}

#endif