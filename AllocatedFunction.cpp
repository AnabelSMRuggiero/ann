#include <functional>
#include <iostream>
#include <memory>
#include <utility>
#include <type_traits>

struct AddOne{

    int valueToAdd;

    int operator()(int in){
        return in+1;
    }

};

template<typename Signature>
struct AllocatedFunction; //Never define this!!!!

/*
    - Basic metafunction/type manipulation
    - ConcreteFunctor move constructor
    - Template Args
    - Variadic Template
    - Allocator
    - PtMF
*/

template<>
struct AllocatedFunction<int(int)>{

    template<typename Functor>
    AllocatedFunction(Functor&& functor) : 
        func{std::make_unique<ConcreteFunctor<Functor>>(std::forward<Functor>(functor))} {}

    AllocatedFunction(const AllocatedFunction& other) : func{other.func->clone()} {}

    AllocatedFunction(AllocatedFunction&& other) = default;

    ~AllocatedFunction() = default;

    AllocatedFunction& operator=(const AllocatedFunction& other){
        func = other.func->clone();
        return *this;
    }

    AllocatedFunction& operator=(AllocatedFunction&&) = default;

    int operator()(int num){
        return func->Invoke(num);
    }
    

    private:

    struct ErasedFunctor{
        virtual int Invoke(int num) = 0;
        virtual std::unique_ptr<ErasedFunctor> clone() = 0;
        virtual ~ErasedFunctor() = 0;
        
    };


    template<typename Functor>
    struct ConcreteFunctor final : ErasedFunctor {

        Functor underlyingFunctor;
        
        ConcreteFunctor(const Functor& functor) : 
            underlyingFunctor{functor} {}


        int Invoke(int num) final{
            return underlyingFunctor(num);
        }

        virtual std::unique_ptr<ErasedFunctor> clone(){
            return std::make_unique<ConcreteFunctor>(this->underlyingFunctor);
        }

        ~ConcreteFunctor() = default;

    };

    std::unique_ptr<ErasedFunctor> func;

};

AllocatedFunction<int(int)>::ErasedFunctor::~ErasedFunctor() = default;

template<>
struct AllocatedFunction<int(int, int)>{

    AllocatedFunction(const AllocatedFunction& other) : func{other.func->clone()} {}

    AllocatedFunction(AllocatedFunction&& other) = default;

    ~AllocatedFunction() = default;

    AllocatedFunction& operator=(const AllocatedFunction& other) {
        func = other.func->clone();
        return *this;
    }

    AllocatedFunction& operator=(AllocatedFunction&&) = default;

    private:

    struct ErasedFunctor{
        virtual int Invoke(int num1, int num2) = 0;
        virtual std::unique_ptr<ErasedFunctor> clone() = 0;
        virtual ~ErasedFunctor() = 0;
    };

    template<typename Functor>
    struct ConcreteFunctor final : ErasedFunctor {

        Functor underlyingFunctor;

        int Invoke(int num1, int num2) final {
            return underlyingFunctor(num1, num2);
        }

        virtual std::unique_ptr<ErasedFunctor> clone() {
            return std::make_unique<ConcreteFunctor>(this->underlyingFunctor);
        }

    };

    std::unique_ptr<ErasedFunctor> func;
};



int main(){

    int valueToAdd = 1;

    //AllocatedFunction<int(int)>

    auto addOne = [=](int in)->int{
        return in+valueToAdd;    
    };

    AddOne oldSchool{1};

    //AllocatedFunction<int(int)> thing = newThing;

    std::function<int(int)> erasedLamdba{addOne};

    std::function<int(int)> erasedFunctor{oldSchool};

    AllocatedFunction<int(int)> prototype{addOne}; //Functor = lambda&, copied onto heap

    AllocatedFunction<int(int)> protoForward{[=](int in)->int{
        return in+valueToAdd;    
    }}; //Functor = literalLambda&&, moved onto heap


    std::cout << "Lambda: " << addOne(1) << std::endl;

    std::cout << "Old style: " << oldSchool(1) << std::endl;

    std::cout << "typeof(addOne) == typeof(oldSchool): " << (typeid(addOne) == typeid(oldSchool)) << std::endl;

    std::cout << "Erased Lambda: " << erasedLamdba(1) << std::endl;

    std::cout << "Erased Functor: " << erasedFunctor(1) << std::endl;

    std::cout << "typeof(erasedLamdba) == typeof(erasedFunctor): " << (typeid(erasedLamdba) == typeid(erasedFunctor)) << std::endl;

    std::cout << "Erased Lambda: " << prototype(1) << std::endl;

    std::cout << "Erased Lambda Literal: " << erasedFunctor(1) << std::endl;

    std::cout << "typeid(prototype) == typeid(protoForward): " << (typeid(prototype) == typeid(protoForward)) << std::endl;


    return 0;
}