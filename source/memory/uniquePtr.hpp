////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include <type_traits>
#include <utility>


// #include <memory> gets more bloated with each passing standard. It's up to
// 55000 lines of code at time of writing, which is nearly half as much code as
// in the skyland repo. Unsurprisingly, including the memory header in common
// headers adds several seconds to the whole project built time. Unacceptable.



template <typename ValueType, typename DeleterType>
class UniquePtr
{
private:

    class Pointer
    {
        template<typename Src>
        static typename Src::pointer ptr_type(typename Src::pointer*);

        template<typename Src>
        static ValueType* ptr_type(...);

        typedef typename std::remove_reference<DeleterType>::type _Del;

    public:
        typedef decltype(ptr_type<_Del>(0)) type;
    };


    typename Pointer::type ptr_;
    DeleterType del_;


public:


    using pointer = typename Pointer::type;
    using element = ValueType;
    using deleter = DeleterType;


    constexpr UniquePtr() : ptr_(), del_()
    {
        static_assert(not std::is_pointer<deleter>::value);
    }


    explicit UniquePtr(pointer p) : ptr_(p), del_(deleter())
    {
        static_assert(not std::is_pointer<deleter>::value, "null deleter");
    }


    UniquePtr(pointer p,
              typename std::conditional<std::is_reference<deleter>::value,
              deleter, const deleter&>::type d)
        : ptr_(p), del_(d)
    {
    }


    UniquePtr(pointer p, typename std::remove_reference<deleter>::type&& d)
        : ptr_(std::move(p)), del_(std::move(d))
    {
        static_assert(not std::is_reference<deleter>::value,
                      "ya passed the deleter wrong you dolt.");
    }


    constexpr UniquePtr(std::nullptr_t)
        : ptr_(), del_()
    {
        static_assert(not std::is_pointer<deleter>::value,
                      "non-ptr value type cannot be init'd with nullptr");
    }


    UniquePtr(UniquePtr&& other)
        : ptr_(other.release()), del_(std::forward<deleter>(other.get_deleter()))
    {
    }


    UniquePtr(const UniquePtr&) = delete;


    UniquePtr& operator=(const UniquePtr&) = delete;


    template<typename Src, typename SrcDel, typename = typename
             std::enable_if
             <std::is_convertible<typename UniquePtr<Src, SrcDel>::pointer,
                                  pointer>::value
              and not std::is_array<Src>::value
              and ((std::is_reference<DeleterType>::value
                    and std::is_same<SrcDel, DeleterType>::value)
                   or (not std::is_reference<DeleterType>::value
                       and std::is_convertible<SrcDel, DeleterType>::value))>
             ::type>
    UniquePtr(UniquePtr<Src, SrcDel>&& other)
    : ptr_(other.release()), del_(other.get_deleter())
    {
    }


    UniquePtr& operator=(UniquePtr&& other)
    {
        reset(other.release());
        get_deleter() = std::forward<deleter>(other.get_deleter());
        return *this;
    }


    template<typename Src, typename SrcDel, typename = typename
             std::enable_if
             <std::is_convertible<typename UniquePtr<Src, SrcDel>::pointer,
                                  pointer>::value
              and not std::is_array<Src>::value>::type>
    UniquePtr& operator=(UniquePtr<Src, SrcDel>&& other)
    {
        reset(other.release());
        get_deleter() = std::forward<SrcDel>(other.get_deleter());
        return *this;
    }


    UniquePtr& operator=(std::nullptr_t)
    {
        reset();
        return *this;
    }


    ~UniquePtr()
    {
        reset();
    }


    pointer operator->() const
    {
        return get();
    }


    pointer get() const
    {
        return ptr_;
    }


    typename std::add_lvalue_reference<element>::type operator*() const
    {
        return *get();
    }


    const deleter& get_deleter() const
    {
        return del_;
    }


    deleter& get_deleter()
    {
        return del_;
    }


    explicit operator bool() const
    {
        return get() == pointer() ? false : true;
    }


    pointer release()
    {
        pointer ret = get();
        ptr_ = pointer();
        return ret;
    }


    void reset(pointer ptr = pointer())
    {
        std::swap(ptr_, ptr);

        if (ptr not_eq pointer()) {
            get_deleter()(ptr);
        }
    }


    void swap(UniquePtr& other)
    {
        std::swap(ptr_, other.ptr_);
        std::swap(del_, other.del_);
    }
};



template<typename ValueType,
         typename DeleterType,
         typename Src,
         typename SrcDel>
bool operator==(const UniquePtr<ValueType, DeleterType>& lhs,
                const UniquePtr<Src, SrcDel>& rhs)
{
    return lhs.get() == rhs.get();
}



template<typename ValueType, typename DeleterType>
bool operator==(const UniquePtr<ValueType, DeleterType>& ptr, std::nullptr_t)
{
    return ptr.get() == nullptr;
}



template<typename ValueType, typename DeleterType>
bool operator==(std::nullptr_t, const UniquePtr<ValueType, DeleterType>& ptr)
{
    return ptr.get() == nullptr;
}



template<typename ValueType,
         typename DeleterType,
         typename Src,
         typename SrcDel>
bool operator not_eq(const UniquePtr<ValueType, DeleterType>& lhs,
                     const UniquePtr<Src, SrcDel>& rhs)
{
    return not (lhs.get() == rhs.get());
}



template<typename ValueType, typename DeleterType>
bool operator not_eq(const UniquePtr<ValueType, DeleterType>& ptr, std::nullptr_t)
{
    return ptr.get() not_eq nullptr;
}



template<typename ValueType, typename DeleterType>
bool operator not_eq(std::nullptr_t, const UniquePtr<ValueType, DeleterType>& ptr)
{
    return ptr.get() not_eq nullptr;
}
