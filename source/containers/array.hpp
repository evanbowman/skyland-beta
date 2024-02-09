#pragma once


template <typename T, unsigned int count> class Array
{
public:
    constexpr auto capacity() const
    {
        return count;
    }


    constexpr auto size() const
    {
        return count;
    }


    constexpr T& operator[](unsigned int index)
    {
        return data_[index];
    }


    constexpr const T& operator[](unsigned int index) const
    {
        return data_[index];
    }


    constexpr T* data()
    {
        return data_;
    }


    constexpr const T* data() const
    {
        return data_;
    }


    using iterator = T*;

    constexpr iterator begin()
    {
        return data_;
    }


    constexpr const T* begin() const
    {
        return data_;
    }


    constexpr iterator end()
    {
        return data_ + count;
    }


    constexpr const T* end() const
    {
        return data_ + count;
    }



private:
    T data_[count];
};
