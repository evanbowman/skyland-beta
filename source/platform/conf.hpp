////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "allocator.hpp"
#include "platform.hpp"
#include "string.hpp"
#include <optional>
#include <variant>


// A small INI reader.
//
class Conf
{
public:
    Conf()
    {
    }

    using Integer = int;

    using StrBuffer = StringAdapter<2000, Buffer<char, 2000 + 1, false>>;
    using String = DynamicMemory<StrBuffer>;
    using Value = std::variant<std::monostate, Integer, String>;

    Value get(const char* file_data, const char* section, const char* key);
    Value get(const char* section, const char* key);

    template <typename T> T expect(const char* section, const char* key)
    {
        auto v = get(section, key);
        if (auto val = std::get_if<T>(&v)) {
            return std::move(*val);
        } else {
            Platform::fatal(
                format("in config ini: missing % from [%]", key, section)
                    .c_str());
        }
    }

private:

    static constexpr int index_capacity = 33;

    struct IndexEntry {
        u16 section_off;
        u16 key_off;
        u16 value_off;
        u8 section_len;
        u8 key_len;
    };

    struct Index {
        const char* source = nullptr;
        int count = 0;
        bool overflow = false; // file exceeded a representable limit; use a scan
        IndexEntry entries[index_capacity];
    };

    const Index& ensure_index(const char* file_data);

    std::optional<DynamicMemory<Index>> index_;
    const char* cached_file_data_ = nullptr;
};
