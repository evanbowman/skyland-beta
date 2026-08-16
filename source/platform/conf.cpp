////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "conf.hpp"
#include "platform/platform.hpp"


namespace {


bool is_space(char c)
{
    return c == ' ' or c == '\t' or c == '\r';
}


bool is_digit(char c)
{
    return c >= '0' and c <= '9';
}


bool bytes_equal(const char* a, const char* b, int n)
{
    for (int i = 0; i < n; ++i) {
        if (a[i] not_eq b[i]) {
            return false;
        }
    }
    return true;
}


const char* value_end(const char* v)
{
    bool in_quotes = false;
    while (*v) {
        const char c = *v;
        if (c == '#') {
            break;
        }
        if (c == '"') {
            in_quotes = not in_quotes;
            ++v;
            continue;
        }
        if (not in_quotes and (c == '\n' or c == '\r' or c == '\t')) {
            break;
        }
        ++v;
    }
    return v;
}


template <typename Visitor>
void enumerate(const char* p, Visitor&& visit)
{
    const char* section = "";
    int section_len = 0;

    while (*p) {
        while (is_space(*p)) {
            ++p;
        }
        if (*p == '\n') {
            ++p;
            continue;
        }
        if (*p == '\0') {
            break;
        }

        if (*p == '#') { // comment line
            while (*p and *p not_eq '\n') {
                ++p;
            }
            continue;
        }

        if (*p == '[') { // section header
            ++p;
            const char* s = p;
            while (*p and *p not_eq ']' and *p not_eq '\n') {
                ++p;
            }
            section = s;
            section_len = (int)(p - s);
            while (*p and *p not_eq '\n') { // ignore anything after ']'
                ++p;
            }
            continue;
        }

        // key = value
        const char* k = p;
        while (*p and *p not_eq '=' and *p not_eq '\n' and not is_space(*p)) {
            ++p;
        }
        const int key_len = (int)(p - k);

        while (is_space(*p)) {
            ++p;
        }
        if (*p not_eq '=') { // malformed line; skip it
            while (*p and *p not_eq '\n') {
                ++p;
            }
            continue;
        }
        ++p; // consume '='
        while (is_space(*p)) {
            ++p;
        }

        const char* v = p;
        p = value_end(v);

        if (not visit(section, section_len, k, key_len, v)) {
            return;
        }

        while (*p and *p not_eq '\n') { // advance past any trailing comment
            ++p;
        }
    }
}


Conf::Value make_value(const char* begin)
{
    const char* const end = value_end(begin);

    bool in_quotes = false;
    bool any = false;
    bool numeric = true;
    Conf::Integer acc = 0;

    for (const char* q = begin; q < end; ++q) {
        const char c = *q;
        if (c == '"') {
            in_quotes = not in_quotes;
            continue;
        }
        if (not in_quotes and c == ' ') { // unquoted spaces are dropped
            continue;
        }
        any = true;
        if (is_digit(c)) {
            acc = acc * 10 + (c - '0');
        } else {
            numeric = false;
        }
    }

    if (not any) {
        return {};
    }
    if (numeric) {
        return acc;
    }

    auto buf = allocate_fast<Conf::StrBuffer>("conf-string");
    in_quotes = false;
    for (const char* q = begin; q < end; ++q) {
        const char c = *q;
        if (c == '"') {
            in_quotes = not in_quotes;
            continue;
        }
        if (not in_quotes and c == ' ') {
            continue;
        }
        buf->push_back(c);
    }
    return buf;
}


} // namespace


const Conf::Index& Conf::ensure_index(const char* file_data)
{
    if (index_ and (**index_).source == file_data) {
        return **index_;
    }

    static_assert(sizeof(Index) <= 2000,
                  "Conf::Index must fit within a single scratch buffer; "
                  "lower Conf::index_capacity if this fails.");

    index_ = allocate<Index>("conf-index");
    Index& idx = **index_;
    idx.source = file_data;
    idx.count = 0;
    idx.overflow = false;

    const char* const base = file_data;

    enumerate(file_data,
              [&idx, base](const char* sec,
                           int sl,
                           const char* key,
                           int kl,
                           const char* val) {
                  if (idx.count >= index_capacity) {
                      idx.overflow = true;
                      return false;
                  }
                  const u32 value_off = (u32)(val - base);
                  if (value_off > 0xffff or sl > 0xff or kl > 0xff) {
                      idx.overflow = true;
                      return false;
                  }
                  auto& e = idx.entries[idx.count++];
                  e.section_off = (u16)(sec - base);
                  e.key_off = (u16)(key - base);
                  e.value_off = (u16)value_off;
                  e.section_len = (u8)sl;
                  e.key_len = (u8)kl;
                  return true;
              });

    return idx;
}


Conf::Value
Conf::get(const char* file_data, const char* section, const char* key)
{
    if (not file_data) {
        return {};
    }

    const int section_len = PLATFORM.strlen(section);
    const int key_len = PLATFORM.strlen(key);

    // Fast path: the file we have cached and indexed.
    if (file_data == cached_file_data_) {
        const Index& idx = ensure_index(file_data);
        if (not idx.overflow) {
            const char* const base = idx.source;
            for (int i = 0; i < idx.count; ++i) {
                const IndexEntry& e = idx.entries[i];
                if (e.section_len == section_len and e.key_len == key_len and
                    bytes_equal(base + e.section_off, section, section_len) and
                    bytes_equal(base + e.key_off, key, key_len)) {
                    return make_value(base + e.value_off);
                }
            }
            return {};
        }
        // overflow: fall through to a direct scan
    }

    Value result{};
    enumerate(file_data,
              [&](const char* sec,
                  int sl,
                  const char* k,
                  int kl,
                  const char* val) {
                  if (sl == section_len and kl == key_len and
                      bytes_equal(sec, section, sl) and
                      bytes_equal(k, key, kl)) {
                      result = make_value(val);
                      return false;
                  }
                  return true;
              });
    return result;
}


Conf::Value Conf::get(const char* section, const char* key)
{
    if (not cached_file_data_) {
        cached_file_data_ = PLATFORM.load_file_contents("", "boot.ini");
    }
    return get(cached_file_data_, section, key);
}
