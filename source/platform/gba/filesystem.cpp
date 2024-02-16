////////////////////////////////////////////////////////////////////////////////
//
// MIT License
//
// Copyright (c) 2020-2024 Evan Bowman
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
////////////////////////////////////////////////////////////////////////////////



#pragma GCC diagnostic push
// Accessing data past __rom_end__ raises -Warray-bounds errors. These would be
// real errors, except that they aren't problematic on the gameboy advance, for
// various reasons, and we're mounting a filesystem at __rom_end__ anyway, so...
#pragma GCC diagnostic ignored "-Warray-bounds"
#pragma GCC diagnostic ignored "-Wstringop-overflow"
#pragma GCC diagnostic ignored "-Wstringop-overread"


#include "filesystem.hpp"
#include "platform/platform.hpp"
#include "string.hpp"



// Symbol declared by the linker script, points to the end of the ROM, where we
// will mount the files.
extern char __rom_end__;



namespace filesystem
{



Root* get_root()
{
    auto root = (Root*)&__rom_end__;
    if (not(root->magic_[0] == '_' and root->magic_[1] == 'F' and
            root->magic_[2] == 'S' and root->magic_[3] == '_')) {
        // The filesystem root must begin with the characters "_FS_". This is
        // how we detect whether there's a filesystem attached to the ROM.
        return nullptr;
    }
    return root;
}



u32 size()
{
    if (not is_mounted()) {
        return 0;
    }

    u32 size = sizeof(Root);

    const char* current = &__rom_end__;
    current += sizeof(Root);

    const auto root = get_root();

    u32 files_remaining = root->file_count_.get();


    while (files_remaining) {
        auto hdr = (FileHeader*)current;

        size += sizeof(FileHeader) + hdr->size_.get();

        --files_remaining;
        current += sizeof(FileHeader) + hdr->size_.get();
    }

    return size;
}



bool is_mounted()
{
    return get_root() not_eq nullptr;
}



void walk(Function<8 * sizeof(void*), void(const char* path)> callback)
{
    const char* current = &__rom_end__;
    current += sizeof(Root);

    const auto root = get_root();

    u32 files_remaining = root->file_count_.get();


    while (files_remaining) {
        auto hdr = (FileHeader*)current;

        callback(hdr->path_);

        --files_remaining;
        current += sizeof(FileHeader) + hdr->size_.get();
    }
}



Pair<FileContents, FileSize> load(FilePath path)
{
    const char* current = &__rom_end__;
    current += sizeof(Root);

    const auto root = get_root();

    u32 files_remaining = root->file_count_.get();


    while (files_remaining) {
        auto hdr = (FileHeader*)current;

        if (str_eq(hdr->path_, path)) {
            if ((u32)(intptr_t)(current + sizeof(FileHeader)) % 4 not_eq 0) {
                Platform::fatal("unaligned file");
            }
            return {current + sizeof(FileHeader), hdr->size_.get()};
        }

        --files_remaining;
        current += sizeof(FileHeader) + hdr->size_.get();
    }

    return {nullptr, 0};
}


} // namespace filesystem
#pragma GCC diagnostic pop
