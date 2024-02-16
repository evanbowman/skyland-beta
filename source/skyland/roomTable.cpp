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

#include "roomTable.hpp"
#include <algorithm>
#include <limits>



namespace skyland
{



void RoomTable::reindex(bool re_sort)
{
    if (rooms_.empty()) {
        for (auto& elem : x_jump_table_) {
            elem = 0;
        }
        return;
    }

    static const auto uninit_index = std::numeric_limits<IndexType>::max();

    for (auto& elem : x_jump_table_) {
        // Initialize to an arbitrarily high number.
        elem = uninit_index;
    }

    if (re_sort) {
        std::sort(rooms_.begin(), rooms_.end(), [](auto& lhs, auto& rhs) {
            return lhs->position().x < rhs->position().x;
        });
    }

    for (u32 i = 0; i < rooms_.size(); ++i) {
        int room_min_x = rooms_[i]->position().x;

        for (int x = room_min_x; x < room_min_x + rooms_[i]->size().x; ++x) {
            if (x_jump_table_[x] > i) {
                x_jump_table_[x] = i;
            }
        }
    }

    for (int i = 0; i < map_width; ++i) {
        if (x_jump_table_[i] == uninit_index) {
            if (i > 0) {
                x_jump_table_[i] = x_jump_table_[i - 1];
            } else {
                x_jump_table_[i] = 0;
            }
        }
    }
}



} // namespace skyland
