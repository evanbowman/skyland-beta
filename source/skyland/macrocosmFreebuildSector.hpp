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


#include "macrocosmSector.hpp"
#include "macrocosmSectorImpl.hpp"
#include "platform/flash_filesystem.hpp"
#include "rle.hpp"



namespace skyland::macro::terrain
{



class FreebuildSector
    : public MacrocosmSectorImpl<FreebuildSector, 10, 10, 9, 8>
{
public:
    FreebuildSector(Vec2<s8> position)
        : MacrocosmSectorImpl(position, Shape::freebuild)
    {
        erase();
        z_view_ = 9;
    }


    FreebuildSector* cast_freebuild_sector() override
    {
        return this;
    }


    void reset()
    {
        p_.orientation_ = Orientation::north;
        for (u8 z = 0; z < 9; ++z) {
            for (u8 x = 0; x < 10; ++x) {
                for (u8 y = 0; y < 10; ++y) {
                    blocks_[z][x][y].type_ = 0;
                    blocks_[z][x][y].data_ = 0;
                }
            }
        }

        set_block({4, 4, 0}, macro::terrain::Type::terrain);
        set_block({4, 4, 1}, macro::terrain::Type::building);
        set_cursor({5, 4, 0});

        shadowcast();

        raster::globalstate::_changed = true;
        raster::globalstate::_shrunk = true;
    }


    static constexpr const u16 screen_mapping_lut[12][12] = {
        {14, 45, 76, 107, 138, 169, 200, 231, 262, 293, 324, 355},
        {43, 74, 105, 136, 167, 198, 229, 260, 291, 322, 353, 384},
        {72, 103, 134, 165, 196, 227, 258, 289, 320, 351, 382, 413},
        {101, 132, 163, 194, 225, 256, 287, 318, 349, 380, 411, 442},
        {130, 161, 192, 223, 254, 285, 316, 347, 378, 409, 440, 471},
        {159, 190, 221, 252, 283, 314, 345, 376, 407, 438, 469, 500},
        {188, 219, 250, 281, 312, 343, 374, 405, 436, 467, 498, 529},
        {217, 248, 279, 310, 341, 372, 403, 434, 465, 496, 527, 558},
        {246, 277, 308, 339, 370, 401, 432, 463, 494, 525, 556, 587},
        {275, 306, 337, 368, 399, 430, 461, 492, 523, 554, 585, 616},
        {304, 335, 366, 397, 428, 459, 490, 521, 552, 583, 614, 645},
        {333, 364, 395, 426, 457, 488, 519, 550, 581, 612, 643, 674},
    };


    static constexpr const Vec2<u8> winding_path[] = {
        {0, 0}, {1, 0}, {0, 1}, {2, 0}, {1, 1}, {0, 2}, {3, 0}, {2, 1}, {1, 2},
        {0, 3}, {4, 0}, {3, 1}, {2, 2}, {1, 3}, {0, 4}, {5, 0}, {4, 1}, {3, 2},
        {2, 3}, {1, 4}, {0, 5}, {6, 0}, {5, 1}, {4, 2}, {3, 3}, {2, 4}, {1, 5},
        {0, 6}, {7, 0}, {6, 1}, {5, 2}, {4, 3}, {3, 4}, {2, 5}, {1, 6}, {0, 7},
        {8, 0}, {7, 1}, {6, 2}, {5, 3}, {4, 4}, {3, 5}, {2, 6}, {1, 7}, {0, 8},
        {9, 0}, {8, 1}, {7, 2}, {6, 3}, {5, 4}, {4, 5}, {3, 6}, {2, 7}, {1, 8},
        {0, 9}, {9, 1}, {8, 2}, {7, 3}, {6, 4}, {5, 5}, {4, 6}, {3, 7}, {2, 8},
        {1, 9}, {9, 2}, {8, 3}, {7, 4}, {6, 5}, {5, 6}, {4, 7}, {3, 8}, {2, 9},
        {9, 3}, {8, 4}, {7, 5}, {6, 6}, {5, 7}, {4, 8}, {3, 9}, {9, 4}, {8, 5},
        {7, 6}, {6, 7}, {5, 8}, {4, 9}, {9, 5}, {8, 6}, {7, 7}, {6, 8}, {5, 9},
        {9, 6}, {8, 7}, {7, 8}, {6, 9}, {9, 7}, {8, 8}, {7, 9}, {9, 8}, {8, 9},
        {9, 9},
    };

    void update() override;
};



} // namespace skyland::macro::terrain
