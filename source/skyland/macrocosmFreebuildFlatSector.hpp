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



namespace skyland::macro::terrain
{



class FreebuildFlatSector
    : public MacrocosmSectorImpl<FreebuildFlatSector, 14, 14, 5, 2>
{
public:
    FreebuildFlatSector(Vec2<s8> position)
        : MacrocosmSectorImpl(position, Shape::freebuild_flat)
    {
        erase();
        z_view_ = 6;
    }


    static constexpr const u16 screen_mapping_lut[14][14] = {
        {14, 45, 76, 107, 138, 169, 200, 231, 262, 293, 324, 355, 386, 417},
        {43, 74, 105, 136, 167, 198, 229, 260, 291, 322, 353, 384, 415, 446},
        {72, 103, 134, 165, 196, 227, 258, 289, 320, 351, 382, 413, 444, 475},
        {101, 132, 163, 194, 225, 256, 287, 318, 349, 380, 411, 442, 473, 504},
        {130, 161, 192, 223, 254, 285, 316, 347, 378, 409, 440, 471, 502, 533},
        {159, 190, 221, 252, 283, 314, 345, 376, 407, 438, 469, 500, 531, 562},
        {188, 219, 250, 281, 312, 343, 374, 405, 436, 467, 498, 529, 560, 591},
        {217, 248, 279, 310, 341, 372, 403, 434, 465, 496, 527, 558, 589, 620},
        {246, 277, 308, 339, 370, 401, 432, 463, 494, 525, 556, 587, 618, 649},
        {275, 306, 337, 368, 399, 430, 461, 492, 523, 554, 585, 616, 647, 678},
        {304, 335, 366, 397, 428, 459, 490, 521, 552, 583, 614, 645, 676, 707},
        {333, 364, 395, 426, 457, 488, 519, 550, 581, 612, 643, 674, 705, 736},
        {362, 393, 424, 455, 486, 517, 548, 579, 610, 641, 672, 703, 734, 765},
        {391, 422, 453, 484, 515, 546, 577, 608, 639, 670, 701, 732, 763, 794},
    };


    // clang-format off
    static constexpr const Vec2<u8> winding_path[] = {
        {0, 0},
        {1, 0}, {0, 1},
        {2, 0}, {1, 1}, {0, 2},
        {3, 0}, {2, 1}, {1, 2}, {0, 3},
        {4, 0}, {3, 1}, {2, 2}, {1, 3}, {0, 4},
        {5, 0}, {4, 1}, {3, 2}, {2, 3}, {1, 4}, {0, 5},
        {6, 0}, {5, 1}, {4, 2}, {3, 3}, {2, 4}, {1, 5}, {0, 6},
        {7, 0}, {6, 1}, {5, 2}, {4, 3}, {3, 4}, {2, 5}, {1, 6}, {0, 7},
        {8, 0}, {7, 1}, {6, 2}, {5, 3}, {4, 4}, {3, 5}, {2, 6}, {1, 7}, {0, 8},
        {9, 0}, {8, 1}, {7, 2}, {6, 3}, {5, 4}, {4, 5}, {3, 6}, {2, 7}, {1, 8}, {0, 9},
        {10, 0}, {9, 1}, {8, 2}, {7, 3}, {6, 4}, {5, 5}, {4, 6}, {3, 7}, {2, 8}, {1, 9}, {0, 10},
        {11, 0}, {10, 1}, {9, 2}, {8, 3}, {7, 4}, {6, 5}, {5, 6}, {4, 7}, {3, 8}, {2, 9}, {1, 10}, {0, 11},
        {12, 0}, {11, 1}, {10, 2}, {9, 3}, {8, 4}, {7, 5}, {6, 6}, {5, 7}, {4, 8}, {3, 9}, {2, 10}, {1, 11}, {0, 12},
        {13, 0}, {12, 1}, {11, 2}, {10, 3}, {9, 4}, {8, 5}, {7, 6}, {6, 7}, {5, 8}, {4, 9}, {3, 10}, {2, 11}, {1, 12}, {0, 13},
        {13, 1}, {12, 2}, {11, 3}, {10, 4}, {9, 5}, {8, 6}, {7, 7}, {6, 8}, {5, 9}, {4, 10}, {3, 11}, {2, 12}, {1, 13},
        {13, 2}, {12, 3}, {11, 4}, {10, 5}, {9, 6}, {8, 7}, {7, 8}, {6, 9}, {5, 10}, {4, 11}, {3, 12}, {2, 13},
        {13, 3}, {12, 4}, {11, 5}, {10, 6}, {9, 7}, {8, 8}, {7, 9}, {6, 10}, {5, 11}, {4, 12}, {3, 13},
        {13, 4}, {12, 5}, {11, 6}, {10, 7}, {9, 8}, {8, 9}, {7, 10}, {6, 11}, {5, 12}, {4, 13},
        {13, 5}, {12, 6}, {11, 7}, {10, 8}, {9, 9}, {8, 10}, {7, 11}, {6, 12}, {5, 13},
        {13, 6}, {12, 7}, {11, 8}, {10, 9}, {9, 10}, {8, 11}, {7, 12}, {6, 13},
        {13, 7}, {12, 8}, {11, 9}, {10, 10}, {9, 11}, {8, 12}, {7, 13},
        {13, 8}, {12, 9}, {11, 10}, {10, 11}, {9, 12}, {8, 13},
        {13, 9}, {12, 10}, {11, 11}, {10, 12}, {9, 13},
        {13, 10}, {12, 11}, {11, 12}, {10, 13},
        {13, 11}, {12, 12}, {11, 13},
        {13, 12}, {12, 13},
        {13, 13}
    };
    // clang-format on


    void update() override;


    void reset()
    {
        p_.orientation_ = Orientation::north;
        for (u8 z = 0; z < size().z; ++z) {
            for (u8 x = 0; x < size().x; ++x) {
                for (u8 y = 0; y < size().y; ++y) {
                    blocks_[z][x][y].type_ = 0;
                    blocks_[z][x][y].data_ = 0;
                }
            }
        }

        set_block({6, 6, 0}, macro::terrain::Type::terrain);
        set_block({6, 6, 1}, macro::terrain::Type::building);
        set_cursor({7, 6, 0});

        shadowcast();

        raster::globalstate::_changed = true;
        raster::globalstate::_shrunk = true;
    }


    static const int z_limit = 5;
    static const int length = 14;
};



} // namespace skyland::macro::terrain
