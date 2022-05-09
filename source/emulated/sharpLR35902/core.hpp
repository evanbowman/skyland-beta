////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "allocator.hpp"



namespace lr35902
{



class Core
{
public:

    Core(const char* rom_path) :
        rom_path_(rom_path)
    {

    }

    void run(Platform& pfrm)
    {
        auto pc = pfrm.load_file_contents("", rom_path_.c_str());
        pc += 256; // skip header

        while (true) {
            u8 op = (*pc++);
            switch (op) {
            case 0: // nop
                info(pfrm, "process nop");
                break;

            case 243:
            case 251: // di
                info(pfrm, "process di");
                break;

            default:
                Platform::fatal(format("invalid opcode %", op).c_str());
            }
        }
    }


private:

    struct RamBank
    {
        struct Sector
        {
            u8 data_[2000];
        };

        RamBank() :
            s1_(allocate_dynamic<Sector>("lr35902-ram-bank-sector")),
            s2_(allocate_dynamic<Sector>("lr35902-ram-bank-sector"))
        {
        }

        void write(u16 offset, u8 val)
        {
            if (offset < 2000) {
                s1_->data_[offset] = val;
            } else if (offset < 4000) {
                s1_->data_[offset - 2000] = val;
            }
        }

        u8 read(u16 offset) const
        {
            if (offset < 2000) {
                return s1_->data_[offset];
            } else if (offset < 4000) {
                return s1_->data_[offset - 2000];
            }
        }

    private:
        DynamicMemory<Sector> s1_;
        DynamicMemory<Sector> s2_;
    };

    RamBank wram_[8];

    struct Registers
    {
        u8 a_;
        u8 b_;
        u8 c_;
        u8 d_;
        u8 h_;
        u8 l_;
        u16 sp_;
        u16 pc_;
    } registers_;

    StringBuffer<64> rom_path_;
};



}
