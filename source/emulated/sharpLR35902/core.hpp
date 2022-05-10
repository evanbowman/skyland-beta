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

    enum class Opcode : u8
    {
     nop       = 0x00,
     jp_a16    = 0xc3,
     ld_sp_imm = 0x31,
     cp_a_imm  = 0xfe,
     di        = 0xf3,
     di_mirror = 0xfb,
     ld_a_imm  = 0x3e,
     ldh       = 0xe0,
     jr_cc     = 0x28,
    };

    enum
    {
     GB_CARRY_FLAG = 16,
     GB_HALF_CARRY_FLAG = 32,
     GB_SUBTRACT_FLAG = 64,
     GB_ZERO_FLAG = 128,
    };



    bool cond(u8 opcode) const
    {
        switch ((opcode >> 3) & 0x3) {
        case 0:
            return not (registers_.af_ & GB_ZERO_FLAG);
        case 1:
            return (registers_.af_ & GB_ZERO_FLAG);
        case 2:
            return not (registers_.af_ & GB_CARRY_FLAG);
        case 3:
            return (registers_.af_ & GB_CARRY_FLAG);
        default:
            __builtin_unreachable();
        }

        return false;
    }



    void run(Platform& pfrm)
    {
        const auto rom_base = (u8*)pfrm.load_file_contents("", rom_path_.c_str());
        if (rom_base == nullptr) {
            Platform::fatal("failed to load rom data");
        }

        auto pc = rom_base + 0x100; // skip header

        while (true) {
            u8 op = (*(pc++));
            switch ((Opcode)op) {
            case Opcode::nop: {
                break;
            }

            case Opcode::jp_a16: {
                u16 addr = *pc;
                addr |= *(pc + 1) << 8;
                pc = rom_base + addr;
                break;
            }

            case Opcode::ld_sp_imm: {
                u16 value = *(pc++);
                value |= *(pc++) << 8;
                registers_.sp_ = value;
                break;
            }

            case Opcode::cp_a_imm: {
                u8 value = *(pc++);
                u8 a = registers_.af_ >> 8;
                registers_.af_ &= 0xff00;
                registers_.af_ |= GB_SUBTRACT_FLAG;
                if (a == value) {
                    registers_.af_ |= GB_ZERO_FLAG;
                }
                if ((a & 0xf) < (value & 0xf)) {
                    registers_.af_ |= GB_HALF_CARRY_FLAG;
                }
                if (a < value) {
                    registers_.af_ |= GB_CARRY_FLAG;
                }
                break;
            }

            case Opcode::jr_cc: {
                u8 offset = *(pc++);
                if (cond(op)) {
                    pc += offset;
                }
                break;
            }

            case Opcode::ld_a_imm: {
                registers_.af_ &= 0x00ff;
                registers_.af_ |= *(pc++) << 8;
                break;
            }

            case Opcode::ldh: {
                [[maybe_unused]]
                u8 hram_offset = *(pc++);
                [[maybe_unused]]
                u8 addr = 0xff00 + hram_offset;
                // TODO: the actual write...
                break;
            }

            case Opcode::di:
            case Opcode::di_mirror: {
                registers_.ime_ = 0;
                break;
            }

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
        u16 af_ = 0x1100; // We're emulating the CGB specifically.
        u8 b_;
        u8 c_;
        u8 d_;
        u8 h_;
        u8 l_;
        u16 sp_;
        u16 pc_;
        u16 ime_ = 1;
    } registers_;

    StringBuffer<64> rom_path_;
};



}
