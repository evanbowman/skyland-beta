#include "bootleg_cart.hpp"
#include "filesystem.hpp"
#include "gba.h"
#include "platform/platform.hpp"



// FIXME: I think bootleg carts actually support up to 64 * 1024 bytes of sram.
#define BOOTLEG_SRAM_SIZE 32 * 1024
#define BOOTLEG_SRAM ((u8*)0xE000000)



static u32 flash_sram_area = 0;



#define BOOTLEG_FLASH_WRITE(pa, pd)                                            \
    {                                                                          \
        *(((u16*)AGB_ROM) + ((pa) / 2)) = pd;                                  \
        __asm("nop");                                                          \
    }



// BORROWED FROM GOOMBA COLOR EMULATOR SOURCE CODE.
// NOTE FROM THE AUTHOR:
//
// This function will auto-detect four common
// types of reproduction flash cartridges.
// Must run in EWRAM because ROM data is
// not visible to the system while checking.
__attribute__((section(".ewram"))) u32 bootleg_get_flash_type()
{

#define AGB_ROM ((u8*)0x8000000)

    u32 rom_data, data;
    u16 ie = REG_IE;

    REG_IE = ie & 0xFFFE;

    rom_data = *(u32*)AGB_ROM;

    // Type 1 or 4
    BOOTLEG_FLASH_WRITE(0, 0xFF);
    BOOTLEG_FLASH_WRITE(0, 0x90);
    data = *(u32*)AGB_ROM;
    BOOTLEG_FLASH_WRITE(0, 0xFF);
    if (rom_data != data) {
        // Check if the chip is responding to this command
        // which then needs a different write command later
        BOOTLEG_FLASH_WRITE(0x59, 0x42);
        data = *(u8*)(AGB_ROM + 0xB2);
        BOOTLEG_FLASH_WRITE(0x59, 0x96);
        BOOTLEG_FLASH_WRITE(0, 0xFF);
        if (data != 0x96) {
            REG_IE = ie;
            return 4;
        }
        REG_IE = ie;
        return 1;
    }

    // Type 2
    BOOTLEG_FLASH_WRITE(0, 0xF0);
    BOOTLEG_FLASH_WRITE(0xAAA, 0xA9);
    BOOTLEG_FLASH_WRITE(0x555, 0x56);
    BOOTLEG_FLASH_WRITE(0xAAA, 0x90);
    data = *(u32*)AGB_ROM;
    BOOTLEG_FLASH_WRITE(0, 0xF0);
    if (rom_data != data) {
        REG_IE = ie;
        return 2;
    }

    // Type 3
    BOOTLEG_FLASH_WRITE(0, 0xF0);
    BOOTLEG_FLASH_WRITE(0xAAA, 0xAA);
    BOOTLEG_FLASH_WRITE(0x555, 0x55);
    BOOTLEG_FLASH_WRITE(0xAAA, 0x90);
    data = *(u32*)AGB_ROM;
    BOOTLEG_FLASH_WRITE(0, 0xF0);
    if (rom_data != data) {
        REG_IE = ie;
        return 3;
    }

    REG_IE = ie;
    return 0;
}



// This function will issue a flash sector erase
// operation at the given sector address and then
// write 64 kilobytes of SRAM data to Flash ROM.
// Must run in EWRAM because ROM data is
// not visible to the system while erasing/writing.
__attribute__((section(".ewram"))) void
bootleg_flash_write_impl(BootlegFlashType flash_type)
{
    if (flash_sram_area == 0) {
        return;
    }
    u32 sa = flash_sram_area;

    if (flash_type == 0)
        return;
    u16 ie = REG_IE;

    REG_IE = ie & 0xFFFE;

    if (flash_type == 1) {
        // Erase flash sector
        BOOTLEG_FLASH_WRITE(sa, 0xFF);
        BOOTLEG_FLASH_WRITE(sa, 0x60);
        BOOTLEG_FLASH_WRITE(sa, 0xD0);
        BOOTLEG_FLASH_WRITE(sa, 0x20);
        BOOTLEG_FLASH_WRITE(sa, 0xD0);
        while (1) {
            __asm("nop");
            if (*(((u16*)AGB_ROM) + (sa / 2)) == 0x80) {
                break;
            }
        }
        BOOTLEG_FLASH_WRITE(sa, 0xFF);

        // Write data
        for (int i = 0; i < BOOTLEG_SRAM_SIZE; i += 2) {
            BOOTLEG_FLASH_WRITE(sa + i, 0x40);
            BOOTLEG_FLASH_WRITE(sa + i,
                                (*(u8*)(BOOTLEG_SRAM + i + 1)) << 8 |
                                    (*(u8*)(BOOTLEG_SRAM + i)));
            while (1) {
                __asm("nop");
                if (*(((u16*)AGB_ROM) + (sa / 2)) == 0x80) {
                    break;
                }
            }
        }
        BOOTLEG_FLASH_WRITE(sa, 0xFF);

    } else if (flash_type == 2) {
        // Erase flash sector
        BOOTLEG_FLASH_WRITE(sa, 0xF0);
        BOOTLEG_FLASH_WRITE(0xAAA, 0xA9);
        BOOTLEG_FLASH_WRITE(0x555, 0x56);
        BOOTLEG_FLASH_WRITE(0xAAA, 0x80);
        BOOTLEG_FLASH_WRITE(0xAAA, 0xA9);
        BOOTLEG_FLASH_WRITE(0x555, 0x56);
        BOOTLEG_FLASH_WRITE(sa, 0x30);
        while (1) {
            __asm("nop");
            if (*(((u16*)AGB_ROM) + (sa / 2)) == 0xFFFF) {
                break;
            }
        }
        BOOTLEG_FLASH_WRITE(sa, 0xF0);

        // Write data
        for (int i = 0; i < BOOTLEG_SRAM_SIZE; i += 2) {
            BOOTLEG_FLASH_WRITE(0xAAA, 0xA9);
            BOOTLEG_FLASH_WRITE(0x555, 0x56);
            BOOTLEG_FLASH_WRITE(0xAAA, 0xA0);
            BOOTLEG_FLASH_WRITE(sa + i,
                                (*(u8*)(BOOTLEG_SRAM + i + 1)) << 8 |
                                    (*(u8*)(BOOTLEG_SRAM + i)));
            while (1) {
                __asm("nop");
                if (*(((u16*)AGB_ROM) + ((sa + i) / 2)) ==
                    ((*(u8*)(BOOTLEG_SRAM + i + 1)) << 8 |
                     (*(u8*)(BOOTLEG_SRAM + i)))) {
                    break;
                }
            }
        }
        BOOTLEG_FLASH_WRITE(sa, 0xF0);

    } else if (flash_type == 3) {
        // Erase flash sector
        BOOTLEG_FLASH_WRITE(sa, 0xF0);
        BOOTLEG_FLASH_WRITE(0xAAA, 0xAA);
        BOOTLEG_FLASH_WRITE(0x555, 0x55);
        BOOTLEG_FLASH_WRITE(0xAAA, 0x80);
        BOOTLEG_FLASH_WRITE(0xAAA, 0xAA);
        BOOTLEG_FLASH_WRITE(0x555, 0x55);
        BOOTLEG_FLASH_WRITE(sa, 0x30);
        while (1) {
            __asm("nop");
            if (*(((u16*)AGB_ROM) + (sa / 2)) == 0xFFFF) {
                break;
            }
        }
        BOOTLEG_FLASH_WRITE(sa, 0xF0);

        // Write data
        for (int i = 0; i < BOOTLEG_SRAM_SIZE; i += 2) {
            BOOTLEG_FLASH_WRITE(0xAAA, 0xAA);
            BOOTLEG_FLASH_WRITE(0x555, 0x55);
            BOOTLEG_FLASH_WRITE(0xAAA, 0xA0);
            BOOTLEG_FLASH_WRITE(sa + i,
                                (*(u8*)(BOOTLEG_SRAM + i + 1)) << 8 |
                                    (*(u8*)(BOOTLEG_SRAM + i)));
            while (1) {
                __asm("nop");
                if (*(((u16*)AGB_ROM) + ((sa + i) / 2)) ==
                    ((*(u8*)(BOOTLEG_SRAM + i + 1)) << 8 |
                     (*(u8*)(BOOTLEG_SRAM + i)))) {
                    break;
                }
            }
        }
        BOOTLEG_FLASH_WRITE(sa, 0xF0);

    } else if (flash_type == 4) {
        // Erase flash sector
        BOOTLEG_FLASH_WRITE(sa, 0xFF);
        BOOTLEG_FLASH_WRITE(sa, 0x60);
        BOOTLEG_FLASH_WRITE(sa, 0xD0);
        BOOTLEG_FLASH_WRITE(sa, 0x20);
        BOOTLEG_FLASH_WRITE(sa, 0xD0);
        while (1) {
            __asm("nop");
            if ((*(((u16*)AGB_ROM) + (sa / 2)) & 0x80) == 0x80) {
                break;
            }
        }
        BOOTLEG_FLASH_WRITE(sa, 0xFF);

        // Write data
        int c = 0;
        while (c < BOOTLEG_SRAM_SIZE) {
            BOOTLEG_FLASH_WRITE(sa + c, 0xEA);
            while (1) {
                __asm("nop");
                if ((*(((u16*)AGB_ROM) + ((sa + c) / 2)) & 0x80) == 0x80) {
                    break;
                }
            }
            BOOTLEG_FLASH_WRITE(sa + c, 0x1FF);
            for (int i = 0; i < 1024; i += 2) {
                BOOTLEG_FLASH_WRITE(sa + c + i,
                                    (*(u8*)(BOOTLEG_SRAM + c + i + 1)) << 8 |
                                        (*(u8*)(BOOTLEG_SRAM + c + i)));
            }
            BOOTLEG_FLASH_WRITE(sa + c, 0xD0);
            while (1) {
                __asm("nop");
                if ((*(((u16*)AGB_ROM) + ((sa + c) / 2)) & 0x80) == 0x80) {
                    break;
                }
            }
            BOOTLEG_FLASH_WRITE(sa + c, 0xFF);
            c += 1024;
        }
    }

    REG_IE = ie;
}



void bootleg_flash_write(BootlegFlashType flash_type)
{
    REG_SNDDMGCNT &= ~(1 << 0xb);
    REG_SNDDMGCNT &= ~(1 << 0xf);

    bootleg_flash_write_impl(flash_type);

    REG_SOUNDCNT_H |= (1 << 9);
    REG_SOUNDCNT_H |= (1 << 8);
}



extern char __rom_end__;



static void bytecopy(u8* dest, u8* src, u32 size)
{
    while (size--) {
        *(dest++) = *(src++);
    }
}



void bootleg_cart_init_sram(Platform& pfrm)
{
    u32 total_rom_size = (__rom_end__ - 0x8000000) + filesystem::size();
    u32 flash_size = 0;
    flash_sram_area = 0;

    // Determine the size of the flash chip by checking for ROM loops,
    // then set the SRAM storage area 0x40000 bytes before the end.
    // This is due to different sector sizes of different flash chips,
    // and should hopefully cover all cases.
    if (memcmp(AGB_ROM + 4, AGB_ROM + 4 + 0x400000, 0x40) == 0) {
        flash_size = 0x400000;
    } else if (memcmp(AGB_ROM + 4, AGB_ROM + 4 + 0x800000, 0x40) == 0) {
        flash_size = 0x800000;
    } else if (memcmp(AGB_ROM + 4, AGB_ROM + 4 + 0x1000000, 0x40) == 0) {
        flash_size = 0x1000000;
    } else {
        flash_size = 0x2000000;
    }
    if (flash_sram_area == 0) {
        flash_sram_area = flash_size - 0x40000;
    }

    // RIP if the selected storage area is within the Goomba Color ROM...
    if (total_rom_size > flash_sram_area) {
        flash_sram_area = 0;
        info(pfrm, "ROM too large to allocate repro flash sram area!");
        return;
    }

    // Finally, restore the SRAM data and proceed.
    bytecopy(BOOTLEG_SRAM, ((u8*)AGB_ROM + flash_sram_area), BOOTLEG_SRAM_SIZE);

    info(pfrm, "Restored SRAM from repro flash.");
}