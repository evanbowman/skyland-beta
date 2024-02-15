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


#include "localization.hpp"
#include "platform/platform.hpp"
#include "script/lisp.hpp"



StringBuffer<32> format_time(u32 seconds, bool include_hours)
{
    StringBuffer<32> result;
    char buffer[32];

    int hours = seconds / 3600;
    int remainder = (int)seconds - hours * 3600;
    int mins = remainder / 60;
    remainder = remainder - mins * 60;
    int secs = remainder;

    if (include_hours) {
        locale_num2str(hours, buffer, 10);
        result += buffer;
        result += ":";
    }

    locale_num2str(mins, buffer, 10);
    result += buffer;
    result += ":";

    if (secs < 10) {
        result += "0";
    }

    locale_num2str(secs, buffer, 10);
    result += buffer;

    return result;
}



class str_const
{
private:
    const char8_t* const p_;
    const size_t sz_;

public:
    template <size_t N>
    constexpr str_const(const char8_t (&a)[N]) : p_(a), sz_(N - 1)
    {
    }

    constexpr char8_t operator[](std::size_t n)
    {
        return n < sz_ ? p_[n] : '0';
    }
};


#ifndef __BYTE_ORDER__
#error "byte order must be defined"
#endif


// FIXME: assumes little endian? Does it matter though, which way we order
// stuff, as long as it's consistent? Actually it does matter, considering
// that we're byte-swapping stuff in unicode.hpp
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#error "TODO: fix the utf-8 decoding (below) for big endian"
#endif


// Needs to be a macro because there's no way to pass a str_const as a
// constexpr parameter. Converts the first utf-8 codepoint in a string to a
// 32-bit integer, for use in a giant switch statement (below).
#define UTF8_GETCHR(_STR_)                                                     \
    []() -> utf8::Codepoint {                                                  \
        if constexpr ((str_const(_STR_)[0] & 0x80) == 0) {                     \
            return str_const(_STR_)[0];                                        \
        } else if constexpr ((str_const(_STR_)[0] & 0xf0) == 0xC0 ||           \
                             (str_const(_STR_)[0] & 0xf0) == 0xD0) {           \
            return (u32)(u8)str_const(_STR_)[0] |                              \
                   (((u32)(u8)str_const(_STR_)[1]) << 8);                      \
        } else if constexpr ((str_const(_STR_)[0] & 0xf0) == 0xE0) {           \
            return (u32)(u8)str_const(_STR_)[0] |                              \
                   (((u32)(u8)str_const(_STR_)[1]) << 8) |                     \
                   ((u32)(u8)str_const(_STR_)[2] << 16);                       \
        } else if constexpr ((str_const(_STR_)[0] & 0xf0) == 0xF0) {           \
            return (u32)(u8)str_const(_STR_)[0] |                              \
                   ((u32)(u8)str_const(_STR_)[1] << 8) |                       \
                   ((u32)(u8)str_const(_STR_)[2] << 16) |                      \
                   ((u32)(u8)str_const(_STR_)[3] << 24);                       \
        } else {                                                               \
            return 0;                                                          \
        }                                                                      \
    }()


template <u32 B, bool C> constexpr void my_assert()
{
    static_assert(C, "oh no");
}

#define UTF8_TESTCHR(_STR_)                                                    \
    []() -> utf8::Codepoint {                                                  \
        my_assert<(u32)(u8)str_const(_STR_)[0], false>();                      \
        return 0;                                                              \
    }()



static const char* font_image = "charset";



void set_font_image(const char* font_image_name)
{
    font_image = font_image_name;
}



Optional<Platform::TextureMapping>
extended_charset_map(const utf8::Codepoint& cp);



Optional<Platform::TextureMapping>
standard_charset_map(const utf8::Codepoint& cp)
{
    auto mapping = [&]() -> Optional<u16> {
        switch (cp) {

            // clang-format off

        case UTF8_GETCHR(u8"0"): return 1;
        case UTF8_GETCHR(u8"1"): return 2;
        case UTF8_GETCHR(u8"2"): return 3;
        case UTF8_GETCHR(u8"3"): return 4;
        case UTF8_GETCHR(u8"4"): return 5;
        case UTF8_GETCHR(u8"5"): return 6;
        case UTF8_GETCHR(u8"6"): return 7;
        case UTF8_GETCHR(u8"7"): return 8;
        case UTF8_GETCHR(u8"8"): return 9;
        case UTF8_GETCHR(u8"9"): return 10;
        case UTF8_GETCHR(u8"a"): return 11;
        case UTF8_GETCHR(u8"b"): return 12;
        case UTF8_GETCHR(u8"c"): return 13;
        case UTF8_GETCHR(u8"d"): return 14;
        case UTF8_GETCHR(u8"e"): return 15;
        case UTF8_GETCHR(u8"f"): return 16;
        case UTF8_GETCHR(u8"g"): return 17;
        case UTF8_GETCHR(u8"h"): return 18;
        case UTF8_GETCHR(u8"i"): return 19;
        case UTF8_GETCHR(u8"j"): return 20;
        case UTF8_GETCHR(u8"k"): return 21;
        case UTF8_GETCHR(u8"l"): return 22;
        case UTF8_GETCHR(u8"m"): return 23;
        case UTF8_GETCHR(u8"n"): return 24;
        case UTF8_GETCHR(u8"o"): return 25;
        case UTF8_GETCHR(u8"p"): return 26;
        case UTF8_GETCHR(u8"q"): return 27;
        case UTF8_GETCHR(u8"r"): return 28;
        case UTF8_GETCHR(u8"s"): return 29;
        case UTF8_GETCHR(u8"t"): return 30;
        case UTF8_GETCHR(u8"u"): return 31;
        case UTF8_GETCHR(u8"v"): return 32;
        case UTF8_GETCHR(u8"w"): return 33;
        case UTF8_GETCHR(u8"x"): return 34;
        case UTF8_GETCHR(u8"y"): return 35;
        case UTF8_GETCHR(u8"z"): return 36;
        case UTF8_GETCHR(u8"."): return 37;
        case UTF8_GETCHR(u8","): return 38;
        case UTF8_GETCHR(u8"，"): return 38;
        case UTF8_GETCHR(u8"A"): return 39;
        case UTF8_GETCHR(u8"B"): return 40;
        case UTF8_GETCHR(u8"C"): return 41;
        case UTF8_GETCHR(u8"D"): return 42;
        case UTF8_GETCHR(u8"E"): return 43;
        case UTF8_GETCHR(u8"F"): return 44;
        case UTF8_GETCHR(u8"G"): return 45;
        case UTF8_GETCHR(u8"H"): return 46;
        case UTF8_GETCHR(u8"I"): return 47;
        case UTF8_GETCHR(u8"J"): return 48;
        case UTF8_GETCHR(u8"K"): return 49;
        case UTF8_GETCHR(u8"L"): return 50;
        case UTF8_GETCHR(u8"M"): return 51;
        case UTF8_GETCHR(u8"N"): return 52;
        case UTF8_GETCHR(u8"O"): return 53;
        case UTF8_GETCHR(u8"P"): return 54;
        case UTF8_GETCHR(u8"Q"): return 55;
        case UTF8_GETCHR(u8"R"): return 56;
        case UTF8_GETCHR(u8"S"): return 57;
        case UTF8_GETCHR(u8"T"): return 58;
        case UTF8_GETCHR(u8"U"): return 59;
        case UTF8_GETCHR(u8"V"): return 60;
        case UTF8_GETCHR(u8"W"): return 61;
        case UTF8_GETCHR(u8"X"): return 62;
        case UTF8_GETCHR(u8"Y"): return 63;
        case UTF8_GETCHR(u8"Z"): return 64;
        case UTF8_GETCHR(u8"\""): return 65;
        case UTF8_GETCHR(u8"'"): return 66;
        case UTF8_GETCHR(u8"["): return 67;
        case UTF8_GETCHR(u8"]"): return 68;
        case UTF8_GETCHR(u8"("): return 69;
        case UTF8_GETCHR(u8")"): return 70;
        case UTF8_GETCHR(u8":"): return 71;
        case UTF8_GETCHR(u8" "): return 72;
        case UTF8_GETCHR(u8"%"): return 93;
        case UTF8_GETCHR(u8"!"): return 94;
        case UTF8_GETCHR(u8"！"): return 94;
        case UTF8_GETCHR(u8"？"): return 95;
        case UTF8_GETCHR(u8"?"): return 95;
        case UTF8_GETCHR(u8"+"): return 98;
        case UTF8_GETCHR(u8"-"): return 99;
        case UTF8_GETCHR(u8"/"): return 100;
        case UTF8_GETCHR(u8"\\"): return 397;
        case UTF8_GETCHR(u8"*"): return 101;
        case UTF8_GETCHR(u8"="): return 102;
        case UTF8_GETCHR(u8"<"): return 103;
        case UTF8_GETCHR(u8">"): return 104;
        case UTF8_GETCHR(u8"#"): return 105;
        case UTF8_GETCHR(u8"_"): return 186;
        case UTF8_GETCHR(u8"$"): return 392;
        case UTF8_GETCHR(u8";"): return 393;
        case UTF8_GETCHR(u8"\n"): return 394;
        case UTF8_GETCHR(u8"`"): return 395;
        case UTF8_GETCHR(u8"@"): return 396;
        case UTF8_GETCHR(u8"©"): return 185;
        case UTF8_GETCHR(u8"。"): return 302;
        case UTF8_GETCHR(u8"|"): return 311;

        case UTF8_GETCHR(u8"単"): return 399;
        case UTF8_GETCHR(u8"頚"): return 400;
        case UTF8_GETCHR(u8"憤"): return 401;
        case UTF8_GETCHR(u8"廃"): return 402;
        case UTF8_GETCHR(u8"妄"): return 403;
        case UTF8_GETCHR(u8"順"): return 404;
        case UTF8_GETCHR(u8"蔑"): return 405;
        case UTF8_GETCHR(u8"畑"): return 406;
        case UTF8_GETCHR(u8"氷"): return 407;
        case UTF8_GETCHR(u8"剣"): return 408;
        case UTF8_GETCHR(u8"捨"): return 409;
        case UTF8_GETCHR(u8"珂"): return 410;
        case UTF8_GETCHR(u8"環"): return 411;
        case UTF8_GETCHR(u8"犀"): return 412;
        case UTF8_GETCHR(u8"孜"): return 413;
        case UTF8_GETCHR(u8"懇"): return 414;
        case UTF8_GETCHR(u8"倫"): return 415;
        case UTF8_GETCHR(u8"盤"): return 416;
        case UTF8_GETCHR(u8"崎"): return 417;
        case UTF8_GETCHR(u8"綾"): return 419;
        case UTF8_GETCHR(u8"羅"): return 420;
        case UTF8_GETCHR(u8"啓"): return 421;
        case UTF8_GETCHR(u8"違"): return 422;
        case UTF8_GETCHR(u8"後"): return 423;
        case UTF8_GETCHR(u8"裾"): return 424;
        case UTF8_GETCHR(u8"蘇"): return 425;
        case UTF8_GETCHR(u8"隊"): return 426;
        case UTF8_GETCHR(u8"鋭"): return 427;
        case UTF8_GETCHR(u8"憧"): return 428;
        case UTF8_GETCHR(u8"渓"): return 429;
        case UTF8_GETCHR(u8"減"): return 430;
        case UTF8_GETCHR(u8"縞"): return 431;
        case UTF8_GETCHR(u8"狩"): return 432;
        case UTF8_GETCHR(u8"績"): return 433;
        case UTF8_GETCHR(u8"鄭"): return 434;
        case UTF8_GETCHR(u8"醜"): return 435;
        case UTF8_GETCHR(u8"鯨"): return 436;
        case UTF8_GETCHR(u8"訂"): return 437;
        case UTF8_GETCHR(u8"芥"): return 438;
        case UTF8_GETCHR(u8"襄"): return 439;
        case UTF8_GETCHR(u8"頂"): return 440;
        case UTF8_GETCHR(u8"掛"): return 441;
        case UTF8_GETCHR(u8"箸"): return 442;
        case UTF8_GETCHR(u8"潔"): return 443;
        case UTF8_GETCHR(u8"揮"): return 444;
        case UTF8_GETCHR(u8"椎"): return 445;
        case UTF8_GETCHR(u8"沖"): return 446;
        case UTF8_GETCHR(u8"頑"): return 447;
        case UTF8_GETCHR(u8"鵠"): return 448;
        case UTF8_GETCHR(u8"語"): return 449;
        case UTF8_GETCHR(u8"奄"): return 450;
        case UTF8_GETCHR(u8"賓"): return 451;
        case UTF8_GETCHR(u8"謙"): return 452;
        case UTF8_GETCHR(u8"曽"): return 453;
        case UTF8_GETCHR(u8"穀"): return 454;
        case UTF8_GETCHR(u8"漬"): return 455;
        case UTF8_GETCHR(u8"罵"): return 456;
        case UTF8_GETCHR(u8"営"): return 457;
        case UTF8_GETCHR(u8"紙"): return 458;
        case UTF8_GETCHR(u8"笠"): return 459;
        case UTF8_GETCHR(u8"拷"): return 460;
        case UTF8_GETCHR(u8"帖"): return 461;
        case UTF8_GETCHR(u8"瀋"): return 462;
        case UTF8_GETCHR(u8"務"): return 463;
        case UTF8_GETCHR(u8"敷"): return 464;
        case UTF8_GETCHR(u8"圧"): return 465;
        case UTF8_GETCHR(u8"逓"): return 466;
        case UTF8_GETCHR(u8"産"): return 467;
        case UTF8_GETCHR(u8"濃"): return 468;
        case UTF8_GETCHR(u8"録"): return 469;
        case UTF8_GETCHR(u8"洸"): return 470;
        case UTF8_GETCHR(u8"議"): return 471;
        case UTF8_GETCHR(u8"翌"): return 472;
        case UTF8_GETCHR(u8"凱"): return 473;
        case UTF8_GETCHR(u8"諸"): return 474;
        case UTF8_GETCHR(u8"坪"): return 475;
        case UTF8_GETCHR(u8"嵐"): return 476;
        case UTF8_GETCHR(u8"徘"): return 477;
        case UTF8_GETCHR(u8"敵"): return 478;
        case UTF8_GETCHR(u8"茨"): return 479;
        case UTF8_GETCHR(u8"刃"): return 480;
        case UTF8_GETCHR(u8"僑"): return 481;
        case UTF8_GETCHR(u8"噺"): return 482;
        case UTF8_GETCHR(u8"甕"): return 483;
        case UTF8_GETCHR(u8"嬢"): return 484;
        case UTF8_GETCHR(u8"費"): return 485;
        case UTF8_GETCHR(u8"碓"): return 486;
        case UTF8_GETCHR(u8"遊"): return 487;
        case UTF8_GETCHR(u8"鷲"): return 488;
        case UTF8_GETCHR(u8"栃"): return 489;
        case UTF8_GETCHR(u8"鳴"): return 490;
        case UTF8_GETCHR(u8"屯"): return 491;
        case UTF8_GETCHR(u8"閥"): return 492;
        case UTF8_GETCHR(u8"鯉"): return 493;
        case UTF8_GETCHR(u8"罰"): return 494;
        case UTF8_GETCHR(u8"診"): return 495;
        case UTF8_GETCHR(u8"詫"): return 496;
        case UTF8_GETCHR(u8"兜"): return 497;
        case UTF8_GETCHR(u8"黛"): return 498;
        case UTF8_GETCHR(u8"葉"): return 499;
        case UTF8_GETCHR(u8"剛"): return 500;
        case UTF8_GETCHR(u8"瑛"): return 501;
        case UTF8_GETCHR(u8"芹"): return 502;
        case UTF8_GETCHR(u8"荏"): return 503;
        case UTF8_GETCHR(u8"楽"): return 504;
        case UTF8_GETCHR(u8"納"): return 505;
        case UTF8_GETCHR(u8"琶"): return 506;
        case UTF8_GETCHR(u8"浄"): return 507;
        case UTF8_GETCHR(u8"隈"): return 508;
        case UTF8_GETCHR(u8"勅"): return 509;
        case UTF8_GETCHR(u8"隷"): return 510;
        case UTF8_GETCHR(u8"舛"): return 511;
        case UTF8_GETCHR(u8"漸"): return 512;
        case UTF8_GETCHR(u8"魁"): return 513;
        case UTF8_GETCHR(u8"稲"): return 514;
        case UTF8_GETCHR(u8"呂"): return 515;
        case UTF8_GETCHR(u8"眞"): return 516;
        case UTF8_GETCHR(u8"徴"): return 517;
        case UTF8_GETCHR(u8"溥"): return 518;
        case UTF8_GETCHR(u8"蔵"): return 519;
        case UTF8_GETCHR(u8"蹴"): return 520;
        case UTF8_GETCHR(u8"込"): return 521;
        case UTF8_GETCHR(u8"幌"): return 522;
        case UTF8_GETCHR(u8"瞑"): return 523;
        case UTF8_GETCHR(u8"楕"): return 524;
        case UTF8_GETCHR(u8"説"): return 525;
        case UTF8_GETCHR(u8"談"): return 526;
        case UTF8_GETCHR(u8"酵"): return 527;
        case UTF8_GETCHR(u8"詳"): return 528;
        case UTF8_GETCHR(u8"騰"): return 529;
        case UTF8_GETCHR(u8"堺"): return 530;
        case UTF8_GETCHR(u8"渕"): return 531;
        case UTF8_GETCHR(u8"矯"): return 532;
        case UTF8_GETCHR(u8"粋"): return 533;
        case UTF8_GETCHR(u8"紳"): return 534;
        case UTF8_GETCHR(u8"簡"): return 535;
        case UTF8_GETCHR(u8"麹"): return 536;
        case UTF8_GETCHR(u8"謳"): return 537;
        case UTF8_GETCHR(u8"陰"): return 538;
        case UTF8_GETCHR(u8"磐"): return 539;
        case UTF8_GETCHR(u8"窮"): return 540;
        case UTF8_GETCHR(u8"芯"): return 541;
        case UTF8_GETCHR(u8"艶"): return 542;
        case UTF8_GETCHR(u8"謡"): return 543;
        case UTF8_GETCHR(u8"俳"): return 544;
        case UTF8_GETCHR(u8"溝"): return 545;
        case UTF8_GETCHR(u8"磯"): return 546;
        case UTF8_GETCHR(u8"緋"): return 547;
        case UTF8_GETCHR(u8"偽"): return 548;
        case UTF8_GETCHR(u8"旭"): return 549;
        case UTF8_GETCHR(u8"労"): return 550;
        case UTF8_GETCHR(u8"押"): return 551;
        case UTF8_GETCHR(u8"誤"): return 552;
        case UTF8_GETCHR(u8"計"): return 553;
        case UTF8_GETCHR(u8"線"): return 554;
        case UTF8_GETCHR(u8"於"): return 555;
        case UTF8_GETCHR(u8"備"): return 556;
        case UTF8_GETCHR(u8"餓"): return 557;
        case UTF8_GETCHR(u8"鮭"): return 558;
        case UTF8_GETCHR(u8"槍"): return 559;
        case UTF8_GETCHR(u8"煙"): return 560;
        case UTF8_GETCHR(u8"薪"): return 561;
        case UTF8_GETCHR(u8"澄"): return 562;
        case UTF8_GETCHR(u8"礎"): return 563;
        case UTF8_GETCHR(u8"売"): return 564;
        case UTF8_GETCHR(u8"汐"): return 565;
        case UTF8_GETCHR(u8"諜"): return 566;
        case UTF8_GETCHR(u8"伝"): return 567;
        case UTF8_GETCHR(u8"寅"): return 568;
        case UTF8_GETCHR(u8"衷"): return 569;
        case UTF8_GETCHR(u8"欽"): return 570;
        case UTF8_GETCHR(u8"濡"): return 571;
        case UTF8_GETCHR(u8"椙"): return 572;
        case UTF8_GETCHR(u8"淑"): return 573;
        case UTF8_GETCHR(u8"掃"): return 574;
        case UTF8_GETCHR(u8"憲"): return 575;
        case UTF8_GETCHR(u8"檀"): return 576;
        case UTF8_GETCHR(u8"暇"): return 577;
        case UTF8_GETCHR(u8"塗"): return 578;
        case UTF8_GETCHR(u8"間"): return 579;
        case UTF8_GETCHR(u8"薦"): return 580;
        case UTF8_GETCHR(u8"髪"): return 581;
        case UTF8_GETCHR(u8"苅"): return 582;
        case UTF8_GETCHR(u8"練"): return 583;
        case UTF8_GETCHR(u8"殉"): return 584;
        case UTF8_GETCHR(u8"嶋"): return 585;
        case UTF8_GETCHR(u8"塁"): return 586;
        case UTF8_GETCHR(u8"祐"): return 587;
        case UTF8_GETCHR(u8"芋"): return 588;
        case UTF8_GETCHR(u8"乾"): return 589;
        case UTF8_GETCHR(u8"獅"): return 590;
        case UTF8_GETCHR(u8"損"): return 591;
        case UTF8_GETCHR(u8"題"): return 592;
        case UTF8_GETCHR(u8"憎"): return 593;
        case UTF8_GETCHR(u8"渇"): return 594;
        case UTF8_GETCHR(u8"棟"): return 595;
        case UTF8_GETCHR(u8"時"): return 596;
        case UTF8_GETCHR(u8"鶴"): return 597;
        case UTF8_GETCHR(u8"稽"): return 598;
        case UTF8_GETCHR(u8"則"): return 599;
        case UTF8_GETCHR(u8"鉱"): return 600;
        case UTF8_GETCHR(u8"従"): return 601;
        case UTF8_GETCHR(u8"圭"): return 602;
        case UTF8_GETCHR(u8"杵"): return 603;
        case UTF8_GETCHR(u8"償"): return 604;
        case UTF8_GETCHR(u8"採"): return 605;
        case UTF8_GETCHR(u8"継"): return 606;
        case UTF8_GETCHR(u8"別"): return 607;
        case UTF8_GETCHR(u8"細"): return 608;
        case UTF8_GETCHR(u8"涯"): return 609;
        case UTF8_GETCHR(u8"蒼"): return 610;
        case UTF8_GETCHR(u8"穂"): return 611;
        case UTF8_GETCHR(u8"囲"): return 612;
        case UTF8_GETCHR(u8"彰"): return 613;
        case UTF8_GETCHR(u8"肖"): return 614;
        case UTF8_GETCHR(u8"笘"): return 615;
        case UTF8_GETCHR(u8"謝"): return 616;
        case UTF8_GETCHR(u8"館"): return 617;
        case UTF8_GETCHR(u8"舗"): return 618;
        case UTF8_GETCHR(u8"婿"): return 619;
        case UTF8_GETCHR(u8"釘"): return 620;
        case UTF8_GETCHR(u8"転"): return 621;
        case UTF8_GETCHR(u8"鐘"): return 622;
        case UTF8_GETCHR(u8"師"): return 623;
        case UTF8_GETCHR(u8"隅"): return 624;
        case UTF8_GETCHR(u8"萩"): return 625;
        case UTF8_GETCHR(u8"叱"): return 626;
        case UTF8_GETCHR(u8"堀"): return 627;
        case UTF8_GETCHR(u8"義"): return 628;
        case UTF8_GETCHR(u8"繊"): return 629;
        case UTF8_GETCHR(u8"柿"): return 630;
        case UTF8_GETCHR(u8"惇"): return 631;
        case UTF8_GETCHR(u8"証"): return 632;
        case UTF8_GETCHR(u8"鄒"): return 633;
        case UTF8_GETCHR(u8"惚"): return 634;
        case UTF8_GETCHR(u8"昔"): return 635;
        case UTF8_GETCHR(u8"椿"): return 636;
        case UTF8_GETCHR(u8"皿"): return 637;
        case UTF8_GETCHR(u8"錬"): return 638;
        case UTF8_GETCHR(u8"誇"): return 639;
        case UTF8_GETCHR(u8"寧"): return 640;
        case UTF8_GETCHR(u8"荻"): return 641;
        case UTF8_GETCHR(u8"札"): return 642;
        case UTF8_GETCHR(u8"縦"): return 643;
        case UTF8_GETCHR(u8"娯"): return 644;
        case UTF8_GETCHR(u8"詞"): return 645;
        case UTF8_GETCHR(u8"鴎"): return 646;
        case UTF8_GETCHR(u8"団"): return 647;
        case UTF8_GETCHR(u8"択"): return 648;
        case UTF8_GETCHR(u8"該"): return 649;
        case UTF8_GETCHR(u8"紋"): return 650;
        case UTF8_GETCHR(u8"囚"): return 651;
        case UTF8_GETCHR(u8"惣"): return 652;
        case UTF8_GETCHR(u8"膠"): return 653;
        case UTF8_GETCHR(u8"鎮"): return 654;
        case UTF8_GETCHR(u8"尹"): return 655;
        case UTF8_GETCHR(u8"樺"): return 656;
        case UTF8_GETCHR(u8"蔭"): return 657;
        case UTF8_GETCHR(u8"衆"): return 658;
        case UTF8_GETCHR(u8"噂"): return 659;
        case UTF8_GETCHR(u8"風"): return 660;
        case UTF8_GETCHR(u8"雇"): return 661;
        case UTF8_GETCHR(u8"沈"): return 662;
        case UTF8_GETCHR(u8"況"): return 663;
        case UTF8_GETCHR(u8"狸"): return 664;
        case UTF8_GETCHR(u8"複"): return 665;
        case UTF8_GETCHR(u8"鋳"): return 666;
        case UTF8_GETCHR(u8"篠"): return 667;
        case UTF8_GETCHR(u8"絵"): return 668;
        case UTF8_GETCHR(u8"謄"): return 669;
        case UTF8_GETCHR(u8"張"): return 670;
        case UTF8_GETCHR(u8"褒"): return 671;
        case UTF8_GETCHR(u8"燈"): return 672;
        case UTF8_GETCHR(u8"倣"): return 673;
        case UTF8_GETCHR(u8"豊"): return 674;
        case UTF8_GETCHR(u8"遅"): return 675;
        case UTF8_GETCHR(u8"佐"): return 676;
        case UTF8_GETCHR(u8"響"): return 677;
        case UTF8_GETCHR(u8"僕"): return 678;
        case UTF8_GETCHR(u8"幹"): return 679;
        case UTF8_GETCHR(u8"卒"): return 680;
        case UTF8_GETCHR(u8"認"): return 681;
        case UTF8_GETCHR(u8"極"): return 682;
        case UTF8_GETCHR(u8"樹"): return 683;
        case UTF8_GETCHR(u8"渚"): return 684;
        case UTF8_GETCHR(u8"窒"): return 685;
        case UTF8_GETCHR(u8"芭"): return 686;
        case UTF8_GETCHR(u8"賃"): return 687;
        case UTF8_GETCHR(u8"翫"): return 688;
        case UTF8_GETCHR(u8"萬"): return 689;
        case UTF8_GETCHR(u8"紹"): return 690;
        case UTF8_GETCHR(u8"飯"): return 691;
        case UTF8_GETCHR(u8"篭"): return 692;
        case UTF8_GETCHR(u8"島"): return 693;
        case UTF8_GETCHR(u8"鱒"): return 694;
        case UTF8_GETCHR(u8"審"): return 695;
        case UTF8_GETCHR(u8"講"): return 696;
        case UTF8_GETCHR(u8"粟"): return 697;
        case UTF8_GETCHR(u8"蝦"): return 698;
        case UTF8_GETCHR(u8"勲"): return 699;
        case UTF8_GETCHR(u8"諏"): return 700;
        case UTF8_GETCHR(u8"脈"): return 701;
        case UTF8_GETCHR(u8"監"): return 702;
        case UTF8_GETCHR(u8"長"): return 703;
        case UTF8_GETCHR(u8"峙"): return 704;
        case UTF8_GETCHR(u8"筆"): return 705;
        case UTF8_GETCHR(u8"鷺"): return 706;
        case UTF8_GETCHR(u8"宵"): return 707;
        case UTF8_GETCHR(u8"靖"): return 708;
        case UTF8_GETCHR(u8"臓"): return 709;
        case UTF8_GETCHR(u8"粧"): return 710;
        case UTF8_GETCHR(u8"悪"): return 711;
        case UTF8_GETCHR(u8"墳"): return 712;
        case UTF8_GETCHR(u8"紡"): return 713;
        case UTF8_GETCHR(u8"餅"): return 714;
        case UTF8_GETCHR(u8"灘"): return 715;
        case UTF8_GETCHR(u8"峠"): return 716;
        case UTF8_GETCHR(u8"済"): return 717;
        case UTF8_GETCHR(u8"栄"): return 718;
        case UTF8_GETCHR(u8"盧"): return 719;
        case UTF8_GETCHR(u8"県"): return 720;
        case UTF8_GETCHR(u8"閉"): return 721;
        case UTF8_GETCHR(u8"喧"): return 722;
        case UTF8_GETCHR(u8"娠"): return 723;
        case UTF8_GETCHR(u8"卿"): return 724;
        case UTF8_GETCHR(u8"蓼"): return 725;
        case UTF8_GETCHR(u8"猟"): return 726;
        case UTF8_GETCHR(u8"嗣"): return 727;
        case UTF8_GETCHR(u8"貧"): return 728;
        case UTF8_GETCHR(u8"軌"): return 729;
        case UTF8_GETCHR(u8"偕"): return 730;
        case UTF8_GETCHR(u8"責"): return 731;
        case UTF8_GETCHR(u8"瑠"): return 732;
        case UTF8_GETCHR(u8"糧"): return 733;
        case UTF8_GETCHR(u8"妖"): return 734;
        case UTF8_GETCHR(u8"雰"): return 735;
        case UTF8_GETCHR(u8"竜"): return 736;
        case UTF8_GETCHR(u8"雑"): return 737;
        case UTF8_GETCHR(u8"勝"): return 738;
        case UTF8_GETCHR(u8"鍵"): return 739;
        case UTF8_GETCHR(u8"擬"): return 740;
        case UTF8_GETCHR(u8"訟"): return 741;
        case UTF8_GETCHR(u8"壷"): return 742;
        case UTF8_GETCHR(u8"嶽"): return 743;
        case UTF8_GETCHR(u8"倶"): return 744;
        case UTF8_GETCHR(u8"怠"): return 745;
        case UTF8_GETCHR(u8"黒"): return 746;
        case UTF8_GETCHR(u8"組"): return 747;
        case UTF8_GETCHR(u8"貨"): return 748;
        case UTF8_GETCHR(u8"倭"): return 749;
        case UTF8_GETCHR(u8"壊"): return 750;
        case UTF8_GETCHR(u8"緒"): return 751;
        case UTF8_GETCHR(u8"塙"): return 752;
        case UTF8_GETCHR(u8"療"): return 753;
        case UTF8_GETCHR(u8"斬"): return 754;
        case UTF8_GETCHR(u8"輪"): return 755;
        case UTF8_GETCHR(u8"曇"): return 756;
        case UTF8_GETCHR(u8"喚"): return 757;
        case UTF8_GETCHR(u8"迭"): return 758;
        case UTF8_GETCHR(u8"惰"): return 759;
        case UTF8_GETCHR(u8"託"): return 760;
        case UTF8_GETCHR(u8"薩"): return 761;
        case UTF8_GETCHR(u8"妃"): return 762;
        case UTF8_GETCHR(u8"蔦"): return 763;
        case UTF8_GETCHR(u8"絹"): return 764;
        case UTF8_GETCHR(u8"厳"): return 765;
        case UTF8_GETCHR(u8"搾"): return 766;
        case UTF8_GETCHR(u8"譲"): return 767;
        case UTF8_GETCHR(u8"確"): return 768;
        case UTF8_GETCHR(u8"視"): return 769;
        case UTF8_GETCHR(u8"暉"): return 770;
        case UTF8_GETCHR(u8"犠"): return 771;
        case UTF8_GETCHR(u8"囃"): return 772;
        case UTF8_GETCHR(u8"訃"): return 773;
        case UTF8_GETCHR(u8"閲"): return 774;
        case UTF8_GETCHR(u8"萱"): return 775;
        case UTF8_GETCHR(u8"煩"): return 776;
        case UTF8_GETCHR(u8"純"): return 777;
        case UTF8_GETCHR(u8"巻"): return 778;
        case UTF8_GETCHR(u8"請"): return 779;
        case UTF8_GETCHR(u8"歯"): return 780;
        case UTF8_GETCHR(u8"術"): return 781;
        case UTF8_GETCHR(u8"鈴"): return 782;
        case UTF8_GETCHR(u8"礒"): return 783;
        case UTF8_GETCHR(u8"債"): return 784;
        case UTF8_GETCHR(u8"鉛"): return 785;
        case UTF8_GETCHR(u8"喪"): return 786;
        case UTF8_GETCHR(u8"禎"): return 787;
        case UTF8_GETCHR(u8"鍋"): return 788;
        case UTF8_GETCHR(u8"禍"): return 789;
        case UTF8_GETCHR(u8"贋"): return 790;
        case UTF8_GETCHR(u8"縫"): return 791;
        case UTF8_GETCHR(u8"焼"): return 792;
        case UTF8_GETCHR(u8"橿"): return 793;
        case UTF8_GETCHR(u8"憶"): return 794;
        case UTF8_GETCHR(u8"腸"): return 795;
        case UTF8_GETCHR(u8"拭"): return 796;
        case UTF8_GETCHR(u8"駆"): return 797;
        case UTF8_GETCHR(u8"柚"): return 798;
        case UTF8_GETCHR(u8"亀"): return 799;
        case UTF8_GETCHR(u8"劇"): return 800;
        case UTF8_GETCHR(u8"麓"): return 801;
        case UTF8_GETCHR(u8"効"): return 802;
        case UTF8_GETCHR(u8"測"): return 803;
        case UTF8_GETCHR(u8"綴"): return 804;
        case UTF8_GETCHR(u8"恣"): return 805;
        case UTF8_GETCHR(u8"詩"): return 806;
        case UTF8_GETCHR(u8"販"): return 807;
        case UTF8_GETCHR(u8"適"): return 808;
        case UTF8_GETCHR(u8"葦"): return 809;
        case UTF8_GETCHR(u8"靴"): return 810;
        case UTF8_GETCHR(u8"艦"): return 811;
        case UTF8_GETCHR(u8"養"): return 812;
        case UTF8_GETCHR(u8"進"): return 813;
        case UTF8_GETCHR(u8"過"): return 814;
        case UTF8_GETCHR(u8"郵"): return 815;
        case UTF8_GETCHR(u8"塚"): return 816;
        case UTF8_GETCHR(u8"丼"): return 817;
        case UTF8_GETCHR(u8"咲"): return 818;
        case UTF8_GETCHR(u8"穏"): return 819;
        case UTF8_GETCHR(u8"預"): return 820;
        case UTF8_GETCHR(u8"綜"): return 821;
        case UTF8_GETCHR(u8"冴"): return 822;
        case UTF8_GETCHR(u8"奪"): return 823;
        case UTF8_GETCHR(u8"優"): return 824;
        case UTF8_GETCHR(u8"締"): return 825;
        case UTF8_GETCHR(u8"濁"): return 826;
        case UTF8_GETCHR(u8"湊"): return 827;
        case UTF8_GETCHR(u8"針"): return 828;
        case UTF8_GETCHR(u8"頭"): return 829;
        case UTF8_GETCHR(u8"陛"): return 830;
        case UTF8_GETCHR(u8"満"): return 831;
        case UTF8_GETCHR(u8"機"): return 832;
        case UTF8_GETCHR(u8"軟"): return 833;
        case UTF8_GETCHR(u8"慮"): return 834;
        case UTF8_GETCHR(u8"繭"): return 835;
        case UTF8_GETCHR(u8"凸"): return 836;
        case UTF8_GETCHR(u8"撫"): return 837;
        case UTF8_GETCHR(u8"窓"): return 838;
        case UTF8_GETCHR(u8"傘"): return 839;
        case UTF8_GETCHR(u8"砕"): return 840;
        case UTF8_GETCHR(u8"亘"): return 841;
        case UTF8_GETCHR(u8"藩"): return 842;
        case UTF8_GETCHR(u8"凧"): return 843;
        case UTF8_GETCHR(u8"煕"): return 844;
        case UTF8_GETCHR(u8"渉"): return 845;
        case UTF8_GETCHR(u8"淵"): return 846;
        case UTF8_GETCHR(u8"弾"): return 847;
        case UTF8_GETCHR(u8"傷"): return 848;
        case UTF8_GETCHR(u8"誕"): return 849;
        case UTF8_GETCHR(u8"旬"): return 850;
        case UTF8_GETCHR(u8"貴"): return 851;
        case UTF8_GETCHR(u8"戦"): return 852;
        case UTF8_GETCHR(u8"阜"): return 853;
        case UTF8_GETCHR(u8"篆"): return 854;
        case UTF8_GETCHR(u8"隻"): return 855;
        case UTF8_GETCHR(u8"鳥"): return 856;
        case UTF8_GETCHR(u8"編"): return 857;
        case UTF8_GETCHR(u8"迦"): return 858;
        case UTF8_GETCHR(u8"酪"): return 859;
        case UTF8_GETCHR(u8"畏"): return 860;
        case UTF8_GETCHR(u8"恵"): return 861;
        case UTF8_GETCHR(u8"宮"): return 862;
        case UTF8_GETCHR(u8"薔"): return 863;
        case UTF8_GETCHR(u8"梶"): return 864;
        case UTF8_GETCHR(u8"嫉"): return 865;
        case UTF8_GETCHR(u8"弔"): return 866;
        case UTF8_GETCHR(u8"弘"): return 867;
        case UTF8_GETCHR(u8"菓"): return 868;
        case UTF8_GETCHR(u8"懸"): return 869;
        case UTF8_GETCHR(u8"徳"): return 870;
        case UTF8_GETCHR(u8"揃"): return 871;
        case UTF8_GETCHR(u8"乗"): return 872;
        case UTF8_GETCHR(u8"粕"): return 873;
        case UTF8_GETCHR(u8"叢"): return 874;
        case UTF8_GETCHR(u8"規"): return 875;
        case UTF8_GETCHR(u8"蕨"): return 876;
        case UTF8_GETCHR(u8"擁"): return 877;
        case UTF8_GETCHR(u8"坂"): return 878;
        case UTF8_GETCHR(u8"韮"): return 879;
        case UTF8_GETCHR(u8"踊"): return 880;
        case UTF8_GETCHR(u8"薮"): return 881;
        case UTF8_GETCHR(u8"膳"): return 882;
        case UTF8_GETCHR(u8"偵"): return 883;
        case UTF8_GETCHR(u8"庶"): return 884;
        case UTF8_GETCHR(u8"達"): return 885;
        case UTF8_GETCHR(u8"崔"): return 886;
        case UTF8_GETCHR(u8"誠"): return 887;
        case UTF8_GETCHR(u8"須"): return 888;
        case UTF8_GETCHR(u8"懐"): return 889;
        case UTF8_GETCHR(u8"猷"): return 890;
        case UTF8_GETCHR(u8"棄"): return 891;
        case UTF8_GETCHR(u8"躍"): return 892;
        case UTF8_GETCHR(u8"芙"): return 893;
        case UTF8_GETCHR(u8"喫"): return 894;
        case UTF8_GETCHR(u8"晩"): return 895;
        case UTF8_GETCHR(u8"報"): return 896;
        case UTF8_GETCHR(u8"帳"): return 897;
        case UTF8_GETCHR(u8"塩"): return 898;
        case UTF8_GETCHR(u8"穫"): return 899;
        case UTF8_GETCHR(u8"痢"): return 900;
        case UTF8_GETCHR(u8"決"): return 901;
        case UTF8_GETCHR(u8"缶"): return 902;
        case UTF8_GETCHR(u8"稔"): return 903;
        case UTF8_GETCHR(u8"隠"): return 904;
        case UTF8_GETCHR(u8"顧"): return 905;
        case UTF8_GETCHR(u8"簑"): return 906;
        case UTF8_GETCHR(u8"冨"): return 907;
        case UTF8_GETCHR(u8"箏"): return 908;
        case UTF8_GETCHR(u8"菅"): return 909;
        case UTF8_GETCHR(u8"霊"): return 910;
        case UTF8_GETCHR(u8"樽"): return 911;
        case UTF8_GETCHR(u8"桝"): return 912;
        case UTF8_GETCHR(u8"釜"): return 913;
        case UTF8_GETCHR(u8"裏"): return 914;
        case UTF8_GETCHR(u8"係"): return 915;
        case UTF8_GETCHR(u8"緊"): return 916;
        case UTF8_GETCHR(u8"蓮"): return 917;
        case UTF8_GETCHR(u8"皓"): return 918;
        case UTF8_GETCHR(u8"廣"): return 919;
        case UTF8_GETCHR(u8"仕"): return 920;
        case UTF8_GETCHR(u8"渦"): return 921;
        case UTF8_GETCHR(u8"赳"): return 922;
        case UTF8_GETCHR(u8"節"): return 923;
        case UTF8_GETCHR(u8"掲"): return 924;
        case UTF8_GETCHR(u8"鈍"): return 925;
        case UTF8_GETCHR(u8"薫"): return 926;
        case UTF8_GETCHR(u8"鱗"): return 927;
        case UTF8_GETCHR(u8"嶺"): return 928;
        case UTF8_GETCHR(u8"鍛"): return 929;
        case UTF8_GETCHR(u8"沼"): return 930;
        case UTF8_GETCHR(u8"斉"): return 931;
        case UTF8_GETCHR(u8"涼"): return 932;
        case UTF8_GETCHR(u8"賢"): return 933;
        case UTF8_GETCHR(u8"勢"): return 934;
        case UTF8_GETCHR(u8"圏"): return 935;
        case UTF8_GETCHR(u8"場"): return 936;
        case UTF8_GETCHR(u8"趙"): return 937;
        case UTF8_GETCHR(u8"栓"): return 938;
        case UTF8_GETCHR(u8"飛"): return 939;
        case UTF8_GETCHR(u8"湛"): return 940;
        case UTF8_GETCHR(u8"弛"): return 941;
        case UTF8_GETCHR(u8"瀕"): return 942;
        case UTF8_GETCHR(u8"讃"): return 943;
        case UTF8_GETCHR(u8"紀"): return 944;
        case UTF8_GETCHR(u8"畝"): return 945;
        case UTF8_GETCHR(u8"続"): return 946;
        case UTF8_GETCHR(u8"絢"): return 947;
        case UTF8_GETCHR(u8"鏡"): return 948;
        case UTF8_GETCHR(u8"処"): return 949;
        case UTF8_GETCHR(u8"龍"): return 950;
        case UTF8_GETCHR(u8"尻"): return 951;
        case UTF8_GETCHR(u8"榛"): return 952;
        case UTF8_GETCHR(u8"頃"): return 953;
        case UTF8_GETCHR(u8"駐"): return 954;
        case UTF8_GETCHR(u8"皐"): return 955;
        case UTF8_GETCHR(u8"連"): return 956;
        case UTF8_GETCHR(u8"梓"): return 957;
        case UTF8_GETCHR(u8"個"): return 958;
        case UTF8_GETCHR(u8"為"): return 959;
        case UTF8_GETCHR(u8"瞳"): return 960;
        case UTF8_GETCHR(u8"袴"): return 961;
        case UTF8_GETCHR(u8"虐"): return 962;
        case UTF8_GETCHR(u8"彭"): return 963;
        case UTF8_GETCHR(u8"町"): return 964;
        case UTF8_GETCHR(u8"覚"): return 965;
        case UTF8_GETCHR(u8"黙"): return 966;
        case UTF8_GETCHR(u8"畿"): return 967;
        case UTF8_GETCHR(u8"親"): return 968;
        case UTF8_GETCHR(u8"劾"): return 969;
        case UTF8_GETCHR(u8"腕"): return 970;
        case UTF8_GETCHR(u8"阪"): return 971;
        case UTF8_GETCHR(u8"検"): return 972;
        case UTF8_GETCHR(u8"倹"): return 973;
        case UTF8_GETCHR(u8"辰"): return 974;
        case UTF8_GETCHR(u8"併"): return 975;
        case UTF8_GETCHR(u8"変"): return 976;
        case UTF8_GETCHR(u8"儀"): return 977;
        case UTF8_GETCHR(u8"窪"): return 978;
        case UTF8_GETCHR(u8"亜"): return 979;
        case UTF8_GETCHR(u8"匡"): return 980;
        case UTF8_GETCHR(u8"櫛"): return 981;
        case UTF8_GETCHR(u8"絡"): return 982;
        case UTF8_GETCHR(u8"給"): return 983;
        case UTF8_GETCHR(u8"辻"): return 984;
        case UTF8_GETCHR(u8"驚"): return 985;
        case UTF8_GETCHR(u8"瀬"): return 986;
        case UTF8_GETCHR(u8"雛"): return 987;
        case UTF8_GETCHR(u8"諾"): return 988;
        case UTF8_GETCHR(u8"匂"): return 989;
        case UTF8_GETCHR(u8"彪"): return 990;
        case UTF8_GETCHR(u8"嗜"): return 991;
        case UTF8_GETCHR(u8"鯛"): return 992;
        case UTF8_GETCHR(u8"肇"): return 993;
        case UTF8_GETCHR(u8"竪"): return 994;
        case UTF8_GETCHR(u8"軍"): return 995;
        case UTF8_GETCHR(u8"選"): return 996;
        case UTF8_GETCHR(u8"琵"): return 997;
        case UTF8_GETCHR(u8"渥"): return 998;
        case UTF8_GETCHR(u8"岬"): return 999;
        case UTF8_GETCHR(u8"軒"): return 1000;
        case UTF8_GETCHR(u8"窯"): return 1001;
        case UTF8_GETCHR(u8"滝"): return 1002;
        case UTF8_GETCHR(u8"蕃"): return 1003;
        case UTF8_GETCHR(u8"難"): return 1004;
        case UTF8_GETCHR(u8"読"): return 1005;
        case UTF8_GETCHR(u8"鉢"): return 1006;
        case UTF8_GETCHR(u8"渋"): return 1007;
        case UTF8_GETCHR(u8"馨"): return 1008;
        case UTF8_GETCHR(u8"討"): return 1009;
        case UTF8_GETCHR(u8"賭"): return 1010;
        case UTF8_GETCHR(u8"無"): return 1011;
        case UTF8_GETCHR(u8"喬"): return 1012;
        case UTF8_GETCHR(u8"庸"): return 1013;
        case UTF8_GETCHR(u8"儲"): return 1014;
        case UTF8_GETCHR(u8"挟"): return 1015;
        case UTF8_GETCHR(u8"銕"): return 1016;
        case UTF8_GETCHR(u8"腫"): return 1017;
        case UTF8_GETCHR(u8"陳"): return 1018;
        case UTF8_GETCHR(u8"慣"): return 1019;
        case UTF8_GETCHR(u8"聖"): return 1020;
        case UTF8_GETCHR(u8"叩"): return 1021;
        case UTF8_GETCHR(u8"価"): return 1022;
        case UTF8_GETCHR(u8"菱"): return 1023;
        case UTF8_GETCHR(u8"項"): return 1024;
        case UTF8_GETCHR(u8"稚"): return 1025;
        case UTF8_GETCHR(u8"殴"): return 1026;
        case UTF8_GETCHR(u8"痴"): return 1027;
        case UTF8_GETCHR(u8"恥"): return 1028;
        case UTF8_GETCHR(u8"仮"): return 1029;
        case UTF8_GETCHR(u8"脩"): return 1031;
        case UTF8_GETCHR(u8"詰"): return 1032;
        case UTF8_GETCHR(u8"実"): return 1033;
        case UTF8_GETCHR(u8"悼"): return 1034;
        case UTF8_GETCHR(u8"檄"): return 1035;
        case UTF8_GETCHR(u8"額"): return 1036;
        case UTF8_GETCHR(u8"岡"): return 1037;
        case UTF8_GETCHR(u8"聞"): return 1038;
        case UTF8_GETCHR(u8"笏"): return 1039;
        case UTF8_GETCHR(u8"護"): return 1040;
        case UTF8_GETCHR(u8"魯"): return 1041;
        case UTF8_GETCHR(u8"蜷"): return 1042;
        case UTF8_GETCHR(u8"寛"): return 1043;
        case UTF8_GETCHR(u8"臼"): return 1044;
        case UTF8_GETCHR(u8"埴"): return 1045;
        case UTF8_GETCHR(u8"値"): return 1046;
        case UTF8_GETCHR(u8"網"): return 1047;
        case UTF8_GETCHR(u8"杖"): return 1048;
        case UTF8_GETCHR(u8"贅"): return 1049;
        case UTF8_GETCHR(u8"鴈"): return 1050;
        case UTF8_GETCHR(u8"藁"): return 1051;
        case UTF8_GETCHR(u8"還"): return 1052;
        case UTF8_GETCHR(u8"砲"): return 1053;
        case UTF8_GETCHR(u8"階"): return 1054;
        case UTF8_GETCHR(u8"甫"): return 1055;
        case UTF8_GETCHR(u8"雲"): return 1056;
        case UTF8_GETCHR(u8"銀"): return 1057;
        case UTF8_GETCHR(u8"烏"): return 1058;
        case UTF8_GETCHR(u8"伽"): return 1059;
        case UTF8_GETCHR(u8"憂"): return 1060;
        case UTF8_GETCHR(u8"襲"): return 1061;
        case UTF8_GETCHR(u8"賠"): return 1062;
        case UTF8_GETCHR(u8"揚"): return 1063;
        case UTF8_GETCHR(u8"顔"): return 1064;
        case UTF8_GETCHR(u8"堰"): return 1065;
        case UTF8_GETCHR(u8"秩"): return 1066;
        case UTF8_GETCHR(u8"舵"): return 1067;
        case UTF8_GETCHR(u8"呉"): return 1068;
        case UTF8_GETCHR(u8"澤"): return 1069;
        case UTF8_GETCHR(u8"東"): return 1070;
        case UTF8_GETCHR(u8"侮"): return 1071;
        case UTF8_GETCHR(u8"撮"): return 1072;
        case UTF8_GETCHR(u8"湯"): return 1073;
        case UTF8_GETCHR(u8"諮"): return 1074;
        case UTF8_GETCHR(u8"復"): return 1075;
        case UTF8_GETCHR(u8"負"): return 1076;
        case UTF8_GETCHR(u8"粛"): return 1077;
        case UTF8_GETCHR(u8"牝"): return 1078;
        case UTF8_GETCHR(u8"帰"): return 1079;
        case UTF8_GETCHR(u8"門"): return 1080;
        case UTF8_GETCHR(u8"課"): return 1081;
        case UTF8_GETCHR(u8"鷹"): return 1082;
        case UTF8_GETCHR(u8"齢"): return 1083;
        case UTF8_GETCHR(u8"暢"): return 1084;
        case UTF8_GETCHR(u8"歴"): return 1085;
        case UTF8_GETCHR(u8"滅"): return 1086;
        case UTF8_GETCHR(u8"拘"): return 1087;
        case UTF8_GETCHR(u8"淳"): return 1088;
        case UTF8_GETCHR(u8"蒔"): return 1089;
        case UTF8_GETCHR(u8"寡"): return 1090;
        case UTF8_GETCHR(u8"衝"): return 1091;
        case UTF8_GETCHR(u8"扱"): return 1092;
        case UTF8_GETCHR(u8"厄"): return 1093;
        case UTF8_GETCHR(u8"砦"): return 1094;
        case UTF8_GETCHR(u8"孫"): return 1095;
        case UTF8_GETCHR(u8"駅"): return 1096;
        case UTF8_GETCHR(u8"問"): return 1097;
        case UTF8_GETCHR(u8"挿"): return 1098;
        case UTF8_GETCHR(u8"質"): return 1099;
        case UTF8_GETCHR(u8"釧"): return 1100;
        case UTF8_GETCHR(u8"檜"): return 1101;
        case UTF8_GETCHR(u8"彫"): return 1102;
        case UTF8_GETCHR(u8"脳"): return 1103;
        case UTF8_GETCHR(u8"噴"): return 1104;
        case UTF8_GETCHR(u8"憩"): return 1105;
        case UTF8_GETCHR(u8"滞"): return 1106;
        case UTF8_GETCHR(u8"興"): return 1107;
        case UTF8_GETCHR(u8"拙"): return 1108;
        case UTF8_GETCHR(u8"徹"): return 1109;
        case UTF8_GETCHR(u8"膚"): return 1110;
        case UTF8_GETCHR(u8"祈"): return 1111;
        case UTF8_GETCHR(u8"蛍"): return 1112;
        case UTF8_GETCHR(u8"哺"): return 1113;
        case UTF8_GETCHR(u8"銅"): return 1114;
        case UTF8_GETCHR(u8"鉄"): return 1115;
        case UTF8_GETCHR(u8"歳"): return 1116;
        case UTF8_GETCHR(u8"姻"): return 1117;
        case UTF8_GETCHR(u8"関"): return 1118;
        case UTF8_GETCHR(u8"捜"): return 1119;
        case UTF8_GETCHR(u8"輩"): return 1120;
        case UTF8_GETCHR(u8"鮮"): return 1121;
        case UTF8_GETCHR(u8"異"): return 1122;
        case UTF8_GETCHR(u8"狛"): return 1123;
        case UTF8_GETCHR(u8"駄"): return 1124;
        case UTF8_GETCHR(u8"壇"): return 1125;
        case UTF8_GETCHR(u8"酔"): return 1126;
        case UTF8_GETCHR(u8"泣"): return 1127;
        case UTF8_GETCHR(u8"宍"): return 1128;
        case UTF8_GETCHR(u8"遼"): return 1129;
        case UTF8_GETCHR(u8"俸"): return 1130;
        case UTF8_GETCHR(u8"諦"): return 1131;
        case UTF8_GETCHR(u8"剰"): return 1132;
        case UTF8_GETCHR(u8"廟"): return 1133;
        case UTF8_GETCHR(u8"緻"): return 1134;
        case UTF8_GETCHR(u8"論"): return 1135;
        case UTF8_GETCHR(u8"貸"): return 1136;
        case UTF8_GETCHR(u8"鎌"): return 1137;
        case UTF8_GETCHR(u8"億"): return 1138;
        case UTF8_GETCHR(u8"赦"): return 1139;
        case UTF8_GETCHR(u8"鳩"): return 1140;
        case UTF8_GETCHR(u8"聴"): return 1141;
        case UTF8_GETCHR(u8"調"): return 1142;
        case UTF8_GETCHR(u8"緩"): return 1143;
        case UTF8_GETCHR(u8"員"): return 1144;
        case UTF8_GETCHR(u8"傑"): return 1145;
        case UTF8_GETCHR(u8"毎"): return 1146;
        case UTF8_GETCHR(u8"鵜"): return 1147;
        case UTF8_GETCHR(u8"隼"): return 1148;
        case UTF8_GETCHR(u8"魚"): return 1149;
        case UTF8_GETCHR(u8"殺"): return 1150;
        case UTF8_GETCHR(u8"昇"): return 1151;
        case UTF8_GETCHR(u8"頒"): return 1152;
        case UTF8_GETCHR(u8"幣"): return 1153;
        case UTF8_GETCHR(u8"綿"): return 1154;
        case UTF8_GETCHR(u8"槙"): return 1155;
        case UTF8_GETCHR(u8"発"): return 1156;
        case UTF8_GETCHR(u8"矢"): return 1157;
        case UTF8_GETCHR(u8"見"): return 1158;
        case UTF8_GETCHR(u8"佑"): return 1159;
        case UTF8_GETCHR(u8"曙"): return 1160;
        case UTF8_GETCHR(u8"墜"): return 1161;
        case UTF8_GETCHR(u8"糸"): return 1162;
        case UTF8_GETCHR(u8"払"): return 1163;
        case UTF8_GETCHR(u8"鎖"): return 1164;
        case UTF8_GETCHR(u8"硯"): return 1165;
        case UTF8_GETCHR(u8"収"): return 1166;
        case UTF8_GETCHR(u8"蕉"): return 1167;
        case UTF8_GETCHR(u8"頼"): return 1168;
        case UTF8_GETCHR(u8"悌"): return 1169;
        case UTF8_GETCHR(u8"懲"): return 1170;
        case UTF8_GETCHR(u8"誘"): return 1171;
        case UTF8_GETCHR(u8"乞"): return 1172;
        case UTF8_GETCHR(u8"娼"): return 1173;
        case UTF8_GETCHR(u8"奨"): return 1174;
        case UTF8_GETCHR(u8"曜"): return 1175;
        case UTF8_GETCHR(u8"尭"): return 1176;
        case UTF8_GETCHR(u8"貢"): return 1177;
        case UTF8_GETCHR(u8"財"): return 1178;
        case UTF8_GETCHR(u8"酉"): return 1179;
        case UTF8_GETCHR(u8"拠"): return 1180;
        case UTF8_GETCHR(u8"鶏"): return 1181;
        case UTF8_GETCHR(u8"寮"): return 1182;
        case UTF8_GETCHR(u8"尋"): return 1183;
        case UTF8_GETCHR(u8"罷"): return 1184;
        case UTF8_GETCHR(u8"拝"): return 1185;
        case UTF8_GETCHR(u8"禅"): return 1186;
        case UTF8_GETCHR(u8"鮎"): return 1187;
        case UTF8_GETCHR(u8"贈"): return 1188;
        case UTF8_GETCHR(u8"錫"): return 1189;
        case UTF8_GETCHR(u8"勧"): return 1190;
        case UTF8_GETCHR(u8"楊"): return 1191;
        case UTF8_GETCHR(u8"茜"): return 1192;
        case UTF8_GETCHR(u8"隣"): return 1193;
        case UTF8_GETCHR(u8"嵯"): return 1194;
        case UTF8_GETCHR(u8"郡"): return 1195;
        case UTF8_GETCHR(u8"彬"): return 1196;
        case UTF8_GETCHR(u8"賜"): return 1197;
        case UTF8_GETCHR(u8"鴻"): return 1198;
        case UTF8_GETCHR(u8"猶"): return 1199;
        case UTF8_GETCHR(u8"許"): return 1200;
        case UTF8_GETCHR(u8"鍾"): return 1202;
        case UTF8_GETCHR(u8"換"): return 1203;
        case UTF8_GETCHR(u8"資"): return 1204;
        case UTF8_GETCHR(u8"帥"): return 1205;
        case UTF8_GETCHR(u8"絶"): return 1206;
        case UTF8_GETCHR(u8"鵬"): return 1207;
        case UTF8_GETCHR(u8"諌"): return 1208;
        case UTF8_GETCHR(u8"気"): return 1209;
        case UTF8_GETCHR(u8"刈"): return 1210;
        case UTF8_GETCHR(u8"樋"): return 1211;
        case UTF8_GETCHR(u8"銭"): return 1212;
        case UTF8_GETCHR(u8"習"): return 1213;
        case UTF8_GETCHR(u8"詠"): return 1214;
        case UTF8_GETCHR(u8"伺"): return 1215;
        case UTF8_GETCHR(u8"豚"): return 1216;
        case UTF8_GETCHR(u8"飲"): return 1217;
        case UTF8_GETCHR(u8"臨"): return 1218;
        case UTF8_GETCHR(u8"飢"): return 1219;
        case UTF8_GETCHR(u8"経"): return 1220;
        case UTF8_GETCHR(u8"棺"): return 1221;
        case UTF8_GETCHR(u8"篤"): return 1222;
        case UTF8_GETCHR(u8"縄"): return 1223;
        case UTF8_GETCHR(u8"昭"): return 1224;
        case UTF8_GETCHR(u8"菩"): return 1225;
        case UTF8_GETCHR(u8"増"): return 1226;
        case UTF8_GETCHR(u8"酢"): return 1227;
        case UTF8_GETCHR(u8"跡"): return 1228;
        case UTF8_GETCHR(u8"飼"): return 1229;
        case UTF8_GETCHR(u8"茗"): return 1230;
        case UTF8_GETCHR(u8"餌"): return 1231;
        case UTF8_GETCHR(u8"饗"): return 1232;
        case UTF8_GETCHR(u8"競"): return 1233;
        case UTF8_GETCHR(u8"閑"): return 1234;
        case UTF8_GETCHR(u8"舘"): return 1235;
        case UTF8_GETCHR(u8"腎"): return 1236;
        case UTF8_GETCHR(u8"冤"): return 1237;
        case UTF8_GETCHR(u8"毬"): return 1238;
        case UTF8_GETCHR(u8"緑"): return 1239;
        case UTF8_GETCHR(u8"樟"): return 1240;
        case UTF8_GETCHR(u8"繰"): return 1241;
        case UTF8_GETCHR(u8"箕"): return 1242;
        case UTF8_GETCHR(u8"煥"): return 1243;
        case UTF8_GETCHR(u8"拡"): return 1244;
        case UTF8_GETCHR(u8"脇"): return 1245;
        case UTF8_GETCHR(u8"塾"): return 1246;
        case UTF8_GETCHR(u8"弁"): return 1247;
        case UTF8_GETCHR(u8"漢"): return 1248;
        case UTF8_GETCHR(u8"謹"): return 1249;
        case UTF8_GETCHR(u8"埼"): return 1250;
        case UTF8_GETCHR(u8"漕"): return 1251;
        case UTF8_GETCHR(u8"範"): return 1252;
        case UTF8_GETCHR(u8"槌"): return 1253;
        case UTF8_GETCHR(u8"対"): return 1254;
        case UTF8_GETCHR(u8"夢"): return 1255;
        case UTF8_GETCHR(u8"聯"): return 1256;
        case UTF8_GETCHR(u8"書"): return 1257;
        case UTF8_GETCHR(u8"宕"): return 1258;
        case UTF8_GETCHR(u8"酌"): return 1259;
        case UTF8_GETCHR(u8"絆"): return 1260;
        case UTF8_GETCHR(u8"煎"): return 1261;
        case UTF8_GETCHR(u8"眺"): return 1262;
        case UTF8_GETCHR(u8"閣"): return 1263;
        case UTF8_GETCHR(u8"霧"): return 1264;
        case UTF8_GETCHR(u8"専"): return 1265;
        case UTF8_GETCHR(u8"陣"): return 1266;
        case UTF8_GETCHR(u8"陽"): return 1267;
        case UTF8_GETCHR(u8"壕"): return 1268;
        case UTF8_GETCHR(u8"縁"): return 1269;
        case UTF8_GETCHR(u8"覧"): return 1270;
        case UTF8_GETCHR(u8"鴨"): return 1271;
        case UTF8_GETCHR(u8"標"): return 1272;
        case UTF8_GETCHR(u8"銘"): return 1273;
        case UTF8_GETCHR(u8"側"): return 1274;
        case UTF8_GETCHR(u8"協"): return 1275;
        case UTF8_GETCHR(u8"冥"): return 1276;
        case UTF8_GETCHR(u8"態"): return 1277;
        case UTF8_GETCHR(u8"轍"): return 1278;
        case UTF8_GETCHR(u8"慶"): return 1279;
        case UTF8_GETCHR(u8"脅"): return 1280;
        case UTF8_GETCHR(u8"麗"): return 1281;
        case UTF8_GETCHR(u8"積"): return 1282;
        case UTF8_GETCHR(u8"諭"): return 1283;
        case UTF8_GETCHR(u8"芸"): return 1284;
        case UTF8_GETCHR(u8"強"): return 1285;
        case UTF8_GETCHR(u8"畠"): return 1286;
        case UTF8_GETCHR(u8"亨"): return 1287;
        case UTF8_GETCHR(u8"呑"): return 1288;
        case UTF8_GETCHR(u8"暮"): return 1289;
        case UTF8_GETCHR(u8"癖"): return 1290;
        case UTF8_GETCHR(u8"沢"): return 1291;
        case UTF8_GETCHR(u8"闘"): return 1292;
        case UTF8_GETCHR(u8"桟"): return 1293;
        case UTF8_GETCHR(u8"訴"): return 1294;
        case UTF8_GETCHR(u8"呪"): return 1295;
        case UTF8_GETCHR(u8"俣"): return 1296;
        case UTF8_GETCHR(u8"邑"): return 1297;
        case UTF8_GETCHR(u8"終"): return 1298;
        case UTF8_GETCHR(u8"汚"): return 1299;
        case UTF8_GETCHR(u8"約"): return 1300;
        case UTF8_GETCHR(u8"準"): return 1301;
        case UTF8_GETCHR(u8"総"): return 1302;
        case UTF8_GETCHR(u8"漁"): return 1303;
        case UTF8_GETCHR(u8"舩"): return 1304;
        case UTF8_GETCHR(u8"祇"): return 1305;
        case UTF8_GETCHR(u8"竿"): return 1306;
        case UTF8_GETCHR(u8"応"): return 1307;
        case UTF8_GETCHR(u8"妊"): return 1308;
        case UTF8_GETCHR(u8"詐"): return 1309;
        case UTF8_GETCHR(u8"橋"): return 1310;
        case UTF8_GETCHR(u8"賄"): return 1311;
        case UTF8_GETCHR(u8"貞"): return 1312;
        case UTF8_GETCHR(u8"錠"): return 1313;
        case UTF8_GETCHR(u8"禄"): return 1314;
        case UTF8_GETCHR(u8"粥"): return 1315;
        case UTF8_GETCHR(u8"獣"): return 1316;
        case UTF8_GETCHR(u8"験"): return 1317;
        case UTF8_GETCHR(u8"姉"): return 1318;
        case UTF8_GETCHR(u8"縮"): return 1319;
        case UTF8_GETCHR(u8"開"): return 1320;
        case UTF8_GETCHR(u8"絞"): return 1321;
        case UTF8_GETCHR(u8"卓"): return 1322;
        case UTF8_GETCHR(u8"詔"): return 1323;
        case UTF8_GETCHR(u8"募"): return 1324;
        case UTF8_GETCHR(u8"縛"): return 1325;
        case UTF8_GETCHR(u8"幡"): return 1326;
        case UTF8_GETCHR(u8"旛"): return 1327;
        case UTF8_GETCHR(u8"笹"): return 1328;
        case UTF8_GETCHR(u8"統"): return 1329;
        case UTF8_GETCHR(u8"匿"): return 1330;
        case UTF8_GETCHR(u8"製"): return 1331;
        case UTF8_GETCHR(u8"邸"): return 1332;
        case UTF8_GETCHR(u8"襟"): return 1333;
        case UTF8_GETCHR(u8"銚"): return 1334;
        case UTF8_GETCHR(u8"熱"): return 1335;
        case UTF8_GETCHR(u8"誰"): return 1336;
        case UTF8_GETCHR(u8"軸"): return 1337;
        case UTF8_GETCHR(u8"弊"): return 1338;
        case UTF8_GETCHR(u8"剃"): return 1339;
        case UTF8_GETCHR(u8"昼"): return 1340;
        case UTF8_GETCHR(u8"逗"): return 1341;
        case UTF8_GETCHR(u8"殻"): return 1342;
        case UTF8_GETCHR(u8"韓"): return 1343;
        case UTF8_GETCHR(u8"広"): return 1344;
        case UTF8_GETCHR(u8"湧"): return 1345;
        case UTF8_GETCHR(u8"彗"): return 1346;
        case UTF8_GETCHR(u8"邪"): return 1347;
        case UTF8_GETCHR(u8"欄"): return 1348;
        case UTF8_GETCHR(u8"仏"): return 1349;
        case UTF8_GETCHR(u8"誌"): return 1350;
        case UTF8_GETCHR(u8"苫"): return 1351;
        case UTF8_GETCHR(u8"尉"): return 1352;
        case UTF8_GETCHR(u8"覇"): return 1353;
        case UTF8_GETCHR(u8"荘"): return 1354;
        case UTF8_GETCHR(u8"騎"): return 1355;
        case UTF8_GETCHR(u8"琢"): return 1356;
        case UTF8_GETCHR(u8"傾"): return 1357;
        case UTF8_GETCHR(u8"碁"): return 1358;
        case UTF8_GETCHR(u8"職"): return 1359;
        case UTF8_GETCHR(u8"紘"): return 1360;
        case UTF8_GETCHR(u8"話"): return 1361;
        case UTF8_GETCHR(u8"偲"): return 1362;
        case UTF8_GETCHR(u8"糞"): return 1363;
        case UTF8_GETCHR(u8"鉾"): return 1364;
        case UTF8_GETCHR(u8"撲"): return 1365;
        case UTF8_GETCHR(u8"潟"): return 1366;
        case UTF8_GETCHR(u8"喋"): return 1367;
        case UTF8_GETCHR(u8"辺"): return 1368;
        case UTF8_GETCHR(u8"獄"): return 1369;
        case UTF8_GETCHR(u8"綺"): return 1370;
        case UTF8_GETCHR(u8"種"): return 1371;
        case UTF8_GETCHR(u8"畳"): return 1372;
        case UTF8_GETCHR(u8"摂"): return 1373;
        case UTF8_GETCHR(u8"蛭"): return 1374;
        case UTF8_GETCHR(u8"駒"): return 1375;
        case UTF8_GETCHR(u8"譜"): return 1376;
        case UTF8_GETCHR(u8"塀"): return 1377;
        case UTF8_GETCHR(u8"鑑"): return 1378;
        case UTF8_GETCHR(u8"図"): return 1379;
        case UTF8_GETCHR(u8"媛"): return 1380;
        case UTF8_GETCHR(u8"扉"): return 1381;
        case UTF8_GETCHR(u8"醤"): return 1382;
        case UTF8_GETCHR(u8"紛"): return 1383;
        case UTF8_GETCHR(u8"榊"): return 1384;
        case UTF8_GETCHR(u8"訳"): return 1385;
        case UTF8_GETCHR(u8"遺"): return 1386;
        case UTF8_GETCHR(u8"狙"): return 1387;
        case UTF8_GETCHR(u8"侠"): return 1388;
        case UTF8_GETCHR(u8"撚"): return 1389;
        case UTF8_GETCHR(u8"嚇"): return 1390;
        case UTF8_GETCHR(u8"斐"): return 1391;
        case UTF8_GETCHR(u8"劉"): return 1392;
        case UTF8_GETCHR(u8"唄"): return 1393;
        case UTF8_GETCHR(u8"鍼"): return 1394;
        case UTF8_GETCHR(u8"榎"): return 1395;
        case UTF8_GETCHR(u8"軽"): return 1396;
        case UTF8_GETCHR(u8"潤"): return 1397;
        case UTF8_GETCHR(u8"賊"): return 1398;
        case UTF8_GETCHR(u8"疎"): return 1399;
        case UTF8_GETCHR(u8"巣"): return 1400;
        case UTF8_GETCHR(u8"執"): return 1401;
        case UTF8_GETCHR(u8"濯"): return 1402;
        case UTF8_GETCHR(u8"啄"): return 1403;
        case UTF8_GETCHR(u8"冊"): return 1404;
        case UTF8_GETCHR(u8"怖"): return 1405;
        case UTF8_GETCHR(u8"華"): return 1406;
        case UTF8_GETCHR(u8"輔"): return 1407;
        case UTF8_GETCHR(u8"紅"): return 1408;
        case UTF8_GETCHR(u8"轄"): return 1409;
        case UTF8_GETCHR(u8"戯"): return 1410;
        case UTF8_GETCHR(u8"麺"): return 1411;
        case UTF8_GETCHR(u8"衛"): return 1412;
        case UTF8_GETCHR(u8"藝"): return 1413;
        case UTF8_GETCHR(u8"壌"): return 1414;
        case UTF8_GETCHR(u8"抜"): return 1415;
        case UTF8_GETCHR(u8"舜"): return 1416;
        case UTF8_GETCHR(u8"鋼"): return 1417;
        case UTF8_GETCHR(u8"訪"): return 1418;
        case UTF8_GETCHR(u8"園"): return 1419;
        case UTF8_GETCHR(u8"織"): return 1420;
        case UTF8_GETCHR(u8"汎"): return 1421;
        case UTF8_GETCHR(u8"癒"): return 1422;
        case UTF8_GETCHR(u8"杞"): return 1423;
        case UTF8_GETCHR(u8"険"): return 1424;
        case UTF8_GETCHR(u8"庵"): return 1425;
        case UTF8_GETCHR(u8"歩"): return 1426;
        case UTF8_GETCHR(u8"韻"): return 1427;
        case UTF8_GETCHR(u8"糾"): return 1428;
        case UTF8_GETCHR(u8"塊"): return 1429;
        case UTF8_GETCHR(u8"畔"): return 1430;
        case UTF8_GETCHR(u8"轟"): return 1431;
        case UTF8_GETCHR(u8"揺"): return 1432;
        case UTF8_GETCHR(u8"佃"): return 1433;
        case UTF8_GETCHR(u8"舶"): return 1434;
        case UTF8_GETCHR(u8"嬉"): return 1435;
        case UTF8_GETCHR(u8"楠"): return 1436;
        case UTF8_GETCHR(u8"凍"): return 1437;
        case UTF8_GETCHR(u8"錯"): return 1438;
        case UTF8_GETCHR(u8"頻"): return 1439;
        case UTF8_GETCHR(u8"較"): return 1440;
        case UTF8_GETCHR(u8"試"): return 1441;
        case UTF8_GETCHR(u8"輸"): return 1442;
        case UTF8_GETCHR(u8"賀"): return 1443;
        case UTF8_GETCHR(u8"祉"): return 1444;
        case UTF8_GETCHR(u8"寝"): return 1445;
        case UTF8_GETCHR(u8"電"): return 1446;
        case UTF8_GETCHR(u8"庁"): return 1447;
        case UTF8_GETCHR(u8"姫"): return 1448;
        case UTF8_GETCHR(u8"峨"): return 1449;
        case UTF8_GETCHR(u8"結"): return 1450;
        case UTF8_GETCHR(u8"藍"): return 1451;
        case UTF8_GETCHR(u8"陥"): return 1452;
        case UTF8_GETCHR(u8"様"): return 1453;
        case UTF8_GETCHR(u8"薬"): return 1454;
        case UTF8_GETCHR(u8"評"): return 1455;
        case UTF8_GETCHR(u8"識"): return 1456;
        case UTF8_GETCHR(u8"巌"): return 1457;
        case UTF8_GETCHR(u8"歓"): return 1458;
        case UTF8_GETCHR(u8"聡"): return 1459;
        case UTF8_GETCHR(u8"貫"): return 1460;
        case UTF8_GETCHR(u8"運"): return 1461;
        case UTF8_GETCHR(u8"暦"): return 1462;
        case UTF8_GETCHR(u8"牟"): return 1463;
        case UTF8_GETCHR(u8"桜"): return 1464;
        case UTF8_GETCHR(u8"貯"): return 1465;
        case UTF8_GETCHR(u8"垣"): return 1466;
        case UTF8_GETCHR(u8"幾"): return 1467;
        case UTF8_GETCHR(u8"謀"): return 1468;
        case UTF8_GETCHR(u8"苑"): return 1469;
        case UTF8_GETCHR(u8"際"): return 1470;
        case UTF8_GETCHR(u8"炊"): return 1471;
        case UTF8_GETCHR(u8"賛"): return 1472;
        case UTF8_GETCHR(u8"醸"): return 1473;
        case UTF8_GETCHR(u8"農"): return 1474;
        case UTF8_GETCHR(u8"戻"): return 1475;
        case UTF8_GETCHR(u8"賞"): return 1476;
        case UTF8_GETCHR(u8"舎"): return 1477;
        case UTF8_GETCHR(u8"権"): return 1478;
        case UTF8_GETCHR(u8"創"): return 1479;
        case UTF8_GETCHR(u8"暁"): return 1480;
        case UTF8_GETCHR(u8"買"): return 1481;
        case UTF8_GETCHR(u8"車"): return 1482;
        case UTF8_GETCHR(u8"陸"): return 1483;
        case UTF8_GETCHR(u8"類"): return 1484;
        case UTF8_GETCHR(u8"栗"): return 1485;
        case UTF8_GETCHR(u8"槻"): return 1486;
        case UTF8_GETCHR(u8"焉"): return 1487;
        case UTF8_GETCHR(u8"鳳"): return 1488;
        case UTF8_GETCHR(u8"浜"): return 1489;
        case UTF8_GETCHR(u8"載"): return 1490;
        case UTF8_GETCHR(u8"繕"): return 1491;
        case UTF8_GETCHR(u8"貝"): return 1492;
        case UTF8_GETCHR(u8"庫"): return 1493;
        case UTF8_GETCHR(u8"睦"): return 1494;
        case UTF8_GETCHR(u8"斎"): return 1495;
        case UTF8_GETCHR(u8"唆"): return 1496;
        case UTF8_GETCHR(u8"頬"): return 1497;
        case UTF8_GETCHR(u8"謎"): return 1498;
        case UTF8_GETCHR(u8"祭"): return 1499;
        case UTF8_GETCHR(u8"倉"): return 1500;
        case UTF8_GETCHR(u8"業"): return 1501;
        case UTF8_GETCHR(u8"災"): return 1502;
        case UTF8_GETCHR(u8"俵"): return 1503;
        case UTF8_GETCHR(u8"両"): return 1504;
        case UTF8_GETCHR(u8"壬"): return 1505;
        case UTF8_GETCHR(u8"現"): return 1506;
        case UTF8_GETCHR(u8"築"): return 1507;
        case UTF8_GETCHR(u8"姚"): return 1508;
        case UTF8_GETCHR(u8"岐"): return 1509;
        case UTF8_GETCHR(u8"顕"): return 1510;
        case UTF8_GETCHR(u8"錦"): return 1511;
        case UTF8_GETCHR(u8"漱"): return 1512;
        case UTF8_GETCHR(u8"週"): return 1513;
        case UTF8_GETCHR(u8"冗"): return 1514;
        case UTF8_GETCHR(u8"勘"): return 1515;
        case UTF8_GETCHR(u8"巳"): return 1516;
        case UTF8_GETCHR(u8"堅"): return 1517;
        case UTF8_GETCHR(u8"蘭"): return 1518;
        case UTF8_GETCHR(u8"騒"): return 1519;
        case UTF8_GETCHR(u8"髄"): return 1520;
        case UTF8_GETCHR(u8"遠"): return 1521;
        case UTF8_GETCHR(u8"魅"): return 1522;
        case UTF8_GETCHR(u8"観"): return 1523;
        case UTF8_GETCHR(u8"憾"): return 1524;
        case UTF8_GETCHR(u8"査"): return 1525;
        case UTF8_GETCHR(u8"級"): return 1526;
        case UTF8_GETCHR(u8"戸"): return 1527;
        case UTF8_GETCHR(u8"領"): return 1528;
        case UTF8_GETCHR(u8"嘘"): return 1529;
        case UTF8_GETCHR(u8"郷"): return 1530;
        case UTF8_GETCHR(u8"闊"): return 1531;
        case UTF8_GETCHR(u8"輝"): return 1532;
        case UTF8_GETCHR(u8"記"): return 1533;
        case UTF8_GETCHR(u8"穣"): return 1534;
        case UTF8_GETCHR(u8"補"): return 1535;
        case UTF8_GETCHR(u8"琉"): return 1536;
        case UTF8_GETCHR(u8"胴"): return 1537;
        case UTF8_GETCHR(u8"敗"): return 1538;
        case UTF8_GETCHR(u8"釣"): return 1539;
        case UTF8_GETCHR(u8"偉"): return 1540;
        case UTF8_GETCHR(u8"馬"): return 1541;
        case UTF8_GETCHR(u8"嫡"): return 1542;
        case UTF8_GETCHR(u8"貰"): return 1543;
        case UTF8_GETCHR(u8"層"): return 1544;
        case UTF8_GETCHR(u8"巽"): return 1545;
        case UTF8_GETCHR(u8"貿"): return 1546;
        case UTF8_GETCHR(u8"児"): return 1547;
        case UTF8_GETCHR(u8"遷"): return 1548;
        case UTF8_GETCHR(u8"帯"): return 1549;
        case UTF8_GETCHR(u8"摯"): return 1550;
        case UTF8_GETCHR(u8"願"): return 1551;
        case UTF8_GETCHR(u8"緯"): return 1552;
        case UTF8_GETCHR(u8"挫"): return 1553;
        case UTF8_GETCHR(u8"銃"): return 1554;
        case UTF8_GETCHR(u8"奮"): return 1555;
        case UTF8_GETCHR(u8"嘆"): return 1556;
        case UTF8_GETCHR(u8"婦"): return 1557;
        case UTF8_GETCHR(u8"鞍"): return 1558;
        case UTF8_GETCHR(u8"紺"): return 1559;
        case UTF8_GETCHR(u8"悩"): return 1560;
        case UTF8_GETCHR(u8"円"): return 1561;
        case UTF8_GETCHR(u8"蓋"): return 1562;
        case UTF8_GETCHR(u8"凄"): return 1563;
        case UTF8_GETCHR(u8"彦"): return 1564;
        case UTF8_GETCHR(u8"剤"): return 1565;
        case UTF8_GETCHR(u8"嶌"): return 1566;
        case UTF8_GETCHR(u8"導"): return 1567;
        case UTF8_GETCHR(u8"誓"): return 1568;
        case UTF8_GETCHR(u8"雫"): return 1569;
        case UTF8_GETCHR(u8"簗"): return 1570;
        case UTF8_GETCHR(u8"壱"): return 1571;
        case UTF8_GETCHR(u8"鮫"): return 1572;
        case UTF8_GETCHR(u8"涙"): return 1573;
        case UTF8_GETCHR(u8"飾"): return 1574;
        case UTF8_GETCHR(u8"甦"): return 1575;
        case UTF8_GETCHR(u8"闇"): return 1576;
        case UTF8_GETCHR(u8"挙"): return 1577;
        case UTF8_GETCHR(u8"釈"): return 1578;
        case UTF8_GETCHR(u8"楢"): return 1579;
        case UTF8_GETCHR(u8"暫"): return 1580;
        case UTF8_GETCHR(u8"並"): return 1581;
        case UTF8_GETCHR(u8"桧"): return 1582;
        case UTF8_GETCHR(u8"峯"): return 1583;
        case UTF8_GETCHR(u8"獲"): return 1584;
        case UTF8_GETCHR(u8"簿"): return 1585;
        case UTF8_GETCHR(u8"猿"): return 1586;
        case UTF8_GETCHR(u8"飽"): return 1587;
        case UTF8_GETCHR(u8"訓"): return 1588;
        case UTF8_GETCHR(u8"樫"): return 1589;
        case UTF8_GETCHR(u8"離"): return 1590;
        case UTF8_GETCHR(u8"弧"): return 1591;
        case UTF8_GETCHR(u8"噛"): return 1592;
        case UTF8_GETCHR(u8"貼"): return 1593;
        case UTF8_GETCHR(u8"駿"): return 1594;
        case UTF8_GETCHR(u8"枠"): return 1595;
        case UTF8_GETCHR(u8"虜"): return 1596;
        case UTF8_GETCHR(u8"醍"): return 1597;
        case UTF8_GETCHR(u8"稜"): return 1598;
        case UTF8_GETCHR(u8"働"): return 1599;
        case UTF8_GETCHR(u8"堕"): return 1600;
        case UTF8_GETCHR(u8"愛"): return 1601;
        case UTF8_GETCHR(u8"朔"): return 1602;
        case UTF8_GETCHR(u8"綱"): return 1603;
        case UTF8_GETCHR(u8"購"): return 1604;
        case UTF8_GETCHR(u8"維"): return 1605;


        // Cyrillic Characters
        case UTF8_GETCHR(u8"А"): return 187;
        case UTF8_GETCHR(u8"Б"): return 188;
        case UTF8_GETCHR(u8"В"): return 189;
        case UTF8_GETCHR(u8"Г"): return 190;
        case UTF8_GETCHR(u8"Д"): return 191;
        case UTF8_GETCHR(u8"Е"): return 192;
        case UTF8_GETCHR(u8"Ж"): return 193;
        case UTF8_GETCHR(u8"З"): return 194;
        case UTF8_GETCHR(u8"И"): return 195;
        case UTF8_GETCHR(u8"Й"): return 196;
        case UTF8_GETCHR(u8"К"): return 197;
        case UTF8_GETCHR(u8"Л"): return 198;
        case UTF8_GETCHR(u8"М"): return 199;
        case UTF8_GETCHR(u8"Н"): return 200;
        case UTF8_GETCHR(u8"О"): return 201;
        case UTF8_GETCHR(u8"П"): return 202;
        case UTF8_GETCHR(u8"Р"): return 203;
        case UTF8_GETCHR(u8"С"): return 204;
        case UTF8_GETCHR(u8"Т"): return 205;
        case UTF8_GETCHR(u8"У"): return 206;
        case UTF8_GETCHR(u8"Ф"): return 207;
        case UTF8_GETCHR(u8"Х"): return 208;
        case UTF8_GETCHR(u8"Ц"): return 209;
        case UTF8_GETCHR(u8"Ч"): return 210;
        case UTF8_GETCHR(u8"Ш"): return 211;
        case UTF8_GETCHR(u8"Щ"): return 212;
        case UTF8_GETCHR(u8"Ъ"): return 213;
        case UTF8_GETCHR(u8"Ы"): return 214;
        case UTF8_GETCHR(u8"Ь"): return 215;
        case UTF8_GETCHR(u8"Э"): return 216;
        case UTF8_GETCHR(u8"Ю"): return 217;
        case UTF8_GETCHR(u8"Я"): return 218;
        case UTF8_GETCHR(u8"а"): return 219;
        case UTF8_GETCHR(u8"б"): return 220;
        case UTF8_GETCHR(u8"в"): return 221;
        case UTF8_GETCHR(u8"г"): return 222;
        case UTF8_GETCHR(u8"д"): return 223;
        case UTF8_GETCHR(u8"е"): return 224;
        case UTF8_GETCHR(u8"ж"): return 225;
        case UTF8_GETCHR(u8"з"): return 226;
        case UTF8_GETCHR(u8"и"): return 227;
        case UTF8_GETCHR(u8"й"): return 228;
        case UTF8_GETCHR(u8"к"): return 229;
        case UTF8_GETCHR(u8"л"): return 230;
        case UTF8_GETCHR(u8"м"): return 231;
        case UTF8_GETCHR(u8"н"): return 232;
        case UTF8_GETCHR(u8"о"): return 233;
        case UTF8_GETCHR(u8"п"): return 234;
        case UTF8_GETCHR(u8"р"): return 235;
        case UTF8_GETCHR(u8"с"): return 236;
        case UTF8_GETCHR(u8"т"): return 237;
        case UTF8_GETCHR(u8"у"): return 238;
        case UTF8_GETCHR(u8"ф"): return 239;
        case UTF8_GETCHR(u8"х"): return 240;
        case UTF8_GETCHR(u8"ц"): return 241;
        case UTF8_GETCHR(u8"ч"): return 242;
        case UTF8_GETCHR(u8"ш"): return 243;
        case UTF8_GETCHR(u8"щ"): return 244;
        case UTF8_GETCHR(u8"ъ"): return 245;
        case UTF8_GETCHR(u8"ы"): return 246;
        case UTF8_GETCHR(u8"ь"): return 246;
        case UTF8_GETCHR(u8"э"): return 248;
        case UTF8_GETCHR(u8"ю"): return 249;
        case UTF8_GETCHR(u8"я"): return 250;
        case UTF8_GETCHR(u8"Ґ"): return 251;
        case UTF8_GETCHR(u8"ґ"): return 252;
        case UTF8_GETCHR(u8"Є"): return 253;
        case UTF8_GETCHR(u8"є"): return 254;
        case UTF8_GETCHR(u8"Ї"): return 255;
        case UTF8_GETCHR(u8"ї"): return 256;
        case UTF8_GETCHR(u8"ё"): return 87;

        // Japanese
        case UTF8_GETCHR(u8"ア"): return 106;
        case UTF8_GETCHR(u8"イ"): return 107;
        case UTF8_GETCHR(u8"ウ"): return 108;
        case UTF8_GETCHR(u8"エ"): return 109;
        case UTF8_GETCHR(u8"オ"): return 110;
        case UTF8_GETCHR(u8"カ"): return 111;
        case UTF8_GETCHR(u8"キ"): return 112;
        case UTF8_GETCHR(u8"ク"): return 113;
        case UTF8_GETCHR(u8"ケ"): return 114;
        case UTF8_GETCHR(u8"コ"): return 115;
        case UTF8_GETCHR(u8"サ"): return 116;
        case UTF8_GETCHR(u8"シ"): return 117;
        case UTF8_GETCHR(u8"ス"): return 118;
        case UTF8_GETCHR(u8"セ"): return 119;
        case UTF8_GETCHR(u8"ソ"): return 120;
        case UTF8_GETCHR(u8"タ"): return 121;
        case UTF8_GETCHR(u8"チ"): return 122;
        case UTF8_GETCHR(u8"ッ"): return 123;
        case UTF8_GETCHR(u8"ツ"): return 124;
        case UTF8_GETCHR(u8"テ"): return 125;
        case UTF8_GETCHR(u8"ト"): return 126;
        case UTF8_GETCHR(u8"ナ"): return 127;
        case UTF8_GETCHR(u8"ニ"): return 128;
        case UTF8_GETCHR(u8"ヌ"): return 129;
        case UTF8_GETCHR(u8"ネ"): return 130;
        case UTF8_GETCHR(u8"ノ"): return 131;
        case UTF8_GETCHR(u8"ハ"): return 132;
        case UTF8_GETCHR(u8"ヒ"): return 133;
        case UTF8_GETCHR(u8"フ"): return 134;
        case UTF8_GETCHR(u8"ヘ"): return 135;
        case UTF8_GETCHR(u8"ホ"): return 136;
        case UTF8_GETCHR(u8"マ"): return 137;
        case UTF8_GETCHR(u8"ミ"): return 138;
        case UTF8_GETCHR(u8"ム"): return 139;
        case UTF8_GETCHR(u8"メ"): return 140;
        case UTF8_GETCHR(u8"モ"): return 141;
        case UTF8_GETCHR(u8"ヤ"): return 142;
        case UTF8_GETCHR(u8"ユ"): return 143;
        case UTF8_GETCHR(u8"ヨ"): return 144;
        case UTF8_GETCHR(u8"ラ"): return 145;
        case UTF8_GETCHR(u8"リ"): return 146;
        case UTF8_GETCHR(u8"ル"): return 147;
        case UTF8_GETCHR(u8"レ"): return 148;
        case UTF8_GETCHR(u8"ロ"): return 149;
        case UTF8_GETCHR(u8"ワ"): return 150;
        case UTF8_GETCHR(u8"ヲ"): return 151;
        case UTF8_GETCHR(u8"ン"): return 152;
        case UTF8_GETCHR(u8"ガ"): return 153;
        case UTF8_GETCHR(u8"ギ"): return 154;
        case UTF8_GETCHR(u8"グ"): return 155;
        case UTF8_GETCHR(u8"ゲ"): return 156;
        case UTF8_GETCHR(u8"ゴ"): return 157;
        case UTF8_GETCHR(u8"ザ"): return 158;
        case UTF8_GETCHR(u8"ジ"): return 159;
        case UTF8_GETCHR(u8"ズ"): return 160;
        case UTF8_GETCHR(u8"ゼ"): return 161;
        case UTF8_GETCHR(u8"ゾ"): return 162;
        case UTF8_GETCHR(u8"ダ"): return 163;
        case UTF8_GETCHR(u8"ヂ"): return 164;
        case UTF8_GETCHR(u8"ヅ"): return 165;
        case UTF8_GETCHR(u8"デ"): return 166;
        case UTF8_GETCHR(u8"ド"): return 167;
        case UTF8_GETCHR(u8"バ"): return 168;
        case UTF8_GETCHR(u8"パ"): return 169;
        case UTF8_GETCHR(u8"ビ"): return 170;
        case UTF8_GETCHR(u8"ピ"): return 171;
        case UTF8_GETCHR(u8"ブ"): return 172;
        case UTF8_GETCHR(u8"プ"): return 173;
        case UTF8_GETCHR(u8"ベ"): return 174;
        case UTF8_GETCHR(u8"ペ"): return 175;
        case UTF8_GETCHR(u8"ボ"): return 176;
        case UTF8_GETCHR(u8"ポ"): return 177;
        case UTF8_GETCHR(u8"ー"): return 178;
        case UTF8_GETCHR(u8"ヴ"): return 179;
        case UTF8_GETCHR(u8"ァ"): return 180;
        case UTF8_GETCHR(u8"ィ"): return 181;
        case UTF8_GETCHR(u8"ゥ"): return 182;
        case UTF8_GETCHR(u8"ェ"): return 183;
        case UTF8_GETCHR(u8"ォ"): return 333;
        case UTF8_GETCHR(u8"ャ"): return 334;
        case UTF8_GETCHR(u8"ュ"): return 335;
        case UTF8_GETCHR(u8"ョ"): return 336;
        case UTF8_GETCHR(u8"ヮ"): return 337;
        case UTF8_GETCHR(u8"ヰ"): return 338;
        case UTF8_GETCHR(u8"ヱ"): return 339;
        case UTF8_GETCHR(u8"ヵ"): return 340;
        case UTF8_GETCHR(u8"ヶ"): return 341;
        case UTF8_GETCHR(u8"ヽ"): return 342;
        case UTF8_GETCHR(u8"ヾ"): return 343;
        case UTF8_GETCHR(u8"・"): return 184;
        case UTF8_GETCHR(u8"あ"): return 257;
        case UTF8_GETCHR(u8"い"): return 258;
        case UTF8_GETCHR(u8"う"): return 259;
        case UTF8_GETCHR(u8"え"): return 260;
        case UTF8_GETCHR(u8"お"): return 261;
        case UTF8_GETCHR(u8"か"): return 262;
        case UTF8_GETCHR(u8"き"): return 263;
        case UTF8_GETCHR(u8"く"): return 264;
        case UTF8_GETCHR(u8"け"): return 265;
        case UTF8_GETCHR(u8"こ"): return 266;
        case UTF8_GETCHR(u8"さ"): return 267;
        case UTF8_GETCHR(u8"し"): return 268;
        case UTF8_GETCHR(u8"す"): return 269;
        case UTF8_GETCHR(u8"せ"): return 270;
        case UTF8_GETCHR(u8"そ"): return 271;
        case UTF8_GETCHR(u8"た"): return 272;
        case UTF8_GETCHR(u8"ち"): return 273;
        case UTF8_GETCHR(u8"つ"): return 274;
        case UTF8_GETCHR(u8"て"): return 275;
        case UTF8_GETCHR(u8"と"): return 276;
        case UTF8_GETCHR(u8"な"): return 277;
        case UTF8_GETCHR(u8"に"): return 278;
        case UTF8_GETCHR(u8"ぬ"): return 279;
        case UTF8_GETCHR(u8"ね"): return 280;
        case UTF8_GETCHR(u8"の"): return 281;
        case UTF8_GETCHR(u8"は"): return 282;
        case UTF8_GETCHR(u8"ひ"): return 283;
        case UTF8_GETCHR(u8"ふ"): return 284;
        case UTF8_GETCHR(u8"へ"): return 285;
        case UTF8_GETCHR(u8"ほ"): return 286;
        case UTF8_GETCHR(u8"ま"): return 287;
        case UTF8_GETCHR(u8"み"): return 288;
        case UTF8_GETCHR(u8"む"): return 289;
        case UTF8_GETCHR(u8"め"): return 290;
        case UTF8_GETCHR(u8"も"): return 291;
        case UTF8_GETCHR(u8"や"): return 292;
        case UTF8_GETCHR(u8"ゆ"): return 293;
        case UTF8_GETCHR(u8"よ"): return 294;
        case UTF8_GETCHR(u8"ら"): return 295;
        case UTF8_GETCHR(u8"り"): return 296;
        case UTF8_GETCHR(u8"る"): return 297;
        case UTF8_GETCHR(u8"れ"): return 298;
        case UTF8_GETCHR(u8"を"): return 299;
        case UTF8_GETCHR(u8"ん"): return 300;
        case UTF8_GETCHR(u8"ぁ"): return 344;
        case UTF8_GETCHR(u8"ぃ"): return 345;
        case UTF8_GETCHR(u8"ぅ"): return 346;
        case UTF8_GETCHR(u8"ぇ"): return 347;
        case UTF8_GETCHR(u8"ぉ"): return 348;
        case UTF8_GETCHR(u8"が"): return 349;
        case UTF8_GETCHR(u8"ぎ"): return 350;
        case UTF8_GETCHR(u8"ぐ"): return 351;
        case UTF8_GETCHR(u8"げ"): return 352;
        case UTF8_GETCHR(u8"ご"): return 353;
        case UTF8_GETCHR(u8"ざ"): return 354;
        case UTF8_GETCHR(u8"じ"): return 355;
        case UTF8_GETCHR(u8"ず"): return 356;
        case UTF8_GETCHR(u8"ぜ"): return 357;
        case UTF8_GETCHR(u8"ぞ"): return 358;
        case UTF8_GETCHR(u8"だ"): return 359;
        case UTF8_GETCHR(u8"ぢ"): return 360;
        case UTF8_GETCHR(u8"っ"): return 361;
        case UTF8_GETCHR(u8"づ"): return 362;
        case UTF8_GETCHR(u8"で"): return 363;
        case UTF8_GETCHR(u8"ど"): return 364;
        case UTF8_GETCHR(u8"ば"): return 365;
        case UTF8_GETCHR(u8"ぱ"): return 366;
        case UTF8_GETCHR(u8"び"): return 367;
        case UTF8_GETCHR(u8"ぴ"): return 368;
        case UTF8_GETCHR(u8"ぶ"): return 369;
        case UTF8_GETCHR(u8"ぷ"): return 370;
        case UTF8_GETCHR(u8"べ"): return 371;
        case UTF8_GETCHR(u8"ぺ"): return 372;
        case UTF8_GETCHR(u8"ぼ"): return 373;
        case UTF8_GETCHR(u8"ぽ"): return 374;
        case UTF8_GETCHR(u8"ゃ"): return 375;
        case UTF8_GETCHR(u8"ゅ"): return 376;
        case UTF8_GETCHR(u8"ょ"): return 377;
        case UTF8_GETCHR(u8"ろ"): return 378;
        case UTF8_GETCHR(u8"ゎ"): return 379;
        case UTF8_GETCHR(u8"わ"): return 380;
        case UTF8_GETCHR(u8"ゐ"): return 381;
        case UTF8_GETCHR(u8"ゑ"): return 382;
        case UTF8_GETCHR(u8"゛"): return 383;
        case UTF8_GETCHR(u8"ゝ"): return 384;
        case UTF8_GETCHR(u8"ゞ"): return 385;
        case UTF8_GETCHR(u8"゜"): return 386;
        case UTF8_GETCHR(u8"、"): return 387;
        case UTF8_GETCHR(u8"設"): return 388;
        case UTF8_GETCHR(u8"構"): return 389;
        case UTF8_GETCHR(u8"動"): return 390;
        case UTF8_GETCHR(u8"撃"): return 391;

        case UTF8_GETCHR(u8"β"): return 301;
        case UTF8_GETCHR(u8"ñ"): return 73;
        case UTF8_GETCHR(u8"á"): return 74;
        case UTF8_GETCHR(u8"é"): return 75;
        case UTF8_GETCHR(u8"í"): return 76;
        case UTF8_GETCHR(u8"ó"): return 77;
        case UTF8_GETCHR(u8"Ó"): return 77;
        case UTF8_GETCHR(u8"ú"): return 78;
        case UTF8_GETCHR(u8"â"): return 79;
        case UTF8_GETCHR(u8"ê"): return 80;
        case UTF8_GETCHR(u8"î"): return 81;
        case UTF8_GETCHR(u8"Ô"): return 82;
        case UTF8_GETCHR(u8"ô"): return 82;
        case UTF8_GETCHR(u8"û"): return 83;
        case UTF8_GETCHR(u8"à"): return 84;
        case UTF8_GETCHR(u8"è"): return 85;
        case UTF8_GETCHR(u8"ù"): return 86;
        case UTF8_GETCHR(u8"ë"): return 87;
        case UTF8_GETCHR(u8"ï"): return 88;
        case UTF8_GETCHR(u8"ü"): return 89;
        case UTF8_GETCHR(u8"ç"): return 90;
        case UTF8_GETCHR(u8"Ç"): return 91;
        case UTF8_GETCHR(u8"ö"): return 92;
        case UTF8_GETCHR(u8"Ö"): return 92;
        case UTF8_GETCHR(u8"¡"): return 96;
        case UTF8_GETCHR(u8"¿"): return 97;
        case UTF8_GETCHR(u8"ì"): return 306;
        case UTF8_GETCHR(u8"ä"): return 303;
        case UTF8_GETCHR(u8"Ü"): return 304;
        case UTF8_GETCHR(u8"ß"): return 305;
        case UTF8_GETCHR(u8"Å"): return 307;
        case UTF8_GETCHR(u8"å"): return 308;
        case UTF8_GETCHR(u8"Ä"): return 309;
        case UTF8_GETCHR(u8"…"): return 310;
        case UTF8_GETCHR(u8"ã"): return 312;
        case UTF8_GETCHR(u8"Õ"): return 314;
        case UTF8_GETCHR(u8"õ"): return 314;
        case UTF8_GETCHR(u8"Ú"): return 315;
        case UTF8_GETCHR(u8"Í"): return 316;
        case UTF8_GETCHR(u8"Ê"): return 317;
        case UTF8_GETCHR(u8"É"): return 318;
        case UTF8_GETCHR(u8"Ã"): return 319;
        case UTF8_GETCHR(u8"Â"): return 320;
        case UTF8_GETCHR(u8"Á"): return 321;
        case UTF8_GETCHR(u8"À"): return 322;
        case UTF8_GETCHR(u8"Æ"): return 323;
        case UTF8_GETCHR(u8"Ð"): return 324;
        case UTF8_GETCHR(u8"ý"): return 325;
        case UTF8_GETCHR(u8"Ý"): return 326;
        case UTF8_GETCHR(u8"Þ"): return 327;
        case UTF8_GETCHR(u8"þ"): return 328;
        case UTF8_GETCHR(u8"æ"): return 329;
        case UTF8_GETCHR(u8"ð"): return 330;
        case UTF8_GETCHR(u8"§"): return 331;
        case UTF8_GETCHR(u8"~"): return 332;

            // clang-format on

        default:
            return nullopt();
        }
    }();
    if (mapping) {
        return Platform::TextureMapping{font_image, *mapping};
    } else {
        return extended_charset_map(cp);
    }
}


Optional<Platform::TextureMapping>
doublesize_texture_map(const utf8::Codepoint& cp)
{
    return {};
}


Optional<Platform::TextureMapping> null_texture_map(const utf8::Codepoint&)
{
    return {};
}


static int language_id = 0;


Platform::TextureCpMapper locale_texture_map()
{
    return standard_charset_map;
}


Platform::TextureCpMapper locale_doublesize_texture_map()
{
    return doublesize_texture_map;
}


void locale_set_language(int language_id)
{
    ::language_id = language_id;
}


// I had to add this code during chinese translation, for places where I needed
// to use traditional chinese numbers rather than arabic numerals.
const char* locale_repr_smallnum(u8 num, Array<char, 40>& buffer)
{
    auto languages = lisp::get_var(lisp::make_symbol("languages"));

    auto lang = lisp::get_list(languages, ::language_id);

    const char* lang_name =
        lang->expect<lisp::Cons>().car()->expect<lisp::Symbol>().name();

    if (str_cmp(lang_name, "chinese") == 0) {
        // Yeah, this is lazy. I could write a string to
        // number-to-unicode-string algorithm for chinese, but I don't feel like
        // it right now.
        switch (num) {
        default:
        case 1:
            return "一";
        case 2:
            return "二";
        case 3:
            return "三";
        case 4:
            return "四";
        case 5:
            return "五";
        case 6:
            return "六";
        case 7:
            return "七";
        case 8:
            return "八";
        case 9:
            return "九";
        case 10:
            return "十";
        case 11:
            return "十一";
        case 12:
            return "十二";
        case 13:
            return "十三";
        case 14:
            return "十四";
        case 15:
            return "十五";
        case 16:
            return "十六";
        case 17:
            return "十七";
        case 18:
            return "十八";
        case 19:
            return "十九";
        case 20:
            return "二十";
        case 21:
            return "二十一";
        case 22:
            return "二十二";
        case 23:
            return "二十三";
        case 24:
            return "二十四";
        case 25:
            return "二十五";
        case 26:
            return "二十六";
        case 27:
            return "二十七";
        case 28:
            return "二十八";
        case 29:
            return "二十九";
        case 30:
            return "三十";
        case 31:
            return "三十一";
        case 32:
            return "三十二";
        case 33:
            return "三十三";
        case 34:
            return "三十四";
        case 35:
            return "三十五";
        case 36:
            return "三十六";
        case 37:
            return "三十七";
        case 38:
            return "三十八";
        case 39:
            return "三十九";
        case 40:
            return "四十";
        case 41:
            return "四十一";
        case 42:
            return "四十二";
        case 43:
            return "四十三";
        case 44:
            return "四十四";
        case 45:
            return "四十五";
        case 46:
            return "四十六";
        case 47:
            return "四十七";
        case 48:
            return "四十八";
        case 49:
            return "四十九";
        }
    } else {
        // Arabic numerals
        locale_num2str(num, buffer.data(), 10);
        return buffer.data();
    }
}


int locale_get_language()
{
    return ::language_id;
}


StringBuffer<31> locale_language_name(int language)
{
    auto languages = lisp::get_var(lisp::make_symbol("languages"));

    auto lang = lisp::get_list(languages, language);

    return lang->expect<lisp::Cons>().car()->expect<lisp::Symbol>().name();
}


bool locale_requires_doublesize_font()
{
    auto languages = lisp::get_var(lisp::make_symbol("languages"));

    auto lang = lisp::get_list(languages, ::language_id);

    return lang->expect<lisp::Cons>()
               .cdr()
               ->expect<lisp::Cons>()
               .car()
               ->expect<lisp::Integer>()
               .value_ == 2;
}



void arabic__to_string(int num, char* buffer, int base)
{
    int i = 0;
    bool is_negative = false;

    if (num == 0) {
        buffer[i++] = '0';
        buffer[i] = '\0';
        return;
    }

    // Based on the behavior of itoa()
    if (num < 0 && base == 10) {
        is_negative = true;
        num = -num;
    }

    while (num != 0) {
        int rem = num % base;
        buffer[i++] = (rem > 9) ? (rem - 10) + 'a' : rem + '0';
        num = num / base;
    }

    if (is_negative) {
        buffer[i++] = '-';
    }

    buffer[i] = '\0';

    str_reverse(buffer, i);

    return;
}


void locale_num2str(int num, char* buffer, int base)
{
    arabic__to_string(num, buffer, base);
}
