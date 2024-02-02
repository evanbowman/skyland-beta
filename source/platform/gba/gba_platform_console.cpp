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


#include "platform/platform.hpp"
#include "gba.h"
#include "allocator.hpp"



using ConsoleLine = Platform::RemoteConsole::Line;


struct RemoteConsoleState
{
    // NOTE: Some of these vars should probably be volatile, but, as we're
    // dealing with keystrokes, which happen on a human timescale, the
    // probability of anything getting messed up is pretty small.

    Optional<DynamicMemory<ConsoleLine>> rx_in_progress_;
    Buffer<DynamicMemory<ConsoleLine>, 4> rx_full_lines_;

    Optional<DynamicMemory<ConsoleLine>> tx_msg_;
};


static EWRAM_DATA Optional<RemoteConsoleState> remote_console_state;



static bool uart_echo_skip = false;



static void uart_serial_isr()
{
    if (uart_echo_skip) {
        uart_echo_skip = false;
        return;
    }

    auto& state = *::remote_console_state;

    const char data = REG_SIODATA8;

    const bool is_uart_error = REG_SIOCNT & (1 << 6);
    if (is_uart_error) {
        return;
    }

    // NOTE: a later discovery: in addition to is_rcv not working in the
    // commented out code (below), send_ready also does not work. i.e. we cannot
    // receive while we're sending.
    const bool send_ready = !(REG_SIOCNT & 0x0010);
    if (send_ready and state.tx_msg_ and not(*state.tx_msg_)->empty()) {

        REG_SIODATA8 = *((*state.tx_msg_)->end() - 1);
        (*state.tx_msg_)->pop_back();
        state.rx_in_progress_.reset();
        return;
    } else if (state.tx_msg_ and (*state.tx_msg_)->empty()) {
        state.tx_msg_.reset();
    }

    // FIXME: I'm not seeing the receive_ready flag for some reason, but this
    // code really _should_ be checking the state of the receive flag in the
    // serial control register before reading a byte. What I'm doing here is
    // possibly undefined behavior that happens to work, because my code is
    // ignoring the null bytes.
    //
    // const bool is_rcv = !(REG_SIOCNT & 0x0020);
    // if (is_rcv) {

    if (data == '\r') {
        if (state.rx_in_progress_) {
            state.rx_full_lines_.push_back(std::move(*state.rx_in_progress_));
            state.rx_in_progress_.reset();
        }
    } else if (data == 0x08 /* ASCII backspace */ or
               data == 0x7f /* Strange char used by picocom as a backspace */) {
        if (state.rx_in_progress_) {
            (*state.rx_in_progress_)->pop_back();
        }
    } else if (data == '\0') {
        // Hmm... how'd we end up with a null byte!?
    } else {
        if (not state.rx_in_progress_) {
            state.rx_in_progress_ =
                allocate_dynamic<ConsoleLine>("uart-rx-buffer");
        }

        if (state.rx_in_progress_) {
            (*state.rx_in_progress_)->push_back(data);
        }
    }

    if (data not_eq '\r') {
        if (data == 0x7f) {
            // Why would you send 0x7f but not handle it upon echo? I have no
            // idea why some terminals send a backspace character yet don't
            // understand THE BACKSPACE CHARACTER THAT THEMSELVES SENT! Anyway,
            // I echo back a proper ascii backspace instead, because apparently
            // the developers who wrote these serial consoles were really
            // stupid. Or am I stupid? Someone is clearly an idiot.
            //
            // Description of the problem: In bash, running picocom, I type
            // backspace. GBA receives 0x7f. Whatever, it's not an ascii
            // backspace, but at least it's sort of standardized. I echo back
            // 0x7f, picocom in bash DOES NOTHING.
            REG_SIODATA8 = 0x08;
        } else if (data == 0x04) {
            // Don't echo ctrl-D back to the terminal. Causes problems in some
            // client shells.
            REG_SIODATA8 = '!';
        } else if (data == '\t') {
            // Don't echo back a tab character. The server interprets tabs as
            // autocomplete requests.
        } else {
            REG_SIODATA8 = data;
        }

        // Most serial consoles are super dumb, and assume that the server will
        // echo back everything that they type. If we do the echo now, we'll
        // raise another serial interrupt, which we want to ignore.
        //
        // NOTE: I was relying on local echo for a while, rather than
        // implementing server-side echo. But then, I discovered that Putty
        // echos everything except for newline characters with local echo turned
        // on, and basically, I don't trust the authors of serial terminals to
        // get anything right, based on my experience so far. That's why the
        // server is handling everything and relies on the minimal possible
        // subset of uart functionality. 9600 baud, no local echo, no flow
        // control.
        uart_echo_skip = true;
    }


    // }
}



void remote_console_start()
{
    ::remote_console_state.emplace();

    irqEnable(IRQ_SERIAL);

    irqSet(IRQ_SERIAL, uart_serial_isr);

    // Stick a character in the data register. This stops the GBA transmitting a
    // Character as soon as it goes into UART mode (?!?)
    // REG_SIODATA8 = 'A';

    // Now to go into UART mode
    REG_RCNT = 0;
    REG_SIOCNT = 0;

    // NOTE: see gba.h for constants
    REG_SIOCNT = SIO_9600 | SIO_UART_LENGTH_8 | SIO_UART_SEND_ENABLE |
                 SIO_UART_RECV_ENABLE | SIO_UART | SIO_IRQ;
}



void Platform::RemoteConsole::start()
{
    remote_console_start();
}



auto Platform::RemoteConsole::peek_buffer() -> Line*
{
    auto& state = *::remote_console_state;
    if (state.rx_in_progress_) {
        return &**state.rx_in_progress_;
    }
    return nullptr;
}



auto Platform::RemoteConsole::readline() -> Optional<Line>
{
    auto& state = *::remote_console_state;

    if (not state.rx_full_lines_.empty()) {
        auto ret = std::move(*state.rx_full_lines_.begin());

        state.rx_full_lines_.erase(state.rx_full_lines_.begin());

        return *ret;
    }
    return {};
}


bool Platform::RemoteConsole::printline(const char* text, const char* prompt)
{
    auto& state = *::remote_console_state;

    if (state.tx_msg_ and not(*state.tx_msg_)->empty()) {
        // TODO: add a queue for output messages! At the moment, there's already
        // a message being written, so we'll need to ignore the input.
        return false;
    }

    state.tx_msg_ = allocate_dynamic<ConsoleLine>("uart-console-output");

    Optional<char> first_char;

    if (*text not_eq '\0') {
        first_char = *text;
        ++text;
    }

    while (*text not_eq '\0') {
        // yeah, this isn't great. We're inserting chars into our buffer in
        // reverse order in the printline call, so that we can simply call
        // pop_back(), which is a constant-time operation, in the more
        // time-critical interrupt handler. Every insert() will essentially
        // memmove what's in the buffer by one byte to the right.
        (*state.tx_msg_)->insert((*state.tx_msg_)->begin(), *text);
        ++text;
    }

    (*state.tx_msg_)->insert((*state.tx_msg_)->begin(), '\r');
    (*state.tx_msg_)->insert((*state.tx_msg_)->begin(), '\n');

    while (*prompt not_eq '\0') {
        (*state.tx_msg_)->insert((*state.tx_msg_)->begin(), *prompt);
        ++prompt;
    }

    if (first_char) {
        // Now that we're done copying the output message, place the first
        // character in the uart shift register, to kick off the send.
        REG_SIODATA8 = *first_char;
    } else {
        auto first = *((*state.tx_msg_)->end() - 1);
        (*state.tx_msg_)->pop_back();
        REG_SIODATA8 = first;
    }

    return true;
}
