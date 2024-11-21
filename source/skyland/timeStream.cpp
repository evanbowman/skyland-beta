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


#include "timeStream.hpp"



namespace skyland::time_stream
{



void TimeStream::pop(u32 bytes)
{
    if (end_) {
        if (auto hdr = end_->end()) {
            // We're rolling back the times stream, let's make sure that we
            // update the highest timestamp in the block.
            end_->elapsed_ = hdr->timestamp_.get();
        }

        end_->pop(bytes);

        if (end_->end() == nullptr) {
            TimeBuffer* last = nullptr;
            TimeBuffer* current = &**buffers_;
            while (current) {
                if (current == end_) {
                    end_ = last;
                    if (last) {
                        last->next_.reset();
                    } else {
                        buffers_.reset();
                    }
                    if (buffer_count_ == 0) {
                        Platform::fatal(
                            "logic error: freed a timestream buffer,"
                            "but buffer count is zero!");
                    }
                    --buffer_count_;
                    break;
                }
                last = current;
                if (not current->next_) {
                    Platform::fatal("logic error: if current != end, "
                                    "current must have next.");
                }
                current = &**current->next_;
            }
        }
    }
}



void TimeStream::update(Time delta)
{
    if (end_) {
        end_->update(delta);
    }
}



void TimeStream::rewind(Time delta)
{
    if (end_) {
        end_->rewind(delta);
        if (end_->elapsed_ < 0) {
            end_->elapsed_ = 0;
        }
    }
}


void TimeStream::clear()
{
    buffers_.reset();
    end_ = nullptr;
    buffer_count_ = 0;
}



u64 TimeStream::begin_timestamp()
{
    if (buffers_) {
        return (*buffers_)->time_window_begin_;
    } else {
        return 0;
    }
}



event::Header* TimeStream::end()
{
    if (end_) {
        return end_->end();
    }
    return nullptr;
}



Optional<u64> TimeStream::end_timestamp()
{
    if (end_) {
        if (end_->end()) {
            return end_->time_window_begin_ + end_->end()->timestamp_.get();
        }
    }
    return {};
}



} // namespace skyland::time_stream
