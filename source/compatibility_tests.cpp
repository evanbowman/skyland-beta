#include "platform/platform.hpp"



void test_platform_compat(Platform& pfrm)
{
    pfrm.screen().clear();

    pfrm.delta_clock().reset();
    pfrm.screen().clear();
    pfrm.screen().display();
    auto dt = pfrm.delta_clock().reset();

    static const auto expected_frame_delta = Microseconds(16667);

    if (abs(expected_frame_delta - dt) > Microseconds(400)) {
        Platform::fatal(format("Hardware fault: timer delta per frame "
                               "has high deviation from 16ms! (% micros)",
                               abs(expected_frame_delta - dt)));
    }
}
