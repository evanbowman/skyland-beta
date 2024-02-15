////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024  Evan Bowman. Some rights reserved.
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

#include "modules/datetimeModule.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/scene.hpp"
#include "skyland/scene/introCreditsScene.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland
{



static constexpr const char* lang_file = "/lang.txt";



class LanguageSelectScene : public Scene
{
private:
    using LanguageOptions =
        Buffer<Pair<StringBuffer<48>, StringBuffer<48>>, 16>;
    DynamicMemory<LanguageOptions> opts_;

    int sel_ = 0;

    Buffer<Text, 8> text_opts_;

    bool clean_boot_;

public:
    Optional<DeferredScene> next_;


    LanguageSelectScene(bool clean_boot)
        : opts_(load_language_options()), clean_boot_(clean_boot)
    {
    }


    void enter(Scene& prev) override;


    void exit(Scene& prev) override;


    ScenePtr<Scene> update(Time delta) override;


private:
    static DynamicMemory<LanguageOptions> load_language_options();
};



} // namespace skyland
