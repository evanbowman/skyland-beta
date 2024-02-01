#pragma once

#include <utility>
#include <concepts>



template <typename F>
class ScopeGuard
{
public:
    ScopeGuard(F&& fn) : fn_(std::move(fn))
    {
    }


    ScopeGuard(ScopeGuard&& o)
    {
        fn_ = std::move(o.fn_);
    }


    ScopeGuard(const ScopeGuard&) = delete;


    ~ScopeGuard()
    {
        fn_();
    }

private:
    F fn_;
};



template <typename F>
auto on_exit(F&& fn)
    requires std::invocable<F>
{
    return ScopeGuard<F>(std::forward<F>(fn));
}
