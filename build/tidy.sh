./set-desktop-toochain.sh
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ./
python3 run_clang_tidy.py -clang-tidy-binary /opt/homebrew/opt/llvm/bin/clang-tidy
