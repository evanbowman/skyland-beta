
1) Add a language mapping to lang.txt. The left hand of the expression should contain the language name as you want it to appear on the language select screen. The right hand side should contain the name of the files in this folder to load text from.

2) Translate the text in adventure_log/ by createing a new file and translating each line

3) Translate the text in english.idf by copying the file and translating each line.

4) Translate the text in dialog/english.ini by copying the file and translating each line.

NOTE: Currently supported glyphs:
1) Latin glyphs required for English, Spanish, French, Italian
2) Most of the common chinese words. But the glyphs are so small, that I'm not sure anyone would be able to read them. But who knows...
3) The most commonly used Japanese Kanji, along with Kana and Hiragana. Again, as with Chinese, the Kanji are tiny and illegible, so I guess a translation would rely mainly on Hiragana...
4) Cyrillic glyphs for the Russian language
5) Some weird and/or accented characters for Portuguese and Nordic languages

NOTE: The text files must be encoded in UTF-8.
NOTE: Try to avoid modified versions of the ASCII comma character, period, question mark, and stuff like that. I've made an effort to support some of these alternate localized punctuation marks, but I may have missed some.
