![](https://github.com/marcoantoniocorallo/Fhree/blob/main/cover_1.png)

---

# EsoCaml

*EsoCaml* is a simple esoteric programming language, quickly developed for funny.
To be honest, this is an esoteric front-end for [a simple PL]([GitHub - marcoantoniocorallo/Fhree: Fhree is a small strongly typed functional language, it is interpreted and it implements the big-step operational semantics.](https://github.com/marcoantoniocorallo/Fhree)) I developed early.
Thus, this is a **functional** *EsoLang* inspired by [*reMorse*]([reMorse - Esolang](https://esolangs.org/wiki/ReMorse)) with an *OCaml*-like back-end.



Each constant and identifier must be written in morse code, and the constants must begin with 3 letters that specify the type.
If there is no type at the beginning of the word, It will be treated as an identifier.

**Morse Code**

| Character | Morse Code |
| --------- | ---------- |
| A         | .-         |
| B         | -...       |
| C         | -.-.       |
| D         | -..        |
| E         | .          |
| F         | ..-.       |
| G         | --.        |
| H         | ....       |
| I         | ..         |
| J         | .---       |
| K         | -.-        |
| L         | .-..       |
| M         | --         |
| N         | -.         |
| O         | ---        |
| P         | .--.       |
| Q         | --.-       |
| R         | .-.        |
| S         | ...        |
| T         | -          |
| U         | ..-        |
| V         | ...-       |
| W         | .--        |
| X         | -..-       |
| Y         | -.--       |
| Z         | --..       |
| 0         | -----      |
| 1         | .----      |
| 2         | ..---      |
| 3         | ...--      |
| 4         | ....-      |
| 5         | .....      |
| 6         | -....      |
| 7         | --...      |
| 8         | ---..      |
| 9         | ----.      |
| .         | .-.-.-     |
| ,         | --..--     |
| ?         | ..--..     |
| '         | .----.     |
| !         | -.-.--     |
| /         | -..-.      |
| (         | -.--.      |
| )         | -.--.-     |
| &         | .-...      |
| :         | ---...     |
| ;         | -.-.-.     |
| =         | -...-      |
| +         | .-.-.      |
| -         | -....-     |
| _         | ..--.-     |
| "         | .-..-.     |
| $         | ...-..-    |
| @         | .--.-.     |

**Literals**

| Literal Type | Prefix | Example       | Value       | Morse                                                |
| ------------ | ------ | ------------- | ----------- | ---------------------------------------------------- |
| Int          | INT    | INT42         | 42          | .. -. - ....- ..--- \|                               |
| Char         | CHR    | CHRT          | 'T'         | -.-. .... .-. -                                      |
| Float        | FLT    | FLT0.18       | 0.18        | ..-. .-.. - ----- .-.-.- .---- ---..                 |
| Bool         | BOL    | BOOLTRUE      | true        | -... --- --- .-.. - .-. ..- .                        |
| String       | STR    | STRHELLO_WORD | Hello World | ... - .-. .... . .-.. .-.. ---__.-- --- .-. .-.. -.. |

Keywords, Operators and types (for type annotations) are divided in three *rotating wheels*:

| Keywords | Operators | Type Annotation |
| -------- | --------- | --------------- |
| LET      | +         | int             |
| IN       | -         | char            |
| IF       | *         | float           |
| THEN     | /         | bool            |
| ELSE     | +.        | string          |
| FUN      | -.        | list            |
| LAMBDA   | *.        |                 |
| NOT      | /.        |                 |
| AND      | ==        |                 |
| OR       | !=        |                 |
| PROJ     | <         |                 |
| HEAD     | >         |                 |
| TAIL     | <=        |                 |
|          | >=        |                 |
|          | ^         |                 |
|          | ::        |                 |
|          | |>        |                 |

You can handle the rotating wheels by an unique, shared, index.

| Operator | Effect                                |
| -------- | ------------------------------------- |
| `>`      | Increase the index by 1               |
| `!`      | Enter the current **keyword**         |
| `@`      | Enter the current **operator**        |
| `#`      | Enter the current **type annotation** |

The index begins with 0 and is restarted when a keyword, an operator or a type is chosen.



In addition to these keywords and operators, you can use the following special ones:

| Symbol | Meaning                           |
| ------ | --------------------------------- |
| `,`    | For constructing tuples and lists |
| `(`    | Left parenthesis for tuples       |
| `)`    | Right parenthesis for tuples      |
| `[`    | Left bracket for lists            |
| `]`    | Right bracket for lists           |
| `:`    | For type annotations              |
| `->`   | For functional types              |

For simplicity, you can use the character `|` as separator between morse code statements and the character `_` as space between two morse code words.



Since the only legal characters are: `.` `-` `>` `!` `@` `#` `,` `(` `)` `[` `]` `:` `_`, every other character will be ignored and can be used **for comments**.



Due to the complexity of this language, it has been tested very little.
EsoCaml is just a toy language, but if you want to play with it, please, let me know what do you thing about it! :)

---
