%heading#macros The Macro Tables

These tables document the macros for the standard environment and for the
[examples/re-nginx-log-processor](../re-examples/nginx-log-processor.lhs).

The main tables have the following columns.

| column             | content                                               |
|--------------------|-------------------------------------------------------|
| **name**           | name of the macro                                     |
| **caps**           | the number of captures in the RE                      |
| **regex**          | the RE (with macros)                                  |
| **examples**       | examples that should be matched by the RE             |
| **anti-examples**  | examples that should not be match by the RE           |
| **fails**          | messages for failing tests (should be empty)          |
| **parser**         | name of Haskell parser for the matched text (optional)|
| **comment**        | notes                                                 |

**These tables best viewed raw.**

  * The [Prelude macros for PCRE](pcre-macros.txt) (and the [macro sources](pcre-macros-src.txt))
  * The [Prelude macros for TDFA](tdfa-macros.txt) (and the [macro sources](tdfa-macros-src.txt))
  * The [nginx-log-processor macros (for PCRE)](pcre-nginx-log-processor.txt) ([and the macro sources](pcre-nginx-log-processor-src.txt))
