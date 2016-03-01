Design considerations
=====================

Safety guarantees
-----------------

- Multi-threading safe
- Should be safe when interrupting threads, including by Thread.stop

Limits
------

- Maximal cyclotomic order is Int.MaxValue / 2 - 1 =
