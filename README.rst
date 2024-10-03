Pairmemo
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pairmemo is an R package for flexible on-disk function memoization. It allows you to cache function calls based on their arguments. Compared to ``memoise`` and ``targets``, Pairmemo emphasizes manual cache management. Caching is keyed by the function's name and arguments but not by its body, so you can choose which cache entries to delete (if any) when altering the function. You can even add new parameters without invalidating the cache, if you give them default arguments. These features make Pairmemo appropriate for large projects where having to rerun code unnecessarily could take days, while Pairmemo's interface remains convenient for small tasks. Pairmemo also stores its calls on disk in a simple fashion that makes them easy to work with outside of Pairmemo, or outside of R entirely.

Usage
============================================================

Install the package with ``remotes::install_github``. Run the tests with ``testthat::test_dir("tests")`` or ``devtools::check()``. `Read the documentation online <https://arfer.net/code/pairmemo>`__, or in R with ``?`` or ``help``.

License
============================================================

This program is copyright 2018 – 2024 Kodi B. Arfer.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the `GNU General Public License`_ for more details.

.. _`GNU General Public License`: http://www.gnu.org/licenses/
