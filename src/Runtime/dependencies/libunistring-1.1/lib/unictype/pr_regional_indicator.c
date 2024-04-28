/* Properties of Unicode characters.
   Copyright (C) 2021-2022 Free Software Foundation, Inc.
   Written by Bruno Haible <bruno@clisp.org>, 2021.

   This file is free software.
   It is dual-licensed under "the GNU LGPLv3+ or the GNU GPLv2+".
   You can redistribute it and/or modify it under either
     - the terms of the GNU Lesser General Public License as published
       by the Free Software Foundation, either version 3, or (at your
       option) any later version, or
     - the terms of the GNU General Public License as published by the
       Free Software Foundation; either version 2, or (at your option)
       any later version, or
     - the same dual license "the GNU LGPLv3+ or the GNU GPLv2+".

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License and the GNU General Public License
   for more details.

   You should have received a copy of the GNU Lesser General Public
   License and of the GNU General Public License along with this
   program.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

/* Specification.  */
#include "unictype.h"

bool
uc_is_property_regional_indicator (ucs4_t uc)
{
  return (uc >= 0x1F1E6 && uc <= 0x1F1FF);
}

const uc_property_t UC_PROPERTY_REGIONAL_INDICATOR =
  { &uc_is_property_regional_indicator };
