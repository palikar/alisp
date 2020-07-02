/*   Alisp - the alisp interpreted language
     Copyright (C) 2020 Stanislav Arnaudov

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any prior version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along
 with this program; if not, write to the Free Software Foundation, Inc.,
 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */

#pragma once


#include <robin_hood.h>


namespace alisp
{


template<typename Key, typename Value>
using Map = robin_hood::unordered_map<Key, Value>;


template<typename Key, typename Value>
using FlatMap = robin_hood::unordered_flat_map<Key, Value>;

template<typename Key, typename Value>
using NodeMap = robin_hood::unordered_node_map<Key, Value>;


template<typename Value>
using Set = robin_hood::unordered_set<Value>;


}
