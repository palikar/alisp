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

#include "alisp/alisp/alisp_module_helpers.hpp"

ALISP_EXPORT alisp::env::ModulePtr init_base64(alisp::env::Environment *, alisp::eval::Evaluator *);
ALISP_EXPORT alisp::env::ModulePtr init_fmt(alisp::env::Environment *, alisp::eval::Evaluator *);
ALISP_EXPORT alisp::env::ModulePtr init_func(alisp::env::Environment *, alisp::eval::Evaluator *);
ALISP_EXPORT alisp::env::ModulePtr init_http(alisp::env::Environment *, alisp::eval::Evaluator *);
ALISP_EXPORT alisp::env::ModulePtr init_json(alisp::env::Environment *, alisp::eval::Evaluator *);
ALISP_EXPORT alisp::env::ModulePtr init_locale(alisp::env::Environment *, alisp::eval::Evaluator *);
ALISP_EXPORT alisp::env::ModulePtr init_process(alisp::env::Environment *, alisp::eval::Evaluator *);
ALISP_EXPORT alisp::env::ModulePtr init_random(alisp::env::Environment *, alisp::eval::Evaluator *);
ALISP_EXPORT alisp::env::ModulePtr init_re(alisp::env::Environment *, alisp::eval::Evaluator *);
ALISP_EXPORT alisp::env::ModulePtr init_xml(alisp::env::Environment *, alisp::eval::Evaluator *);
