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


#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/utility/defines.hpp"


namespace alisp
{

namespace detail
{


#ifdef ALISP_WIN
inline constexpr auto separator = "\\";
#else
inline constexpr auto separator = "/";
#endif



ALObjectPtr Froot(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fdirectories(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fentries(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fglob(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Ftouch(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fcopy_contest(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fcopy(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fmove(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fmake_symlink(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fdelete(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fmkdir(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Ftemp_file(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fread_bytes(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fread_text(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fwrite_text(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fwrite_bytes(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fappend_text(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fappend_bytes(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fjoin(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<2>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fsplit(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fexpand(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Ffilename(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fdirname(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fcommon_parent(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fext(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fno_ext(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fswap_ext(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fbase(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Frelative(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fshort(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Flong(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fcanonical(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Ffull(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fexists(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fdirecotry(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Ffile(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fsymlink(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Freadable(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fwritable(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fexecutable(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fabsolute(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fprelative(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fis_Froot(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fexit(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fsame(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fparent_of(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fchild_of(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fancestor_of(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fdescendant_of(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fhidden(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fempty(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

}  // namespace detail

env::ModulePtr init_fileio(env::Environment *, eval::Evaluator *)
{

    auto Mfileio = module_init("fileio");
    auto fio_ptr = Mfileio.get();

    module_defvar(fio_ptr, "directory-separator", make_string(detail::separator));


    return Mfileio;
}


}  // namespace alisp
