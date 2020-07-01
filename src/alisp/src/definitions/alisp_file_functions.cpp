#include <algorithm>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_pattern_matching.hpp"
#include "alisp/alisp/alisp_files.hpp"

#include "alisp/utility/macros.hpp"

#include "alisp/alisp/declarations/files.hpp"

namespace alisp
{


struct Sfile_open
{
    inline static const std::string name = "file-open";

    inline static const std::string doc{ R"((file-open PATH [:out] [:in])

Open a file from the filesystem. If `:out` is specified, the file will
be opened for writing. If `:in` is specified the file will be opened
for reading. Provind both keyword arguemnts is also possible. The
function returns a resrouse descriptor that can be used to access the underlying file.

```elisp
(defvar file-1 (file-open "./file-1.al" :out)
(defvar file-2 (file-open "./file-2.al" :in)
```
)))" };

    static ALObjectPtr Ffile_open(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_min_size<1>(t_obj));
        auto name = eval_check(eval, t_obj, 0, &assert_string<size_t>);


        auto output = contains(t_obj, ":out") ? Qt : Qnil;
        auto input  = contains(t_obj, ":in") ? Qt : Qnil;

        auto file_obj = FileHelpers::open_file(name, output, input);

        return file_obj;
    }
};

struct Sfile_close
{
    inline static const std::string name = "file-close";

    inline static const std::string doc{ R"((file-close FILE)

Close an opened file and release the file descriptor. `FILE` should be
a valid resource descriptor pointing to a file.

```elisp
(defvar file-1 (file-open "./file-1.al" :out)
(file-close file-1)
```
))" };

    static ALObjectPtr Ffile_close(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto file = eval_check(eval, t_obj, 0, &assert_file<size_t>);

        FileHelpers::close_file(file);
        return Qt;
    }
};

struct Sfile_read_line
{
    inline static const std::string name = "file-read-line";

    inline static const std::string doc{ R"((file-read-line FILE)

Read a single line from a file an return it.`FILE` should be a valid
resource descriptor pointing to a file. This function also moves the
position of the underlying file stream after the read line.

)" };

    static ALObjectPtr Ffile_read_line(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {

        AL_CHECK(assert_size<1>(t_obj));
        auto file = eval_check(eval, t_obj, 0, &assert_file<size_t>);

        auto &file_obj = FileHelpers::get_file(file);

        if (file_obj.m_input and !file_obj.m_file.eof())
        {
            std::string line;
            std::getline(file_obj.m_file, line);
            return make_string(line);
        }

        return Qnil;
    }
};

struct Sfile_write_line
{
    inline static const std::string name = "file-write-line";

    inline static const std::string doc{ R"((file-write-line FILE STRING)

Write `STRING` to a file, followed by a new line. `FILE` should be
a valid resource descriptor pointing to a file.

)" };

    static ALObjectPtr Ffile_write_line(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {

        AL_CHECK(assert_size<2>(t_obj));
        auto file = eval_check(eval, t_obj, 0, &assert_file<size_t>);
        auto line = eval_check(eval, t_obj, 1, &assert_string<size_t>);

        auto &file_obj = FileHelpers::get_file(file);

        if (file_obj.m_output)
        {
            file_obj.m_file << line->to_string() << '\n';
            return Qt;
        }

        return Qnil;
    }
};

struct Sfile_has_more
{
    inline static const std::string name = "file-has-more";

    inline static const std::string doc{ R"((file-has-more FILE)

Check if there is more data to read of a `FILE`. `FILE` should be a
valid resource descriptor pointing to a file. Return `t` if the stream
pointer has reached to the end of the file and `nil` otherwise.
)" };

    static ALObjectPtr Ffile_has_more(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {

        AL_CHECK(assert_size<1>(t_obj));

        auto file = eval_check(eval, t_obj, 0, &assert_file<size_t>);

        auto &file_obj = FileHelpers::get_file(file);

        return !file_obj.m_file.eof() ? Qt : Qnil;
    }
};

}  // namespace alisp
