#pragma once

#include <string>
#include <sstream>
#include <variant>
#include <vector>
#include <iterator>
#include <bitset>
#include <cstdint>
#include <utility>
#include <memory>
#include <filesystem>
#include <fstream>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_factory.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_parser.hpp"


namespace alisp
{

namespace detail
{


}


class LanguageEngine
{
  private:
    
    env::Environment m_environment;
    eval::Evaluator m_evaluator;
    parser::ALParser<env::Environment> m_parser;


    void do_eval(std::string& t_input, const std::string& t_file)
    {
        // if (opts.eval_debug) std::cout << "DEUBG[EVAL]: " << alisp::dump(eval_res) << "\n";
        // if (opts.parse_debug) std::cout << "DEUBG[PARSER]: " << alisp::dump(p) << "\n";
        
        auto parse_result = m_parser.parse(t_input, t_file);
        for (auto sexp : parse_result ) {
            auto eval_result = m_evaluator.eval(sexp);
            std::cout << *eval_result << "\n";
        }
    }

        
  public:

    static bool skip_bom(std::ifstream &infile) {
        size_t bytes_needed = 3;
        char buffer[3];

        memset(buffer, '\0', bytes_needed);

        infile.read(buffer, static_cast<std::streamsize>(bytes_needed));

        if ((buffer[0] == '\xef')
            && (buffer[1] == '\xbb')
            && (buffer[2] == '\xbf')) {

            infile.seekg(3);
            return true;
        }

        infile.seekg(0);

        return false;
    }

    static std::string load_file(const std::string &t_filename) {
        std::ifstream infile(t_filename.c_str(), std::ios::in | std::ios::ate | std::ios::binary );

        if (!infile.is_open()) {}

        auto size = infile.tellg();
        infile.seekg(0, std::ios::beg);

        assert(size >= 0);
        
        if (skip_bom(infile)) {
            size-=3;
            assert(size >= 0);
        }

        if (size == std::streampos(0))
        {
            return std::string();
        } else {
            std::vector<char> v(static_cast<size_t>(size));
            infile.read(&v[0], static_cast<std::streamsize>(size));
            return std::string(v.begin(), v.end());
        }
    }

    
    LanguageEngine() : m_environment(),
                       m_evaluator(m_environment),
                       m_parser(m_environment)
    {
        init_system();
    }

    void init_system()
    {
        
    }


    void eval_statement(std::string& command)
    {
        try {
            do_eval(command, "__EVAL__");
        }catch (...) {
            handle_errors_lippincott<false>();
        }

    }

    
    void eval_file(const std::filesystem::path& t_path)
    {
        try {
            auto file_content = load_file(t_path);
            do_eval(file_content, t_path);
        }catch (...) {
            handle_errors_lippincott<true>();
        }
    }

    
};


}
