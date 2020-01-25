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

#include <fmt/format.h>
#include <rang.hpp>

#include "alisp/alisp/alisp_common.hpp"

#include "alisp/utility.hpp"


namespace alisp
{


enum class SignalTag
{
    UNKNOWN,
    PARSER,
    EVAL,
    INVALID_ARGUMENTS,
    ENV,
    USER,
    MODULE_IMPORT,
    MODULE_REFERNCE,
    ILLEGAL_NAME,
    FLOW_CONTROL,

};


constexpr const char *signal_tag_to_string(SignalTag type)
{
    constexpr const char *const names[] = { "unknown", "parser", "eval", "invalid_arguments", "env", "module_import", "module_reference", "user", "illegal_name" };
    return names[static_cast<int>(type)];
}


/*  _____                          */
/* | ____|_ __ _ __ ___  _ __ ___  */
/* |  _| | '__| '__/ _ \| '__/ __| */
/* | |___| |  | | | (_) | |  \__ \ */
/* |_____|_|  |_|  \___/|_|  |___/ */


struct al_exception : public std::runtime_error
{
    al_exception(std::string what, SignalTag tag) : runtime_error(what), m_tag(tag) {}


    SignalTag tag() const { return m_tag; }
    const std::string &name() const { return m_signal_name; }

  protected:
    SignalTag m_tag;
    std::string m_signal_name;
};

struct signal_exception : public al_exception
{

    signal_exception(ALObjectPtr sym, ALObjectPtr list) : al_exception(format(sym, list), SignalTag::USER), m_sym(sym), m_list(list)
    {

        m_signal_name = m_sym->to_string();
    }

  private:
    ALObjectPtr m_sym;
    ALObjectPtr m_list;

    static std::string format(ALObjectPtr sym, ALObjectPtr list)
    {
        std::ostringstream ss;
        ss << "Signal error <" << dump(sym) << "> :";
        ss << dump(list);
        ss << '\n';
        return ss.str();
    }
};

struct parse_exception : public al_exception
{
  public:
    parse_exception(const std::string &t_why, const FileLocation &t_where, const std::string &t_input)
      : al_exception(format(t_why, t_where, t_input), SignalTag::PARSER)
    {
        m_signal_name = "parser-signal";
    }


  private:
    static std::string format(const std::string &t_why, const FileLocation &t_where, const std::string &t_input)
    {
        const static int LINE_CONTEXT = 1;
        std::ostringstream ss;

        ss << "\t"
           << "In file \'" << t_where.file << '\'' << ": "
           << "line: " << t_where.line << ", col: " << t_where.col << '\n';
        ss << "\t" << t_why << "\n";

        auto lines = utility::split(t_input, '\n');

        auto start_index = static_cast<int>(t_where.line) - LINE_CONTEXT < 0 ? 0 : static_cast<int>(t_where.line) - LINE_CONTEXT;
        auto end_index   = (static_cast<int>(t_where.line) + LINE_CONTEXT) > static_cast<int>(std::size(lines))
                           ? static_cast<int>(std::size(lines))
                           : static_cast<int>(std::size(lines)) + LINE_CONTEXT;

        for (auto i = static_cast<size_t>(start_index); i < static_cast<size_t>(end_index); ++i)
        {
            ss << "\t" << i << " |"
               << "\t" << lines[i] << "\n";
        }

        return ss.str();
    }
};

struct environment_error : public al_exception
{
  public:
    environment_error(const std::string &t_why) : al_exception(t_why, SignalTag::ENV) { m_signal_name = "environment-signal"; }
};

struct eval_error : public al_exception
{
  public:
    eval_error(const std::string &t_why) : al_exception(t_why, SignalTag::EVAL) { m_signal_name = "eval-signal"; }
};

struct argument_error : public al_exception
{
  public:
    argument_error(const std::string &t_why) : al_exception(t_why, SignalTag::INVALID_ARGUMENTS) { m_signal_name = "eval-signal"; }
};

struct module_error : public al_exception
{
  private:
    std::string m_module;

  public:
    module_error(std::string t_module, const std::string& t_why) :
        al_exception(format(t_module,  t_why), SignalTag::MODULE_IMPORT),
        m_module(std::move(t_module))
    {
        m_signal_name = "module-signal";
    }


    static std::string format(std::string t_module, const std::string t_why)
    {
        std::ostringstream ss;
        ss << "Module import error when loading " << t_module << ":\n\t" << t_why;
        ss << '\n';
        return ss.str();
    }
};

struct module_refence_error : public al_exception
{


  public:
    module_refence_error(std::string t_module, const std::string& t_reference, bool symbol = false) :
        al_exception(format(t_module,  t_reference, symbol), SignalTag::MODULE_REFERNCE)
    {
        m_signal_name = "module-signal";
    }


    static std::string format(std::string t_module, const std::string t_ref, bool t_symbol)
    {
        std::ostringstream ss;
        ss << "Module reference error. \n\tThe module " << t_module << " does not contain a refernce to the ";
        if (t_symbol ) {
            ss << "symbol " << t_ref ;
        } else  {
            ss << "module " << t_ref ;
        }
        ss << '\n';
        return ss.str();
    }
};

struct illegal_name_error : public al_exception
{
    
  public:
    illegal_name_error(const std::string& t_name, const std::string& t_why) :
        al_exception(format(t_why, t_name), SignalTag::ILLEGAL_NAME)
    {
        m_signal_name = "illegal-name-signal";
    }

    
    static std::string format(std::string t_why, const std::string& t_name)
    {
        std::ostringstream ss;
        ss << "Invalid name error for \"" << t_name << "\"\t" << t_why << '\n';
        return ss.str();
    }

};

struct interrupt_error : public al_exception
{
    
  public:
    interrupt_error() :
        al_exception("KeyboardInterrupt", SignalTag::ILLEGAL_NAME)
    {
        m_signal_name = "interrupt-signal";
    }    

};


/*  _____ _                  ____            _             _  */
/* |  ___| | _____      __  / ___|___  _ __ | |_ _ __ ___ | | */
/* | |_  | |/ _ \ \ /\ / / | |   / _ \| '_ \| __| '__/ _ \| | */
/* |  _| | | (_) \ V  V /  | |__| (_) | | | | |_| | | (_) | | */
/* |_|   |_|\___/ \_/\_/    \____\___/|_| |_|\__|_|  \___/|_| */


struct al_continue : public al_exception
{
  public:
    al_continue() : al_exception("", SignalTag::FLOW_CONTROL) {}
};

struct al_break : public al_exception
{
  public:
    al_break() : al_exception("", SignalTag::FLOW_CONTROL) {}
};

struct al_return : public al_exception
{
  public:
    al_return(ALObjectPtr value) : al_exception("", SignalTag::FLOW_CONTROL), m_value(value) {}

    ALObjectPtr value() { return m_value; }

  private:
    ALObjectPtr m_value;
};




struct al_exit : public al_exception
{
  public:
    al_exit(int value) : al_exception("", SignalTag::FLOW_CONTROL), m_value(value) {}
    int value() { return m_value; }
  private:
    int m_value;
};


/*  _   _ _   _ _  */
/* | | | | |_(_) | */
/* | | | | __| | | */
/* | |_| | |_| | | */
/*  \___/ \__|_|_| */

template<bool should_exit = false> void handle_errors_lippincott()
{

    try
    {
        throw;
    }
    catch (parse_exception &p_exc)
    {
        std::cout << rang::fg::red << "Unhandeled parser signal: <" << signal_tag_to_string(p_exc.tag()) << ", " << p_exc.name() << ">\n"
                  << rang::fg::reset;
        std::cout << '\t' << p_exc.what() << "\n";
    }
    catch (environment_error &p_exc)
    {
        std::cout << rang::fg::red << "Unhandeled environment signal:<" << signal_tag_to_string(p_exc.tag()) << ", " << p_exc.name() << ">\n"
                  << rang::fg::reset;
        std::cout << '\t' << p_exc.what() << "\n";
    }
    catch (eval_error &p_exc)
    {
        std::cout << rang::fg::red << "Unhandeled evaluation signal:<" << signal_tag_to_string(p_exc.tag()) << ", " << p_exc.name() << ">\n"
                  << rang::fg::reset;
        std::cout << '\t' << p_exc.what() << "\n";
    }
    catch (signal_exception &p_exc)
    {
        std::cout << rang::fg::red << "Unhandeled user signal:<" << signal_tag_to_string(p_exc.tag()) << ", " << p_exc.name() << ">\n"
                  << rang::fg::reset;
        std::cout << '\t' << p_exc.what() << "\n";
    }
    catch (argument_error &p_exc)
    {
        std::cout << rang::fg::red << "Invalid Arguments error:<" << signal_tag_to_string(p_exc.tag()) << ", " << p_exc.name() << ">\n"
                  << rang::fg::reset;
        std::cout << '\t' << p_exc.what() << "\n";
    }
    catch (module_error &p_exc)
    {
        std::cout << rang::fg::red << "Module error:.\n" << rang::fg::reset;
        std::cout << '\t' << p_exc.what() << "\n";
    }
    catch (module_refence_error &p_exc)
    {
        std::cout << rang::fg::red << "Reference error:.\n" << rang::fg::reset;
        std::cout << '\t' << p_exc.what() << "\n";
    }
    catch (alobject_error &p_exc)
    {
        std::cout << rang::fg::red << "ALObject exception. This is not normal. Report bug or something.\n" << rang::fg::reset;
        std::cout << '\t' << p_exc.what() << "\n";
    }
    catch (interrupt_error &p_exc)
    {
        std::cout << rang::fg::red << "Interrupt Exception." << rang::fg::reset;
    }

    if constexpr (should_exit) { exit(1); }
}

}  // namespace alisp
