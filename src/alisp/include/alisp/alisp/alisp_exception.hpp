#pragma once

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"


#include <fmt/format.h>
#include <rang.hpp>

namespace alisp
{


enum class SignalTag {
    UNKNOWN,
    PARSER,
    EVAL,
    INVALID_ARGUMENTS,
    ENV,
    USER,
    FLOW_CONTROL,

};


constexpr const char* signal_tag_to_string( SignalTag type){
    constexpr const char* const names[] = {"unknown", "parser", "eval", "invalid_arguments", "env", "user"};
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
    const std::string& name() const { return m_signal_name; }

  protected:
    SignalTag m_tag;
    std::string m_signal_name;

};


struct signal_exception : public al_exception
{

    signal_exception(ALObjectPtr sym, ALObjectPtr list) :
        al_exception(format(sym, list), SignalTag::USER), m_sym(sym), m_list(list) {

        m_signal_name = m_sym->to_string();
    }

  private:
    ALObjectPtr m_sym;
    ALObjectPtr m_list;

    static std::string format(ALObjectPtr sym, ALObjectPtr list){
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

    parse_exception(const std::string& t_why, const FileLocation& t_where, const std::string& t_input) :
        al_exception(format(t_why, t_where, t_input), SignalTag::PARSER)
    {
        m_signal_name = "parser-signal";
    }


  private:
    static std::string format(const std::string& t_why, const FileLocation& t_where, const std::string& t_input)
    {
        const static int LINE_CONTEXT = 1;
        std::ostringstream ss;

        ss << "\t" << "In file \'" << t_where.file << '\'' << ": " << "line: " << t_where.line << ", col: " << t_where.col << '\n';
        ss << "\t" << t_why << "\n";

        auto lines = utility::split(t_input, '\n');

        auto start_index = static_cast<int>(t_where.line) - LINE_CONTEXT < 0 ? 0 : static_cast<int>(t_where.line) - LINE_CONTEXT;
        auto end_index = (static_cast<int>(t_where.line) + LINE_CONTEXT) > static_cast<int>(std::size(lines)) ?
            static_cast<int>(std::size(lines)) : static_cast<int>(std::size(lines)) + LINE_CONTEXT;

        for (auto i = static_cast<size_t>(start_index); i < static_cast<size_t>(end_index); ++i) {
            ss << "\t" <<  i << " |" << "\t" << lines[i] << "\n";
        }

        return ss.str();
    }

};


struct environment_error : public al_exception
{
  public:
    environment_error(const std::string& t_why) : al_exception(t_why, SignalTag::ENV) {
        m_signal_name = "environment-signal";
    }

};


struct eval_error : public al_exception
{
  public:
    eval_error(const std::string& t_why) : al_exception(t_why, SignalTag::EVAL) {
        m_signal_name = "eval-signal";
    }
};


struct argument_error : public al_exception
{
  public:
    argument_error(const std::string& t_why) : al_exception(t_why, SignalTag::INVALID_ARGUMENTS) {
        m_signal_name = "eval-signal";
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

struct al_return  : public al_exception
{
  public:
    al_return(ALObjectPtr value) : al_exception("", SignalTag::FLOW_CONTROL), m_value(value) {}

    ALObjectPtr value() { return m_value; }
  private:
    ALObjectPtr m_value;
};



/*  _   _ _   _ _  */
/* | | | | |_(_) | */
/* | | | | __| | | */
/* | |_| | |_| | | */
/*  \___/ \__|_|_| */


template<bool should_exit = false>
void handle_errors_lippincott(){

    try {
        throw;
    } catch (parse_exception& p_exc) {
        std::cout << rang::fg::red << "Unhandeled parser signal: <"
                  << signal_tag_to_string(p_exc.tag()) << ", " << p_exc.name()  << ">\n" << rang::fg::reset;
        std::cout << p_exc.what() << "\n";
    } catch (environment_error& p_exc) {
        std::cout << rang::fg::red << "Unhandeled environment signal:<"
                  << signal_tag_to_string(p_exc.tag()) << ", " << p_exc.name()  << ">\n" << rang::fg::reset;
        std::cout << p_exc.what() << "\n";
    } catch (eval_error& p_exc) {
        std::cout << rang::fg::red << "Unhandeled evaluation signal:<"
                  << signal_tag_to_string(p_exc.tag()) << ", " << p_exc.name()  << ">\n" << rang::fg::reset;
        std::cout << p_exc.what() << "\n";
    } catch (signal_exception& p_exc) {
        std::cout << rang::fg::red << "Unhandeled user signal:<"
                  << signal_tag_to_string(p_exc.tag()) << ", " << p_exc.name()  << ">\n" << rang::fg::reset;
        std::cout << p_exc.what() << "\n";
    }  catch (argument_error& p_exc) {
        std::cout << rang::fg::red << "Invalid Arguments error:<"
                  << signal_tag_to_string(p_exc.tag()) << ", " << p_exc.name()  << ">\n" << rang::fg::reset;
        std::cout << p_exc.what() << "\n";
    }
    

    if constexpr (should_exit) { exit(1); }

}

}
