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

#include <iostream>
#include <string_view>
#include <string>
#include <sstream>
#include <cstdio>
#include <memory>
#include <fstream>

namespace alisp
{

namespace streams
{


class ALStream
{
  private:
  public:
    virtual void write(const std::string &t_input)      = 0;
    virtual void write(const std::string_view &t_input) = 0;
    virtual void write(const char *c_str)               = 0;
    virtual void write(char c)                          = 0;

    virtual int get_char()                     = 0;
    virtual std::string get_chars(size_t cout) = 0;
    virtual std::string get_line()             = 0;

    // virtual size_t tellg() = 0;
    // virtual void seekg(size_t cout) = 0;

    virtual bool hasmore() = 0;


    virtual std::string content() = 0;
    virtual ~ALStream() {}
};

class CoutStream : public ALStream
{
  private:
    inline static std::unique_ptr<CoutStream> m_instance;

  public:
    static CoutStream *get_instance()
    {
        if (!m_instance)
        {
            m_instance = std::make_unique<CoutStream>();
        }
        return m_instance.get();
    }

    CoutStream() {}

    void write(const std::string &t_input) override { std::cout << t_input; }
    void write(const std::string_view &t_input) override { std::cout << t_input; }
    void write(const char *c_str) override { std::cout << c_str; }
    void write(char c) override { std::putchar(c); }

    int get_char() override { return 0; }
    std::string get_chars(size_t) override { return ""; }
    std::string get_line() override { return ""; }

    bool hasmore() override { return true; }

    std::string content() override { return ""; }
};

class CerrStream : public ALStream
{
  private:
    inline static std::unique_ptr<CerrStream> m_instance;

  public:
    static CerrStream *get_instance()
    {
        if (!m_instance)
        {
            m_instance = std::make_unique<CerrStream>();
        }
        return m_instance.get();
    }

    CerrStream() {}

    void write(const std::string &t_input) override { std::cerr << t_input; }
    void write(const std::string_view &t_input) override { std::cerr << t_input; }
    void write(const char *c_str) override { std::cerr << c_str; }
    void write(char c) override { std::putc(c, stderr); }

    int get_char() override { return 0; }
    std::string get_chars(size_t) override { return ""; }
    std::string get_line() override { return ""; }

    bool hasmore() override { return true; }

    std::string content() override { return ""; }
};

class CinStream : public ALStream
{
  private:
    inline static std::unique_ptr<CinStream> m_instance;

  public:
    static CinStream *get_instance()
    {
        if (!m_instance)
        {
            m_instance = std::make_unique<CinStream>();
        }
        return m_instance.get();
    }
    CinStream() {}

    void write(const std::string &) override {}
    void write(const std::string_view &) override {}
    void write(const char *) override {}
    void write(char) override {}

    int get_char() override { return std::getc(stdin); }

    std::string get_chars(size_t count) override
    {
        std::string str;

        for (size_t i = 0; i < count; ++i)
        {
            str.push_back(static_cast<char>(std::getchar()));
        }

        return str;
    }

    std::string get_line() override
    {
        std::string str;
        std::getline(std::cin, str);
        return str;
    }


    bool hasmore() override { return true; }


    std::string content() override { return ""; }
};

class FileStream : public ALStream
{
  private:
    std::fstream &m_stream;

  public:
    explicit FileStream(std::fstream &t_stream) : m_stream(t_stream) {}


    void write(const std::string &t_input) override { m_stream << t_input; }
    void write(const std::string_view &t_input) override { m_stream << t_input; }
    void write(const char *c_str) override { m_stream << c_str; }
    void write(char c) override { m_stream.put(c); }

    int get_char() override { return m_stream.get(); }

    std::string get_chars(size_t count) override
    {
        std::string str;

        for (size_t i = 0; i < count; ++i)
        {
            str.push_back(static_cast<char>(m_stream.get()));
        }

        return str;
    }

    std::string get_line() override
    {
        std::string str;
        std::getline(m_stream, str);
        return str;
    }

    bool hasmore() override { return !m_stream.eof(); }

    std::string content() override
    {
        std::stringstream buffer;
        buffer << m_stream.rdbuf();
        return buffer.str();
    }
};

class StringStream : public ALStream
{
  private:
    std::string m_str;
    size_t m_pos;

  public:
    StringStream() : m_str(), m_pos(0) {}
    StringStream(std::string &t_str) : m_str(t_str), m_pos(0) {}

    void write(const std::string &t_input) override { m_str += t_input; }
    void write(const std::string_view &t_input) override { m_str.append(t_input.data()); }
    void write(const char *c_str) override { m_str.append(c_str); }
    void write(char c) override { m_str.push_back(c); }

    int get_char() override { return m_str[m_pos++]; }

    std::string get_chars(size_t count) override
    {
        std::string str;
        size_t fin = m_pos + count;
        for (auto i = m_pos; i < fin and i < std::size(m_str); ++i)
        {
            char ch = m_str[i];
            ++m_pos;
            str.push_back(ch);
        }
        return str;
    }

    std::string get_line() override
    {
        std::string str;
        for (auto i = m_pos; i < std::size(m_str); ++i)
        {
            char ch = m_str[i];
            if (ch == '\n')
            {
                ++m_pos;
                break;
            }
            ++m_pos;
            str.push_back(ch);
        }
        return str;
    }

    bool hasmore() override { return m_pos < std::size(m_str); }

    std::string content() override { return m_str; }
};


inline ALStream &operator<<(ALStream &os, char c)
{
    os.write(c);
    return os;
}

inline ALStream &operator<<(ALStream &os, int c)
{
    os.write(std::to_string(c));
    return os;
}

inline ALStream &operator<<(ALStream &os, long c)
{
    os.write(std::to_string(c));
    return os;
}

inline ALStream &operator<<(ALStream &os, short c)
{
    os.write(std::to_string(c));
    return os;
}

inline ALStream &operator<<(ALStream &os, long long c)
{
    os.write(std::to_string(c));
    return os;
}

inline ALStream &operator<<(ALStream &os, float c)
{
    os.write(std::to_string(c));
    return os;
}

inline ALStream &operator<<(ALStream &os, double c)
{
    os.write(std::to_string(c));
    return os;
}

inline ALStream &operator<<(ALStream &os, const char *c)
{
    os.write(c);
    return os;
}

inline ALStream &operator<<(ALStream &os, const std::string &c)
{
    os.write(c);
    return os;
}

inline ALStream &operator<<(ALStream &os, const std::string_view &c)
{
    os.write(c);
    return os;
}

}  // namespace streams

}  // namespace alisp
