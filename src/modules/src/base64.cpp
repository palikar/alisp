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

#include "alisp/config.hpp"
#include "alisp/alisp/alisp_module_helpers.hpp"


namespace base64
{
using namespace alisp;

namespace detail
{

struct Base64
{
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wuseless-cast"
#endif

    static std::string Encode(const std::string &data)
    {
        static constexpr char sEncodingTable[] = { 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
                                                   'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
                                                   'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
                                                   'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
                                                   '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/' };

        size_t in_len  = data.size();
        size_t out_len = 4 * ((in_len + 2) / 3);
        std::string ret(out_len, '\0');
        size_t i;
        char *p = const_cast<char *>(ret.c_str());

        for (i = 0; i < in_len - 2; i += 3)
        {
            *p++ = sEncodingTable[(data[i] >> 2) & 0x3F];
            *p++ = sEncodingTable[((data[i] & 0x3) << 4) | (static_cast<int>((data[i + 1] & 0xF0) >> 4))];
            *p++ = sEncodingTable[((data[i + 1] & 0xF) << 2) | (static_cast<int>((data[i + 2] & 0xC0) >> 6))];
            *p++ = sEncodingTable[data[i + 2] & 0x3F];
        }
        if (i < in_len)
        {
            *p++ = sEncodingTable[(data[i] >> 2) & 0x3F];
            if (i == (in_len - 1))
            {
                *p++ = sEncodingTable[((data[i] & 0x3) << 4)];
                *p++ = '=';
            }
            else
            {
                *p++ = sEncodingTable[((data[i] & 0x3) << 4) | (static_cast<int>((data[i + 1] & 0xF0) >> 4))];
                *p++ = sEncodingTable[((data[i + 1] & 0xF) << 2)];
            }
            *p++ = '=';
        }

        return ret;
    }

    static bool Decode(const std::string &input, std::string &out)
    {
        static constexpr unsigned char kDecodingTable[] = {
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 62, 64, 64, 64, 63, 52, 53, 54, 55,
            56, 57, 58, 59, 60, 61, 64, 64, 64, 64, 64, 64, 64, 0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12,
            13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 64, 64, 64, 64, 64, 64, 26, 27, 28, 29, 30, 31, 32,
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 64, 64, 64, 64, 64, 64, 64,
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64
        };

        size_t in_len = input.size();
        if (in_len % 4 != 0)
        {
            return false;
        }

        size_t out_len = in_len / 4 * 3;
        if (input[in_len - 1] == '=') out_len--;
        if (input[in_len - 2] == '=') out_len--;

        out.resize(out_len);

        for (size_t i = 0, j = 0; i < in_len;)
        {
            uint32_t a = input[i] == '=' ? 0 & i++ : kDecodingTable[static_cast<int>(input[i++])];
            uint32_t b = input[i] == '=' ? 0 & i++ : kDecodingTable[static_cast<int>(input[i++])];
            uint32_t c = input[i] == '=' ? 0 & i++ : kDecodingTable[static_cast<int>(input[i++])];
            uint32_t d = input[i] == '=' ? 0 & i++ : kDecodingTable[static_cast<int>(input[i++])];

            uint32_t triple = (a << 3 * 6) + (b << 2 * 6) + (c << 1 * 6) + (d << 0 * 6);

            if (j < out_len) out[j++] = (triple >> 2 * 8) & 0xFF;
            if (j < out_len) out[j++] = (triple >> 1 * 8) & 0xFF;
            if (j < out_len) out[j++] = (triple >> 0 * 8) & 0xFF;
        }

        return true;
    }


#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
};

struct Base16
{

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wuseless-cast"
#endif

    static std::string Encode(const std::string &input)
    {
        static constexpr char hex_digits[] = "0123456789ABCDEF";

        std::string output;
        output.reserve(input.length() * 2);
        for (unsigned char c : input)
        {
            output.push_back(hex_digits[c >> 4]);
            output.push_back(hex_digits[c & 15]);
        }
        return output;
    }

    static int hex_value(char hex_digit)
    {
        switch (hex_digit)
        {
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9': return hex_digit - '0';

            case 'A':
            case 'B':
            case 'C':
            case 'D':
            case 'E':
            case 'F': return hex_digit - 'A' + 10;

            case 'a':
            case 'b':
            case 'c':
            case 'd':
            case 'e':
            case 'f': return hex_digit - 'a' + 10;
        }
        return -1;
    }

    static bool Decode(const std::string &input, std::string &output)
    {
        size_t len = input.length();
        if (len & 1)
        {
            return false;
        }

        output.reserve(len / 2);
        for (auto it = input.begin(); it != input.end();)
        {
            int hi = hex_value(*it++);
            int lo = hex_value(*it++);
            output.push_back(hi << 4 | lo);
        }
        return true;
    }


#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
};

struct Base32
{

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wuseless-cast"
#pragma GCC diagnostic ignored "-Wold-style-cast"
#endif

    static bool Encode32Block(unsigned char *in5, unsigned char *out8)
    {
        // pack 5 bytes
        uint64_t buffer = 0;
        for (int i = 0; i < 5; i++)
        {
            if (i != 0)
            {
                buffer = (buffer << 8);
            }
            buffer = buffer | in5[i];
        }
        // output 8 bytes
        for (int j = 7; j >= 0; j--)
        {
            buffer          = buffer << (24 + (7 - j) * 5);
            buffer          = buffer >> (24 + (7 - j) * 5);
            unsigned char c = (unsigned char)(buffer >> (j * 5));
            // self check
            if (c >= 32) return false;
            out8[7 - j] = c;
        }
        return true;
    }

    static bool Decode32Block(unsigned char *in8, unsigned char *out5)
    {
        // pack 8 bytes
        uint64_t buffer = 0;
        for (int i = 0; i < 8; i++)
        {
            // input check
            if (in8[i] >= 32) return false;
            if (i != 0)
            {
                buffer = (buffer << 5);
            }
            buffer = buffer | in8[i];
        }
        // output 5 bytes
        for (int j = 4; j >= 0; j--)
        {
            out5[4 - j] = (unsigned char)(buffer >> (j * 8));
        }
        return true;
    }

    static bool Encode32(unsigned char *in, int inLen, unsigned char *out)
    {
        if ((in == 0) || (inLen <= 0) || (out == 0)) return false;

        int d = inLen / 5;
        int r = inLen % 5;

        unsigned char outBuff[8];

        for (int j = 0; j < d; j++)
        {
            if (!Encode32Block(&in[j * 5], &outBuff[0])) return false;
            memmove(&out[j * 8], &outBuff[0], sizeof(unsigned char) * 8);
        }

        unsigned char padd[5];
        memset(padd, 0, sizeof(unsigned char) * 5);
        for (int i = 0; i < r; i++)
        {
            padd[i] = in[inLen - r + i];
        }
        if (!Encode32Block(&padd[0], &outBuff[0])) return false;
        memmove(&out[d * 8], &outBuff[0], sizeof(unsigned char) * GetEncode32Length(r));

        return true;
    }

    static bool Decode32(unsigned char *in, int inLen, unsigned char *out)
    {
        if ((in == 0) || (inLen <= 0) || (out == 0)) return false;

        int d = inLen / 8;
        int r = inLen % 8;

        unsigned char outBuff[5];

        for (int j = 0; j < d; j++)
        {
            if (!Decode32Block(&in[j * 8], &outBuff[0])) return false;
            memmove(&out[j * 5], &outBuff[0], sizeof(unsigned char) * 5);
        }

        unsigned char padd[8];
        memset(padd, 0, sizeof(unsigned char) * 8);
        for (int i = 0; i < r; i++)
        {
            padd[i] = in[inLen - r + i];
        }
        if (!Decode32Block(&padd[0], &outBuff[0])) return false;

        memmove(&out[d * 5], &outBuff[0], sizeof(unsigned char) * GetDecode32Length(r));

        return true;
    }

    static int GetDecode32Length(int bytes)
    {
        int bits   = bytes * 5;
        int length = bits / 8;
        return length;
    }

    static int GetEncode32Length(int bytes)
    {
        int bits   = bytes * 8;
        int length = bits / 5;
        if ((bits % 5) > 0)
        {
            length++;
        }
        return length;
    }

    static bool Map32(unsigned char *inout32, int inout32Len, const char *alpha32)
    {
        if ((inout32 == 0) || (inout32Len <= 0) || (alpha32 == 0)) return false;
        for (int i = 0; i < inout32Len; i++)
        {
            if (inout32[i] >= 32) return false;
            inout32[i] = alpha32[inout32[i]];
        }
        return true;
    }

    static void ReverseMap(unsigned char *inAlpha32, unsigned char *outMap)
    {
        memset(outMap, 0, sizeof(unsigned char) * 256);
        for (int i = 0; i < 32; i++)
        {
            outMap[(int)inAlpha32[i]] = i;
        }
    }

    static bool Unmap32(unsigned char *inout32, int inout32Len, unsigned char *alpha32)
    {
        if ((inout32 == 0) || (inout32Len <= 0) || (alpha32 == 0)) return false;
        unsigned char rmap[256];
        ReverseMap(alpha32, rmap);
        for (int i = 0; i < inout32Len; i++)
        {
            inout32[i] = rmap[(int)inout32[i]];
        }
        return true;
    }

    static std::string Encode(const std::string &input)
    {
        static char alphabet[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
        int encodeLength       = GetEncode32Length(input.size());
        unsigned char *data32  = (unsigned char *)alloca(sizeof(char) * encodeLength);
        // unsigned char* data32 = new unsigned char[encodeLength];
        Encode32(reinterpret_cast<unsigned char *>(const_cast<char *>(input.data())), input.size(), data32);
        Map32(data32, encodeLength, &alphabet[0]);
        std::string s(reinterpret_cast<char *>(data32), encodeLength);
        return s;
    }

    static bool Decode(std::string input, std::string &output)
    {
        static char alphabet[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
        int encodeLength       = input.size();
        unsigned char *data32  = reinterpret_cast<unsigned char *>(const_cast<char *>(input.data()));
        Unmap32(data32, encodeLength, reinterpret_cast<unsigned char *>(&alphabet[0]));
        int decodeLength         = GetDecode32Length(input.size());
        unsigned char *decode256 = (unsigned char *)alloca(sizeof(char) * decodeLength);
        if (!Decode32(data32, encodeLength, decode256))
        {
            return false;
        }
        output = std::string(reinterpret_cast<char *>(decode256), decodeLength);
        return true;
    }


#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
};

}  // namespace detail

struct base64_encode_string
{
    inline static const std::string name{ "base64-encode-string" };

    inline static const Signature signature{ String{} };

    inline static const std::string doc{ R"((base64-encode-string STRING)

Return base64 encoded version of the string `STRING`.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));

        auto str = eval->eval(obj->i(0));
        AL_CHECK(assert_string(str));

        return make_string(detail::Base64::Encode(str->to_string()));
    }
};

struct base64_encode_bytes
{
    inline static const std::string name{ "base64-encode-bytes" };

    inline static const Signature signature{ ByteArray{} };

    inline static const std::string doc{ R"((base64-encode-bytes STRING)

Return the bytes of encoding the string `STRING` in base64.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));

        auto list = eval->eval(obj->i(0));
        AL_CHECK(assert_byte_array(list));

        std::vector<char> v;
        v.reserve(std::size(*list));

        for (auto &b : *list)
        {
            v.emplace_back(static_cast<char>(b->to_int()));
        }

        return make_string(detail::Base64::Encode(std::string(v.begin(), v.end())));
    }
};

struct base64_decode_string
{
    inline static const std::string name{ "base64-decode-string" };

    inline static const Signature signature{ String{} };

    inline static const std::string doc{ R"((base64-decode-string STRING)

Decode the base64 encoded string `STRING` and return the result as string.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));

        auto str = eval->eval(obj->i(0));
        AL_CHECK(assert_string(str));
        std::string out;
        auto res = detail::Base64::Decode(str->to_string(), out);
        if (res)
        {
            return make_string(out);
        }
        return Qnil;
    }
};

struct base64_decode_bytes
{
    inline static const std::string name{ "base64-decode-bytes" };

    inline static const Signature signature{ String{} };

    inline static const std::string doc{ R"((base64-decode-bytes BYTES_LIST)

Decode the base64 encoded bytes `BYTES_LIST` and return the result as string.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));

        auto str = eval->eval(obj->i(0));
        AL_CHECK(assert_string(str));
        std::string out;
        auto res = detail::Base64::Decode(str->to_string(), out);
        if (res)
        {

            ALObject::list_type bytes;
            for (const char c : out)
            {
                bytes.push_back(make_int(static_cast<ALObject::int_type>(c)));
            }
            return make_list(bytes);
        }
        return Qnil;
    }
};

struct base16_encode_string
{
    inline static const std::string name{ "base16-encode-string" };

    inline static const Signature signature{ String{} };

    inline static const std::string doc{ R"((base16-encode-string STRING)

Return base16 encoded version of the string `STRING`.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));

        auto str = eval->eval(obj->i(0));
        AL_CHECK(assert_string(str));

        return make_string(detail::Base16::Encode(str->to_string()));
    }
};

struct base16_encode_bytes
{
    inline static const std::string name{ "base16-encode-bytes" };

    inline static const Signature signature{ ByteArray{} };

    inline static const std::string doc{ R"((base16-encode-string STRING)

Return the bytes of encoding the string `STRING` in base16.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));

        auto list = eval->eval(obj->i(0));
        AL_CHECK(assert_byte_array(list));

        std::vector<char> v;
        v.reserve(std::size(*list));

        for (auto &b : *list)
        {
            v.emplace_back(static_cast<char>(b->to_int()));
        }

        return make_string(detail::Base16::Encode(std::string(v.begin(), v.end())));
    }
};

struct base16_decode_string
{
    inline static const std::string name{ "base16-decode-string" };

    inline static const Signature signature{ String{} };

    inline static const std::string doc{ R"((base16-decode-string STRING)

Decode the base16 encoded string `STRING` and return the result as string.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));

        auto str = eval->eval(obj->i(0));
        AL_CHECK(assert_string(str));
        std::string out;
        auto res = detail::Base16::Decode(str->to_string(), out);
        if (res)
        {
            return make_string(out);
        }
        return Qnil;
    }
};

struct base16_decode_bytes
{
    inline static const std::string name{ "base16-decode-bytes" };

    inline static const Signature signature{ String{} };

    inline static const std::string doc{ R"((base16-decode-bytes BYTES_LIST)

Decode the base16 encoded bytes `BYTES_LIST` and return the result as string.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));

        auto str = eval->eval(obj->i(0));
        AL_CHECK(assert_string(str));
        std::string out;
        auto res = detail::Base16::Decode(str->to_string(), out);
        if (res)
        {

            ALObject::list_type bytes;
            for (const char c : out)
            {
                bytes.push_back(make_int(static_cast<ALObject::int_type>(c)));
            }
            return make_list(bytes);
        }
        return Qnil;
    }
};

struct base32_encode_string
{
    inline static const std::string name{ "base32-encode-string" };

    inline static const Signature signature{ String{} };

    inline static const std::string doc{ R"((base32-encode-string STRING)

Return base32 encoded version of the string `STRING`.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));

        auto str = eval->eval(obj->i(0));
        AL_CHECK(assert_string(str));

        return make_string(detail::Base32::Encode(str->to_string()));
    }
};

struct base32_decode_string
{
    inline static const std::string name{ "base32-decode-string" };

    inline static const Signature signature{ String{} };

    inline static const std::string doc{ R"((base32-decode-string STRING)

Decode the base32 encoded string `STRING` and return the result as string.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));

        auto str = eval->eval(obj->i(0));
        AL_CHECK(assert_string(str));
        std::string out;
        auto res = detail::Base32::Decode(str->to_string(), out);
        if (res)
        {
            return make_string(out);
        }
        return Qnil;
    }
};

struct base32_decode_bytes
{
    inline static const std::string name{ "base32-decode-bytes" };

    inline static const Signature signature{ String{} };

    inline static const std::string doc{ R"((base32-decode-bytes BYTES_LIST)

Decode the base32 encoded bytes `BYTES_LIST` and return the result as string.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));

        auto str = eval->eval(obj->i(0));
        AL_CHECK(assert_string(str));
        std::string out;
        auto res = detail::Base32::Decode(str->to_string(), out);
        if (res)
        {

            ALObject::list_type bytes;
            for (const char c : out)
            {
                bytes.push_back(make_int(static_cast<ALObject::int_type>(c)));
            }
            return make_list(bytes);
        }
        return Qnil;
    }
};

struct base32_encode_bytes
{
    inline static const std::string name{ "base32-encode-bytes" };

    inline static const Signature signature{ ByteArray{} };

    inline static const std::string doc{ R"((base32-encode-bytes STRING)

Return the bytes of encoding the string `STRING` in base32.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        assert_size<1>(obj);

        auto list = eval->eval(obj->i(0));
        assert_byte_array(list);

        std::vector<char> v;
        v.reserve(std::size(*list));

        for (auto &b : *list)
        {
            v.emplace_back(static_cast<char>(b->to_int()));
        }

        return make_string(detail::Base32::Encode(std::string(v.begin(), v.end())));
    }
};


}  // namespace base64

ALISP_EXPORT alisp::env::ModulePtr init_base64(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    using namespace alisp;

    auto Mbase64    = alisp::module_init("base64");
    auto base64_ptr = Mbase64.get();

    module_doc(
      base64_ptr,
      R"(The `base64` module provides several codecs for encoding byte-data -- base64, base32 and base16. There are functions that operate on strings as well as ones that take raw byte lists. The decoding functions return either a string representation of the original input or byte list that contains the pure bytes of the decoded information.

Example:
```elisp

(println (base64-decode-string (base64-encode-string "this is a string")))
(println (base16-decode-string (base16-encode-string "this is a string")))

(mapc println (base64-decode-bytes (base64-encode-bytes '(97 98 99))))
(mapc println (base16-decode-bytes (base16-encode-bytes '(97 98 99))))
```
)");


    module_defun<base64::base16_decode_bytes>(base64_ptr);
    module_defun<base64::base16_encode_bytes>(base64_ptr);
    module_defun<base64::base16_decode_string>(base64_ptr);
    module_defun<base64::base16_encode_string>(base64_ptr);

    module_defun<base64::base32_decode_bytes>(base64_ptr);
    module_defun<base64::base32_encode_bytes>(base64_ptr);
    module_defun<base64::base32_decode_string>(base64_ptr);
    module_defun<base64::base32_encode_string>(base64_ptr);

    module_defun<base64::base64_decode_bytes>(base64_ptr);
    module_defun<base64::base64_encode_bytes>(base64_ptr);
    module_defun<base64::base64_decode_string>(base64_ptr);
    module_defun<base64::base64_encode_string>(base64_ptr);

    return Mbase64;
}
