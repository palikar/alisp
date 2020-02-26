#include "alisp/config.hpp"
#include "alisp/alisp/alisp_module_helpers.hpp"


namespace base64
{
using namespace alisp;

namespace detail
{


struct Base64 {
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wuseless-cast"
#endif

    static std::string Encode(const std::string data)
    {
        static constexpr char sEncodingTable[] = {
            'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
            'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
            'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
            'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
            'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
            'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
            'w', 'x', 'y', 'z', '0', '1', '2', '3',
            '4', '5', '6', '7', '8', '9', '+', '/'
        };

        size_t in_len = data.size();
        size_t out_len = 4 * ((in_len + 2) / 3);
        std::string ret(out_len, '\0');
        size_t i;
        char *p = const_cast<char*>(ret.c_str());

        for (i = 0; i < in_len - 2; i += 3) {
            *p++ = sEncodingTable[(data[i] >> 2) & 0x3F];
            *p++ = sEncodingTable[((data[i] & 0x3) << 4) | (static_cast<int>((data[i + 1] & 0xF0) >> 4))];
            *p++ = sEncodingTable[((data[i + 1] & 0xF) << 2) | (static_cast<int>((data[i + 2] & 0xC0) >> 6))];
            *p++ = sEncodingTable[data[i + 2] & 0x3F];
        }
        if (i < in_len) {
            *p++ = sEncodingTable[(data[i] >> 2) & 0x3F];
            if (i == (in_len - 1)) {
                *p++ = sEncodingTable[((data[i] & 0x3) << 4)];
                *p++ = '=';
            }
            else {
                *p++ = sEncodingTable[((data[i] & 0x3) << 4) | (static_cast<int>((data[i + 1] & 0xF0) >> 4))];
                *p++ = sEncodingTable[((data[i + 1] & 0xF) << 2)];
            }
            *p++ = '=';
        }

        return ret;
    }

    static bool Decode(const std::string& input, std::string& out)
    {
        static constexpr unsigned char kDecodingTable[] = {
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 62, 64, 64, 64, 63,
            52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 64, 64, 64, 64, 64, 64,
            64,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
            15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 64, 64, 64, 64, 64,
            64, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
            41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 64, 64, 64, 64, 64,
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
            64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64
        };

        size_t in_len = input.size();
        if (in_len % 4 != 0) { return false; }

        size_t out_len = in_len / 4 * 3;
        if (input[in_len - 1] == '=') out_len--;
        if (input[in_len - 2] == '=') out_len--;

        out.resize(out_len);

        for (size_t i = 0, j = 0; i < in_len;) {
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

struct Base16 {

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wuseless-cast"
#endif

    static std::string Encode(const std::string& input)
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
        switch (hex_digit) {
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
              return hex_digit - '0';

          case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
              return hex_digit - 'A' + 10;

          case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
              return hex_digit - 'a' + 10;
        }
        return -1;
    }

    static bool Decode(const std::string& input, std::string& output)
    {
        size_t len = input.length();
        if (len & 1) { return false; }

        output.reserve(len / 2);
        for (auto it = input.begin(); it != input.end(); )
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


}

ALObjectPtr Fbase64_encode_string(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);

    auto str = eval->eval(obj->i(0));
    assert_string(str);

    return make_string(detail::Base64::Encode(str->to_string()));
}

ALObjectPtr Fbase64_encode_bytes(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);

    auto list = eval->eval(obj->i(0));
    assert_byte_array(list);

    std::vector<char> v;
    v.reserve(std::size(*list));

    for (auto& b : *list) { v.emplace_back(static_cast<char>(b->to_int())); }
    
    return make_string(detail::Base64::Encode(std::string(v.begin(), v.end())));
}

ALObjectPtr Fbase64_decode_string(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);

    auto str = eval->eval(obj->i(0));
    assert_string(str);
    std::string out;
    auto res = detail::Base64::Decode(str->to_string(), out);
    if (res) {
        return make_string(out);
    }
    return Qnil;
}

ALObjectPtr Fbase64_decode_bytes(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);

    auto str = eval->eval(obj->i(0));
    assert_string(str);
    std::string out;
    auto res = detail::Base64::Decode(str->to_string(), out);
    if (res) {

        ALObject::list_type bytes;
        for (const char c : out) { bytes.push_back(make_int(static_cast<ALObject::int_type>(c))); }
        return make_list(bytes);
    }
    return Qnil;
}



ALObjectPtr Fbase16_encode_string(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);

    auto str = eval->eval(obj->i(0));
    assert_string(str);

    return make_string(detail::Base16::Encode(str->to_string()));
}

ALObjectPtr Fbase16_encode_bytes(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);

    auto list = eval->eval(obj->i(0));
    assert_byte_array(list);

    std::vector<char> v;
    v.reserve(std::size(*list));

    for (auto& b : *list) { v.emplace_back(static_cast<char>(b->to_int())); }
    
    return make_string(detail::Base16::Encode(std::string(v.begin(), v.end())));
}

ALObjectPtr Fbase16_decode_string(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);

    auto str = eval->eval(obj->i(0));
    assert_string(str);
    std::string out;
    auto res = detail::Base16::Decode(str->to_string(), out);
    if (res) {
        return make_string(out);
    }
    return Qnil;
}

ALObjectPtr Fbase16_decode_bytes(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);

    auto str = eval->eval(obj->i(0));
    assert_string(str);
    std::string out;
    auto res = detail::Base16::Decode(str->to_string(), out);
    if (res) {

        ALObject::list_type bytes;
        for (const char c : out) { bytes.push_back(make_int(static_cast<ALObject::int_type>(c))); }
        return make_list(bytes);
    }
    return Qnil;
}

}

ALISP_EXPORT alisp::env::ModulePtr init_base64(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mbase64 = alisp::module_init("base64");
    auto base64_ptr = Mbase64.get();

    alisp::module_doc(base64_ptr, R"(The `base64` module provides several codecs for encoding byte-data.)");

    alisp::module_defun(base64_ptr, "base64-encode-string", &base64::Fbase64_encode_string);
    alisp::module_defun(base64_ptr, "base64-encode-bytes", &base64::Fbase64_encode_bytes);
    alisp::module_defun(base64_ptr, "base64-decode-string", &base64::Fbase64_decode_string);
    alisp::module_defun(base64_ptr, "base64-decode-bytes", &base64::Fbase64_decode_bytes);


    alisp::module_defun(base64_ptr, "base16-encode-string", &base64::Fbase16_encode_string);
    alisp::module_defun(base64_ptr, "base16-encode-bytes", &base64::Fbase16_encode_bytes);
    alisp::module_defun(base64_ptr, "base16-decode-string", &base64::Fbase16_decode_string);
    alisp::module_defun(base64_ptr, "base16-decode-bytes", &base64::Fbase16_decode_bytes);

    return Mbase64;
}
