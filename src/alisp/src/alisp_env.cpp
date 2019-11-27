#include <algorithm>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"

#include "alisp/utility/macros.hpp"


template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;


namespace alisp
{


template<size_t N, class ... Callable>
void visit_(ALObjectType type, ALObject* obj, std::tuple<std::pair<ALObjectType, Callable>...> & visitors){

    if constexpr (N >= sizeof...(Callable)) {
        return;
    }
    else{
        
        if (std::get<N>(visitors).first == type) {
            std::get<N>(visitors).second(obj);
        } else{
            visit_<N+1>(obj->type(), obj, visitors);
        }
    }
}

	
template <class ... Callable>
void visit(ALObject* obj, std::tuple<std::pair<ALObjectType, Callable>...> visitors){
    visit_<0>(obj->type(), obj, visitors);
}

	template<typename Callable>
	struct match {
	public:
    Callable m_fun;
	public:
		match(Callable && t_fun) : m_fun(std::forward<Callable>(t_fun)){}
	};

	
	template<typename Check, typename Call>
	struct pattern_entry {
	private:
		Check m_check;
		Call m_call;
	public:
		pattern_entry(Check check, Call call) :
			m_check(check),  m_call(call){}
		bool check(ALObject* obj){ return m_check(obj);}
		void call(ALObject* obj){ m_call(obj);}
	};

	auto type(ALObjectType t_type){
    return match([t_t=std::move(t_type)](ALObject* obj)->bool {
									 return  obj->type() == t_t;
								 });
	}


	template<typename Check, typename Call>
	pattern_entry<Check, Call> operator>(match<Check> t_match, Call && callable)
	{
		return pattern_entry(t_match.m_fun, callable);
	}



	template<size_t N, class ... Matches, class ... Checks>
	void visit_match_(ALObject* obj, std::tuple<pattern_entry<Checks, Matches>...> patterns){
     
    if constexpr (N >= sizeof...(Checks)) {
        return;
			} else {
			if (std::get<N>(patterns).check(obj)) {
				std::get<N>(patterns).call(obj);
			} else{
				visit_match_<N+1>(obj, patterns);
			}
    }
	}

	template <class ... Matches, class ... Checks >
	void visit_match(ALObject* obj, std::tuple<pattern_entry<Checks, Matches>...> patterns ){
		visit_match_<0>(obj, patterns);
	}

	template <class ... Matches, class ... Checks >
	void make_visit(ALObject* obj, pattern_entry<Matches, Checks> ... entries){
    visit_match(obj, std::tuple(entries ...));
	}


	
	ALObject* eval_list (eval::Evaluator* evl, ALObject* t_obj, size_t t_offset = 0) {

		auto objects = t_obj->children();
		const auto hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(t_offset);


		auto start_it = std::next(std::begin(objects), hops);
		auto end_it = std::prev(std::end(objects));

		while (start_it != end_it) {
			evl->eval(*start_it);
			start_it = std::next(start_it);
		}

		return evl->eval(*end_it);
	}

	template<size_t N>
        ALObject* eval_list_n (eval::Evaluator* evl, ALObject* t_obj,size_t t_offset = 0) {

        auto objects = t_obj->children();
        const auto hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(t_offset);
        const auto return_hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(N);

        auto start_it = std::next(std::begin(objects), hops);
        auto return_it = std::next(std::begin(objects), return_hops - 1);
        auto end_it = std::end(objects);

        while (start_it != return_it) {
            evl->eval(*start_it);
            start_it = std::next(start_it);
        }
        auto res = evl->eval(*start_it);
        start_it = std::next(start_it);

        while (start_it != end_it) {
            evl->eval(*start_it);
            start_it = std::next(start_it);
        }

        return res;
    }

    ALObject* eval_list_1 (eval::Evaluator* evl, ALObject* t_obj, size_t t_offset = 0) {
        return eval_list_n<0>(evl, t_obj, t_offset);
    }

    ALObject* eval_list_2 (eval::Evaluator* evl, ALObject* t_obj, size_t t_offset = 0) {
        return eval_list_n<1>(evl, t_obj, t_offset);
    }


    template<bool eval, typename Callable>
        auto apply (eval::Evaluator* evl, ALObject* t_obj, Callable t_fun, size_t t_offset = 0){

        auto objects = t_obj->children();
        const auto hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(t_offset);

        auto start_it = std::next(std::begin(objects), hops);
        auto end_it = std::prev(std::end(objects));

        while (start_it != end_it) {
            if constexpr (eval){
                t_fun(evl->eval(*start_it));
            } else {
                t_fun(*start_it);
            }
            ++start_it;
        }

        if constexpr (eval){
            return t_fun(evl->eval(*end_it));
        } else {
            return t_fun(*end_it);
        }

    }

    template<bool eval, typename Callable, typename StartType>
        StartType reduce (eval::Evaluator* evl, ALObject* t_obj, Callable && t_fun, StartType t_start, size_t t_offset = 0)
    {
        auto objects = t_obj->children();
        const auto hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(t_offset);

        auto start_it = std::next(std::begin(objects), hops);
        auto end_it = std::end(objects);

        StartType val =
            [&](){
                if constexpr (eval){
                    return t_fun(t_start, evl->eval(*start_it++));
                } else {
                    return t_fun(t_start, *start_it++);
                }
            }();


        while (start_it != end_it) {

            if constexpr (eval){
                val = t_fun(val, evl->eval(*start_it));
            } else {
                val = t_fun(val, *start_it);
            }
            ++start_it;
        }

        return val;
    }




    ALObject* Fdefvar(ALObject* obj, env::Environment* env, eval::Evaluator*)
    {
        auto new_var = new ALCell(obj->i(0)->to_string());
        new_var->make_value(obj->i(1));
        env->put(obj->i(0), new_var);
        return Qt;
    }

    ALObject* Fquote(ALObject* obj, env::Environment*, eval::Evaluator*)
    {
        return obj->i(0);
    }


    ALObject* Fdefun(ALObject* obj, env::Environment* env, eval::Evaluator*)
    {
        auto new_fun = new ALCell(obj->i(0)->to_string());
        new_fun->make_function(obj->i(1), splice(obj, 2));
        env->put(obj->i(0), new_fun);
        return Qt;
    }


    ALObject* Fsetq(ALObject* obj, env::Environment* env, eval::Evaluator* evl)
    {
        auto new_var = new ALCell(obj->i(0)->to_string());
        new_var->make_value(evl->eval(obj->i(1)));
        env->put(obj->i(0), new_var);
        return Qt;
    }


	ALObject* Fprint(ALObject* tt_obj, env::Environment*, eval::Evaluator* eval)
	{
    const auto fun =
			[](ALObject* t_obj){

				if(t_obj->type() == ALObjectType::STRING_VALUE || t_obj->type() == ALObjectType::SYMBOL) {
					std::cout << t_obj->to_string() << "\n";
					return Qt;
				} else if(t_obj->type() == ALObjectType::INT_VALUE) {
					std::cout << t_obj->to_int() << "\n";
					return Qt;
				} else if(t_obj->type() == ALObjectType::REAL_VALUE) {
					std::cout << t_obj->to_real() << "\n";
					return Qt;
				}


				return Qnil;
			};


    auto val = eval->eval(tt_obj->i(0));

    ::alisp::visit(val,
                   std::tuple(
										 std::pair{ALObjectType::INT_VALUE, [](ALObject* obj) { std::cout << dump(obj) << '\n'; }},
										 std::pair{ALObjectType::REAL_VALUE, [](ALObject* obj) { std::cout << dump(obj) << '\n'; }},
										 std::pair{ALObjectType::STRING_VALUE, [](ALObject* obj) { std::cout << dump(obj) << '\n'; }},
										 std::pair{ALObjectType::SYMBOL, [](ALObject* obj) { std::cout << dump(obj) << '\n'; }},
										 std::pair{ALObjectType::LIST, [](ALObject* obj) { std::cout << dump(obj) << '\n'; }}
										 ));

    // std::visit(overloaded {
		//         [](ALObject::string_type& arg) { std::cout << arg << '\n'; },
		//         [](ALObject::int_type&    arg) { std::cout << arg << '\n'; },
		//         [](ALObject::real_type&   arg) { std::cout << arg << '\n'; },
		//         [](ALObject::list_type&      ) { std::cout << "can't print list object" << ' '; }
		//     }, val->data());

		return fun(val);
				
		vis(ojb, {
				type(ALObjectType::INT_VALUE) > [](ALObject* obj) { std::cout << dump(obj) << '\n'; },
				type(ALObjectType::REAL_VALUE) > [](ALObject* obj) { std::cout << dump(obj) << '\n'; }
					
			});
		
				
	}


	ALObject* Fif(ALObject* obj, env::Environment*, eval::Evaluator* evl)
	{
		// TODO: sanity checks

		if (eval::Evaluator::is_truthy(evl->eval(obj->i(0)))) {
			return evl->eval(obj->i(1));
		} else if (obj->length() == 3) {
			return evl->eval(obj->i(2));
		} else {
			return Qnil;
		}
	}

	ALObject* Fwhile(ALObject* obj, env::Environment*, eval::Evaluator* evl)
	{
		while (eval::Evaluator::is_truthy(evl->eval(obj->i(0)))) {
			eval_list(evl, obj, 1);
		}
		return Qt;
	}

	static const auto add_obj_fun = [](int64_t t_acc, ALObject* t_obj) {return t_acc + t_obj->to_int();};
	static const auto sub_obj_fun = [](int64_t t_acc, ALObject* t_obj) {return t_acc - t_obj->to_int();};
	static const auto mul_obj_fun = [](int64_t t_acc, ALObject* t_obj) {return t_acc * t_obj->to_int();};
	static const auto div_obj_fun = [](int64_t t_acc, ALObject* t_obj) {return t_acc / t_obj->to_int();};


	ALObject* Fmultiply(ALObject* obj, env::Environment*, eval::Evaluator* evl)
	{
		const int64_t product = reduce<true>(evl, obj, mul_obj_fun, static_cast<int64_t>(1));
		return make_int(product);
	}


	ALObject* Fplus(ALObject* obj, env::Environment*, eval::Evaluator* evl)
	{
		const int64_t sum = reduce<true>(evl, obj, add_obj_fun, static_cast<int64_t>(0));
		return make_int(sum);
	}


	ALObject* Fminus(ALObject* obj, env::Environment*, eval::Evaluator* evl)
	{
		const int64_t sub = reduce<true>(evl, obj, sub_obj_fun, obj->i(0)->to_int(), 1);
		return make_int(sub);
	}

	ALObject* Fdev(ALObject* obj, env::Environment*, eval::Evaluator* evl)
	{
		const int64_t div = reduce<true>(evl, obj, div_obj_fun, obj->i(0)->to_int(), 1);
		return make_int(div);
	}


	ALObject* Flt(ALObject* obj, env::Environment*, eval::Evaluator* evl)
	{
		const auto one = evl->eval(obj->i(0))->to_int();
		const auto two = evl->eval(obj->i(1))->to_int();
		if (one < two) return Qt;
		else return Qnil;
	}


	ALObject* Fleq(ALObject* obj, env::Environment*, eval::Evaluator* evl)
	{
		const auto one = evl->eval(obj->i(0))->to_int();
		const auto two = evl->eval(obj->i(1))->to_int();
		if (one <= two) return Qt;
		else return Qnil;
	}


	ALObject* Fgt(ALObject* obj, env::Environment*, eval::Evaluator* evl)
	{
		const auto one = evl->eval(obj->i(0))->to_int();
		const auto two = evl->eval(obj->i(1))->to_int();
		if (one > two) return Qt;
		else return Qnil;
	}

	ALObject* Fgeq(ALObject* obj, env::Environment*, eval::Evaluator* evl)
	{
		const auto one = evl->eval(obj->i(0))->to_int();
		const auto two = evl->eval(obj->i(1))->to_int();
		if (one >= two) return Qt;
		else return Qnil;
	}

	ALObject* Feq(ALObject* obj, env::Environment*, eval::Evaluator* evl)
	{
		const auto one = evl->eval(obj->i(0))->to_int();
		const auto two = evl->eval(obj->i(1))->to_int();
		if (one == two) return Qt;
		else return Qnil;
	}

	ALObject* Fneq(ALObject* obj, env::Environment*, eval::Evaluator* evl)
	{
		const auto one = evl->eval(obj->i(0))->to_int();
		const auto two = evl->eval(obj->i(1))->to_int();
		if (one != two) return Qt;
		else return Qnil;
	}


	ALObject* Fprogn(ALObject* obj, env::Environment*, eval::Evaluator* evl)
	{
		return eval_list(evl, obj, 1);
	}


	ALObject* Flet(ALObject* obj, env::Environment* env, eval::Evaluator* evl)
	{
		env::detail::ScopePushPop spp{*env};

		auto varlist = obj->i(0);

		std::vector<std::pair<ALObject*,ALCell*>> cells;
		cells.reserve(std::size(varlist->children()));

		for (auto var : varlist->children()) {
			auto new_var = new ALCell(var->i(0)->to_string());
			new_var->make_value(evl->eval(var->i(1)));
			cells.push_back({var->i(0), new_var});
		}

		for (auto[ob, cell] : cells) {
			env->put(ob, cell);
		}

		return eval_list(evl, obj, 1);
	}


	ALObject* Fletx(ALObject* obj, env::Environment* env, eval::Evaluator* evl)
	{
		env::detail::ScopePushPop spp{*env};

		auto varlist = obj->i(0);
		for (auto var : varlist->children()) {
			auto new_var = new ALCell(var->i(0)->to_string());
			new_var->make_value(evl->eval(var->i(1)));
			env->put(var->i(0), new_var);
		}

		return eval_list(evl, obj, 1);
	}

}
