#pragma once
#include <cstdlib>

namespace alisp{

	namespace util{

		
		template<class ForwardIt, class T>
		void iota(ForwardIt first, ForwardIt last, T value)
		{
			while(first != last) {
				*first++ = value();
				++value;
			}
		}


		template<class T>
		class Icrementer
		{
		private:
			T _init; 
			T _val;			
		public:
			
			Icrementer(const T initial, const T val) :
				_init(initial), _val(val){};
			void operator++(){_init += _val;}
			T operator()(){return _init;}			
		};

		
		template<class T>
		class IcrementerWithFact
		{
		private:
			T _init; 
			T _val;
			T _fact;
		public:
			
			IcrementerWithFact(const T initial, const T val, const T fact) :
				_init(initial), _val(val), _fact(fact){};
			void operator++(){_init += _val * _fact;}
			T operator()(){return _init;}			
		};
	}
	
}
