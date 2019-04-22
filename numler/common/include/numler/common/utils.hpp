
namespace nu{

	namespace utils{

		
		template<class ForwardIt, class T>
		void iota(ForwardIt first, ForwardIt last, T value)
		{
			while(first != last) {
				*first++ = value();
				++value;
			}
		}

		template<class T>
		class Inc
		{
		private:
			T _init;
			T _val;
		public:
			Inc(const T initial, const T val) : _init(initial), _val(val){};
			void operator++(){_init += _val;}
			T operator()(){return _init;}
		};

		
	}
	
}
