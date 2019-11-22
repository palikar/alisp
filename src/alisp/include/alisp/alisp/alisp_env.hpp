#pragma once


#include <unordered_map>
#include <vector>

#include "alisp/alisp/alisp_common.hpp"


namespace alisp {






	namespace env{
		
		struct CellStack {
			using Scope = std::unordered_map<std::string, ALCell>;
			using StackFrame = std::vector<Scope>;
			using Stacks = std::vector<StackData>;

			StackFrame(){
				push_frame();
				push_scope();
			}

			void push_frame() { m_stacks.emplace_back(1); }
			void push_scope() { m_stacks.back().emplace_back(); }
			
			void pop_frame() { m_stacks.pop_back(); }
			void pop_scope() { m_stacks.back().pop_back(); }

			
			
			Stacks stacks;
			int call_depth = 0;
			
		};

			
		class Environment {
			public:

			Environment(){}


		private:
			CellStack m_stack

		};


	}




	


}
