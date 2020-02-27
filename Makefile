BUILD_DEBUG_DIR?=build


generate_builds:
	bash generate_builds.sh ./configs_builds.txt

performance_builds:
	bash generate_builds.sh ./timing_builds.txt
	python ./plot_performance.py ./



plain_build:
	mkdir -p $(BUILD_DEBUG_DIR)
	cd $(BUILD_DEBUG_DIR) && conan install ..
	cd $(BUILD_DEBUG_DIR) && cmake .. -DCMAKE_BUILD_TYPE=Debug
	cd $(BUILD_DEBUG_DIR) && make -j4

build:
	@make -j8 -C $(BUILD_DEBUG_DIR)


count_line:
	@cloc CMakeLists.txt ./src ./scripts/ --force-lang=lisp,al | sed -e 's/Lisp /ALisp/'

format_project:
	@echo "Fromating the source directory..."
	@find ./src/ -iname *.hpp -o -iname *.cpp | xargs clang-format -i -style=file


doc_debug:
	@mkdocs serve

doc_gen:
	@mkdocs build

clean:
	rm -rf build_*	
	@rm -rf site

