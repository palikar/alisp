BUILD_DEBUG_DIR?=build


generate_builds:
	bash generate_builds.sh ./configs_builds.txt

performance_builds:
	bash generate_builds.sh ./timing_builds.txt
	python ./plot_performance.py ./

clean:
	rm -rf build_*


plain_build:
	mkdir -p $(BUILD_DEBUG_DIR)
	cd $(BUILD_DEBUG_DIR) && conan install ..
	cd $(BUILD_DEBUG_DIR) && cmake .. -DCMAKE_BUILD_TYPE=Debug
	cd $(BUILD_DEBUG_DIR) && make -j4


count_line:
	@cloc CMakeLists.txt ./src ./scripts/ --force-lang=lisp,al

