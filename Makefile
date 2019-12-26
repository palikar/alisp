BUILD_DEBUG_DIR?=build


generate_builds:
	bash generate_builds.sh

clean:
	rm -rf build_*


plain_build:
	mkdir -p $(BUILD_DEBUG_DIR)
	cd $(BUILD_DEBUG_DIR) && conan install ..
	cd $(BUILD_DEBUG_DIR) && cmake .. -DCMAKE_BUILD_TYPE=Debug
	cd $(BUILD_DEBUG_DIR) && make -j4
