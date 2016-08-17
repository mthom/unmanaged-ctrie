# cmake -G "Unix Makefiles" -DCMAKE_CXX_COMPILER=/usr/bin/clang++ -DCMAKE_CXX_FLAGS="-Wall -Wextra -DNDEBUG -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -Ofast -fomit-frame-pointer -std=c++1z -fvisibility-inlines-hidden -fPIC -Woverloaded-virtual -ffunction-sections -fdata-sections -Wcast-qual -Wunused-parameter " -DCMAKE_LD_FLAGS="-pthread -ltinfo -ldl"

cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
set(CMAKE_LEGACY_CYGWIN_WIN32 0)

project("unmanaged-ctrie" CXX)

set(CMAKE_CXX_COMPILER "/usr/bin/clang++")
set(CMAKE_CXX_FLAGS, "${CMAKE_CXX_FLAGS} -Wall -Wextra -DNDEBUG -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -g -O2 -fomit-frame-pointer -std=c++1z -fvisibility-inlines-hidden -fPIC -Woverloaded-virtual -ffunction-sections -fdata-sections -Wcast-qual -Wunused-parameter")
set(CMAKE_LD_FLAGS "-pthread -ldl")
set(GTEST_INCLUDE_DIR /usr/include)
set(GTEST_SRC_DIR /usr/src/gtest)
set(GTEST_LIB_DIR /usr/lib)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include
	            ${GTEST_INCLUDE_DIR}
		    ${GTEST_SRC_DIR}/include)

find_package(Threads)

link_directories(${GTEST_LIB_DIR})

set(CTRIE_SOURCE test-ctrie.cpp ${GTEST_SRC_DIR}/src/gtest-all.cc)

add_executable(test-ctrie ${CTRIE_SOURCE})

target_link_libraries(test-ctrie ${CMAKE_THREAD_LIBS_INIT})
