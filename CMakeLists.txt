cmake_minimum_required(VERSION 3.16)
project(pallet_packing_tools LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_executable(candidate_heights main_candidate_heights.cpp)
add_executable(stable_pallet_support60 stable_pallet_support60.cpp)
add_executable(stable_pallet_support70 stable_pallet_support70.cpp)
add_executable(stable_pallet_ga70 stable_pallet_ga70.cpp)
add_executable(stable_multi_pallet_ga70 stable_multi_pallet_ga70.cpp)
add_executable(pallet_analyzer pallet_analyzer.cpp)
add_executable(unpack unpack.cpp)
