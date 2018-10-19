cmake_minimum_required(VERSION 2.8)
project(silenceremove)
add_executable(silenceremove main.c silence.c)
target_link_libraries(silenceremove -lm)