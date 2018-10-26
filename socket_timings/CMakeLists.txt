cmake_minimum_required(VERSION 3.5)

project(MDI
        LANGUAGES C)

find_package(MPI REQUIRED)

include_directories(${MPI_INCLUDE_PATH})

add_executable(main
           main.c)
target_link_libraries(main ${MPI_LIBRARIES})

if(MPI_COMPILE_FLAGS)
  set_target_properties(main PROPERTIES
    COMPILE_FLAGS "${MPI_COMPILE_FLAGS}")
endif()

if(MPI_LINK_FLAGS)
  set_target_properties(main PROPERTIES
    LINK_FLAGS "${MPI_LINK_FLAGS}")
endif()




add_executable(driver
           driver.c)
#include_directories("${CMAKE_SOURCE_DIR}/../lib/mdi_build/molssi_driver_interface/")
#link_directories("${CMAKE_SOURCE_DIR}/../lib/mdi_build/molssi_driver_interface/")
target_link_libraries(driver ${CMAKE_SOURCE_DIR}/../lib/mdi_build/molssi_driver_interface/libmdi.a)

#find_library(LIB_LOCATION NAMES libmdi.a mdi)
#target_link_libraries(driver ${LIB_LOCATION})




add_executable(production
           production.c)
target_link_libraries(production ${CMAKE_SOURCE_DIR}/../lib/mdi_build/molssi_driver_interface/libmdi.a)





add_executable(zmq_server
           zmq_server.c)
target_link_libraries(zmq_server ${CMAKE_SOURCE_DIR}/../lib/zmq_build/lib/libzmq.so.5)




add_executable(zmq_client
           zmq_client.c)
target_link_libraries(zmq_client ${CMAKE_SOURCE_DIR}/../lib/zmq_build/lib/libzmq.so.5)
