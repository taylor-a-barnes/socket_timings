cmake_minimum_required(VERSION 3.5)

project(MDI
        LANGUAGES C CXX Fortran)

find_package(MPI)

if(MPI_FOUND)
   include_directories(${MPI_INCLUDE_PATH})
else()
   include_directories(${CMAKE_SOURCE_DIR}/STUBS_MPI/)
endif()

set( MDI_LOCATION ${CMAKE_SOURCE_DIR}/../lib/mdi_build/MDI_Library/ )
link_directories( ${MDI_LOCATION} )




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
target_link_libraries(driver mdi
                             ${MPI_LIBRARIES})
set_target_properties(driver
                      PROPERTIES LINKER_LANGUAGE CXX)

if(MPI_COMPILE_FLAGS)
  set_target_properties(driver PROPERTIES
    COMPILE_FLAGS "${MPI_COMPILE_FLAGS}")
endif()

if(MPI_LINK_FLAGS)
  set_target_properties(driver PROPERTIES
    LINK_FLAGS "${MPI_LINK_FLAGS}")
endif()




add_executable(production
           production.c)
target_link_libraries(production mdi
                                 ${MPI_LIBRARIES})
set_target_properties(production
                      PROPERTIES LINKER_LANGUAGE CXX)

if(MPI_COMPILE_FLAGS)
  set_target_properties(production PROPERTIES
    COMPILE_FLAGS "${MPI_COMPILE_FLAGS}")
endif()

if(MPI_LINK_FLAGS)
  set_target_properties(production PROPERTIES
    LINK_FLAGS "${MPI_LINK_FLAGS}")
endif()




add_executable(mpi_driver
           mpi_driver.c)
target_link_libraries(mpi_driver mdi
                                 ${MPI_LIBRARIES})
set_target_properties(mpi_driver
                      PROPERTIES LINKER_LANGUAGE CXX)

if(MPI_COMPILE_FLAGS)
  set_target_properties(mpi_driver PROPERTIES
    COMPILE_FLAGS "${MPI_COMPILE_FLAGS}")
endif()

if(MPI_LINK_FLAGS)
  set_target_properties(mpi_driver PROPERTIES
    LINK_FLAGS "${MPI_LINK_FLAGS}")
endif()




add_executable(mpi_production
           mpi_production.c)
target_link_libraries(mpi_production mdi
                                     ${MPI_LIBRARIES})
set_target_properties(mpi_production
                      PROPERTIES LINKER_LANGUAGE CXX)

if(MPI_COMPILE_FLAGS)
  set_target_properties(mpi_production PROPERTIES
    COMPILE_FLAGS "${MPI_COMPILE_FLAGS}")
endif()

if(MPI_LINK_FLAGS)
  set_target_properties(mpi_production PROPERTIES
    LINK_FLAGS "${MPI_LINK_FLAGS}")
endif()


add_executable(mpi_driver_f90
           mpi_driver_f90.f90)
target_link_libraries(mpi_driver_f90 mdi
                                 ${MPI_LIBRARIES})
target_include_directories(mpi_driver_f90 PUBLIC
   ${CMAKE_SOURCE_DIR}/../lib/mdi_build/MDI_Library/
   ${MPI_INCLUDE_PATH} )

if(MPI_COMPILE_FLAGS)
  set_target_properties(mpi_driver_f90 PROPERTIES
    COMPILE_FLAGS "${MPI_COMPILE_FLAGS}")
endif()

if(MPI_LINK_FLAGS)
  set_target_properties(mpi_driver_f90 PROPERTIES
    LINK_FLAGS "${MPI_LINK_FLAGS}")
endif()


add_executable(mpi_production_f90
           mpi_production_f90.f90)
target_link_libraries(mpi_production_f90 mdi
                                 ${MPI_LIBRARIES})
target_include_directories(mpi_production_f90 PUBLIC
   ${CMAKE_SOURCE_DIR}/../lib/mdi_build/MDI_Library/
   ${MPI_INCLUDE_PATH} )

if(MPI_COMPILE_FLAGS)
  set_target_properties(mpi_production_f90 PROPERTIES
    COMPILE_FLAGS "${MPI_COMPILE_FLAGS}")
endif()

if(MPI_LINK_FLAGS)
  set_target_properties(mpi_production_f90 PROPERTIES
    LINK_FLAGS "${MPI_LINK_FLAGS}")
endif()





add_executable(zmq_server
           zmq_server.c)
target_link_libraries(zmq_server ${CMAKE_SOURCE_DIR}/../lib/zmq_build/lib/libzmq.so.5)




add_executable(zmq_client
           zmq_client.c)
target_link_libraries(zmq_client ${CMAKE_SOURCE_DIR}/../lib/zmq_build/lib/libzmq.so.5)


# copy MDI python files
#configure_file(${MDI_LOCATION}/mdi.py ${CMAKE_CURRENT_BINARY_DIR}/mdi/mdi.py COPYONLY)
#configure_file(${MDI_LOCATION}/__init__.py ${CMAKE_CURRENT_BINARY_DIR}/mdi/__init__.py COPYONLY)
file(COPY ${MDI_LOCATION} DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/mdi)

# copy socket_timings python files
configure_file(${CMAKE_SOURCE_DIR}/driver.py ${CMAKE_CURRENT_BINARY_DIR}/driver.py COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/production.py ${CMAKE_CURRENT_BINARY_DIR}/production.py COPYONLY)
