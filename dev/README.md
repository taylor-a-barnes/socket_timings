To add the MDI subtree:

git subtree add --prefix=lib/mdi https://github.com/MolSSI/molssi_driver_interface master --squash

To pull MDI updates:

git subtree pull --prefix=lib/mdi https://github.com/MolSSI/molssi_driver_interface master --squash

To add the ZMQ subtree:

git subtree add --prefix=lib/libzmq https://github.com/zeromq/libzmq master --squash
