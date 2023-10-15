.DEFAULT_GOAL := run

run:
	cd build && ninja && ./cuthil-mckee-example ../examples/star.1.4.json

ninja:
	cd build && ninja

init:
	git submodule update --init --recursive
	meson setup build
