.PHONY: help install nightly index start-server deploy

help:
	@echo "Type 'make install' to install Herbie"
	@echo "Then type 'racket src/herbie.rkt web' to run it."

install:
	cargo build --release --manifest-path=egg-herbie/Cargo.toml
	-raco pkg remove --auto herbie && echo "Warning: uninstalling herbie and reinstalling local version"
	-raco pkg remove --auto egg-herbie && echo "Warning: uninstalling egg-herbie and reinstalling local version"
	-raco pkg remove --auto egg-herbie-linux && echo "Warning: uninstalling egg-herbie and reinstalling local version"
	-raco pkg remove --auto egg-herbie-windows && echo "Warning: uninstalling egg-herbie and reinstalling local version"
	-raco pkg remove --auto egg-herbie-osx && echo "Warning: uninstalling egg-herbie and reinstalling local version"
	raco pkg install ./egg-herbie
	raco pkg install --skip-installed --name herbie src/
	raco pkg update --name herbie src/

nightly: install
	bash infra/nightly.sh
	$(MAKE) index

index:
	bash infra/publish.sh index

start-server: install
	racket src/herbie.rkt web --seed 1 --timeout 150 --num-iters 2 \
		--demo --public --prefix /demo/ --port 4053 --save-session www/demo/ \
		--log infra/server.log --quiet 2>&1

# This rule is run by herbie.uwplse.org on every commit to Github.
# It does not restart the demo server, but it does pull new static content
deploy:
	git -C $(shell ~/uwplse/getdir) pull

herbie.zip herbie.zip.CHECKSUM:
	raco pkg create src/
	mv src.zip herbie.zip
	mv src.zip.CHECKSUM herbie.zip.CHECKSUM
