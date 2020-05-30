.PHONY: dummy clean build

dummy:
	@echo "Nice to see you!\nFor a local installation, please use 'stack install'"

clean-package:
	rm -Rf _build

build-package: clean-package
	stack build --test --copy-bins --local-bin-path _build/ \
	&& npm --prefix web-frontend/ run build \
	&& cp -R web-frontend/build/ _build/makeobj-web-frontend/

deb: build-package
	cp -f LICENSE debian/copyright \
	&& debuild -us -uc -b

cheaty-deb:
	cp -f LICENSE debian/copyright \
	&& debuild -us -uc -b
