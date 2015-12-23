.ONESHELL:

help:
	@
	echo -e "\e[33;1m"
	echo "* dev dependencies"
	echo "  * artanis"
	echo "    to host the web locally"
	echo "  * bundle & jekyll"
	echo "* makefile operations"
	echo "  * run"
	echo "    use jekyll serve to run it"
	echo "  * gis"
	echo "  * clean"
	echo "* I wish you happy making ^-^"
	echo "  please read the makefile for more informations"
	echo -e "\e[0m"

run:
	cat _config.yml local.yml > _config-local.yml

	 jekyll serve       \
	  --config  _config-local.yml  \
	  --watch                      \
	  --drafts

gis:
	dev-tool/run.scm

clean:
	@
	echo -e "\e[33;1m"
	echo "* clean"
	echo -e "\e[0m"
	rm -f *~ */*~ */*/*~ */*/*/*~ */*/*/*/*~  */*/*/*/*/*~
