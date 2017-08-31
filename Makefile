HOSTNAME=`hostname`

.PHONY: MaTheseR_install MaTheseR_test MaTheseR_document MaTheseR_check

## start

hajime:
	make krakenator_mount_data
	make krakenator_mount_OUTPUT
	git pull

yame:
	git status
	git pull
	git push

## Rpackage
MaTheseR_install:
	R -e 'devtools::install(pkg = "./Rpackage")'

MaTheseR_test:
	R -e 'devtools::test(pkg = "./Rpackage")'

MaTheseR_document:
	R -e 'devtools::document(pkg = "./Rpackage")'

MaTheseR_check:
	R -e 'devtools::check(pkg = "./Rpackage")'

## krakenator
krakenator_con_jupyter:
	ssh -L 8786:localhost:8786 -t cayek@krakenator.imag.fr 

krakenator_log_shell:
	ssh -X -t cayek@krakenator "cd ~/Projects/Thesis/MaThese; log_screen ${S} bash"

krakenator_log_R:
	ssh -X -t cayek@krakenator "cd ~/Projects/Thesis/MaThese; log_screen ${S} R"

krakenator_push_hook:
	scp ./hooks/post-receive.sh cayek@krakenator:/home/cayek/Gits/2017/MaThese.git/hooks/post-receive

krakenator_deploy_all:
	cd ../lfmm; make krakenator_deploy
	cd ../ExpRiment/; make krakenator_deploy
	make krakenator_deploy

krakenator_deploy:
	git status
# git commit --allow-empty -am "deploy on krakenator"
	git push krakenator master

## Data/
krakenator_mount_data:
	if [ $(HOSTNAME) == "timc-bcm-15.imag.fr" ] ; then \
		sshfs cayek@krakenator.imag.fr:/home/cayek/Projects/Thesis/Data Data -o allow_other; \
	fi

krakenator_umount_data:
	if [ $(HOSTNAME) == "timc-bcm-15.imag.fr" ] ; then \
		sudo umount Data/; \
	fi

## OUTPUT
krakenator_mount_OUTPUT:
	if [ $(HOSTNAME) == "timc-bcm-15.imag.fr" ] ; then \
		sshfs cayek@krakenator.imag.fr:/home/cayek/Projects/Thesis/MaThese/OUTPUT OUTPUT -o allow_other; \
	fi

krakenator_umount_OUTPUT:
	echo "${green}=====Umount OUTPUT/=====${reset}"
	if [ $(HOSTNAME) == "timc-bcm-15.imag.fr" ] ; then \
		sudo umount OUTPUT; \
	fi

