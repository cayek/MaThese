HOSTNAME=`hostname`

.PHONY: MaTheseR_install MaTheseR_test MaTheseR_document MaTheseR_check

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
krakenator_push_hook:
	scp ./hooks/post-receive.sh cayek@krakenator:/home/cayek/Gits/2017/Thesis.git/hooks/post-receive

krakenator_deploy:
	git status
	git commit --allow-empty -am "deploy on krakenator"
	git push krakenator master

krakenator_R: 
	ssh -X -t cayek@krakenator "cd ~/Projects/Thesis/; screen R"

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
		sshfs cayek@krakenator.imag.fr:/home/cayek/Projects/Thesis/OUTPUT OUTPUT -o allow_other; \
	fi

krakenator_umount_OUTPUT:
	echo "${green}=====Umount OUTPUT/=====${reset}"
	if [ $(HOSTNAME) == "timc-bcm-15.imag.fr" ] ; then \
		sudo umount OUTPUT; \
	fi

# patator
patator_push_hook:
	scp ./hooks/post-receive.sh cayek@patator:/home/cayek/Gits/2017/Thesis.git/hooks/post-receive

patator_deploy:
	git status
	git commit --allow-empty -am "deploy on patator"
	git push patator master

patator_R: 
	ssh -X -t cayek@patator "cd ~/Projects/Thesis/; screen R"

