#!/bin/sh
dest=/home/cayek/Projects/Thesis/MaThese/

echo "post-receive hook"

while read oldrev newrev ref
do
    if [[ $ref =~ .*/master$ ]];
    then
        echo "Master ref received.  Deploying master branch to production..."
        git --work-tree=$dest --git-dir=/home/cayek/Gits/2017/MaThese.git checkout -f
	      cd $dest
	      R CMD INSTALL --no-multiarch --with-keep.source Rpackage
    else
        echo "Ref $ref successfully received.  Doing nothing: only the master branch may be deployed on this server."
    fi
done
