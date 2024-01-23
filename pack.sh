#!/bin/bash

DIR=$(pwd)
rm -rfd /tmp/ps386038 && \
rm -rfd *.tar.gz && \
cd .. && \
cp -R $DIR /tmp/ps386038 && \
cd $DIR && \
rm -rfd /tmp/ps386038/.git && \
rm -rfd /tmp/ps386038/.stack-work && \
rm -rfd /tmp/ps386038/src/Runtime/dependencies/* && \
rm -rfd /tmp/ps386038/tests && \
cd /tmp/ && \
tar -czvf ps386038.tar.gz ./ps386038 && \
cd $DIR && \
rm -rfd ./ps386038 && \
mv /tmp/ps386038.tar.gz .
