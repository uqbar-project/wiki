#!/bin/bash

echo -e "Starting to install of uqbar wiki dependencies \r"
sleep 1
echo -ne '#                     (5%)\r'
#Install the requirements for js first
npm install
echo -ne '###########             (50%)\r'
sleep 1
#then run bundle install for installing the required gems
bundle install
echo -ne '#######################   (100%)\r'
sleep 1
echo -ne 'Finishing Installation\n'
sleep 1
echo -ne '\n'