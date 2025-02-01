#!/bin/bash
mill --ticker false boar.assembly
aws s3 cp ./out/boar/assembly.dest/out.jar s3://bruce-test.vivi.com/boar.jar
export PAGER=""
sleep 3
aws lambda update-function-code --region us-east-1 --function-name boar --s3-bucket bruce-test.vivi.com --s3-key boar.jar 

