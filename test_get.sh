#!/bin/bash
curl -s -X GET http://localhost:8001/issue | \
    json_pp
