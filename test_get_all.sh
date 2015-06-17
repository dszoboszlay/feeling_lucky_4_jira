#!/bin/bash
curl -s -X GET http://localhost:8001/issues | \
    json_pp
