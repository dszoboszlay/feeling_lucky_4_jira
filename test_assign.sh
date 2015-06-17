#!/bin/bash
curl -s -X POST http://localhost:8001/issue | \
    json_pp
