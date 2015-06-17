#!/bin/bash
curl -s -X POST http://localhost:8001/return | \
    json_pp
