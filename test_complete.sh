#!/bin/bash
curl -s -X POST http://localhost:8001/complete | \
    json_pp
