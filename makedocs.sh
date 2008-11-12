#!/bin/sh

clisp <<EOF
(require :lml)
(lml:process-dir ".")
EOF

