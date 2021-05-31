# racket project

[![Test](https://github.com/dannypsnl/racket-project/actions/workflows/test.yml/badge.svg)](https://github.com/dannypsnl/racket-project/actions/workflows/test.yml)
[![Coverage Status](https://coveralls.io/repos/github/dannypsnl/racket-project/badge.svg?branch=coverage-github-action)](https://coveralls.io/github/dannypsnl/racket-project?branch=coverage-github-action)

### How to use

```sh
sh ./replace.sh $your_project_name $your_github_id $your_name
# or want to keep origin github_id and name
sh ./replace.sh $your_project_name

# commit
rm ./replace.sh && git add . && git commit -m "rename"
```
