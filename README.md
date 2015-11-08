A hacked-together task runner using `turtle` and `ascii-progress`. 

- Logs stdout/stderr with timestamps to `.tasks/<hash>/logs/<timestamp>`
- Records the last 5 run times in `.tasks/<hash>/times`
- Displays an ASCII progress bar

I intend to use this simple runner to refactor some internal bash scripts, which currently 

- Run isolated tasks in sequence, so output doesn't get garbled
- Require inspection of whatever side-effects the script was supposed to perform, to make sure it worked

Parallelism, logging, checking error codes, and outputting red text when things go wrong. Revolutionary.
