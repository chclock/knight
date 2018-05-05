# knight

a html templating engine for scheme

#usage

1. raven install knight

2. 

```
(import (knight knight))

(render "<%= hello %> -- <%- hello %> -- <%# note %>"
    '((hello . "<world>")))
;; or
(render-file "demo.html" '((hello . "<world>")))
```

3. you can include file by

> <% include "header.html" %>  ;or    
> <% include "header" %>  ;or   
> <% include header %>

4. change default template suffix "ejs"

> (set-template-suffix! suffix)

5. change default template path "."

> (set-template-path! path)