(import (knight knight))

(display (render "<%= hello %> -- <%- hello%> -- <%# note %>" '((hello . "<world>"))))