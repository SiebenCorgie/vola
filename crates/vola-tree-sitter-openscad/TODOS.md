Collection of  tests / TODOs I'll have yet to check out.

# Test
- Test that we tranform the [Scope of variables](https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/General#Scope_of_variables) correctly
  - Are we handeling Anonymous scopes correctly (i.e ignore the scope)
  - Not sure how the assignment works actually given. I _think_ its is supposed to be `last-write-in-(not-anonymous)-scope` but it is actually not.
  ```openscad
  b = a;
  a = b;
  b = 55;
  echo(a); //55
  echo(b); //55
  ```
  but
  ```openscad
  a = b;
  b = 55;
  echo(a); //undef
  echo(b); //55
  ```
