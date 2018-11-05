# XMPP server on MirageOS

## Demos

The demos folder serves as a reference to some basic necessities for the project.

Sockets is very simple, basically copied from the mirage website to check building of the unikernel and running locally.

Send-receive is still simple, basically being an echo server for whatever the user decides to send it.

xml-parsing is more complex, it handles the connections but also has to include the xml parsing section which is not so simple due to having to push data into a stream where the xml parser pulls it out the other side.

xml-parsing-pkg is an effort to split the code into a core package which abstracts away from the lower level detail of sending data back and forth. This is the final demo before starting the actual implementation.
