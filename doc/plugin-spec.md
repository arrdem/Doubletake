# Language plugin specification #
### Overview ###

After evaluating several structures for a plugin system, I settled on a
self-registering model for plugins. Basically instead of requiring some sort
of standard file/folder structure, I will instead provide a spec for the
interface between the Doubletake core code and all language plugins.

Essentially I will gurantee that every file matching the name
"/doubletake/lang/*/core.clj". These file will be arbitrarily evaluated and
are expected to use the API described below to register themselves as valid
language processors.

## Doubletake API ##
provided in the namespace doubletake.plugin.api:
~~~~
register-file-type [file-extension processor]
    ; registers a new file extension and corresponding
    ; parser with the Doubletake core processor
    ;
    ; the parser argument should be the full name of the 
    ; processor function implemented by the plugin, and
    ; satisfying the processor standard documented seperately.
~~~~
And that's it! there is nothing else that a plugin needs to do, other than
make sure it does any required setup work and registers itself!
