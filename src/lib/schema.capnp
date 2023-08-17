@0xf1e82afdb86e2425;

interface Process {
    stdout @0 (data :Text) -> ();
    stderr @1 (data :Text) -> ();
    stdin  @2 () -> (data :Text);
}

interface Logger {
  log @0 (msg :Text) -> ();
}

interface Client {
  shell @0 (id :Text) -> (process :Process);
  build @1 (logger :Logger, ctx :Text, spec :Text) -> (id :Text);
}

interface Admin {
  addClient    @0 (id :Text) -> (cap :Client);
  # Return a new submission endpoint for client "id".
  # Returns an error if "id" is already registered.

  removeClient @1 (id :Text) -> (cap :Client);
  # Remove a client, so that the sturdy-ref can no longer be used to connect.

  listClients  @2 () -> (clients :List(Text));
}