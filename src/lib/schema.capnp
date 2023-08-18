@0xf1e82afdb86e2425;

interface Process {
    stdout @0 () -> (data :Text);
    stderr @1 () -> (data :Text);
    stdin  @2 (data :Text) -> ();
}

interface Job {
  log     @0 (start :Int64) -> (log :Data, next :Int64);
  # Return a chunk of log data starting at byte offset "start" and the
  # value to use for "start" in the next call to continue reading.
  # Returns 0 bytes of data to indicate end-of-file.
  # If the log is incomplete and there is no data currently available,
  # it waits until something changes before returning.
  # If "start" is negative then it is relative to the end of the log.

  result @1 () -> (output :Text);
  # Waits for the job to finish. Resolves to an error if the job fails.
  # The output depends on the job type. For a "docker push", it is the RepoId of
  # the pushed image.

  cancel @2 () -> ();
  # Request that the job be cancelled.
  # Note: jobs will be also cancelled if their reference count reaches zero.
}

interface Client {
  shell @0 (id :Text) -> (process :Process);
  build @1 (ctx :Text, spec :Text) -> (job :Job);
}

interface Admin {
  addClient    @0 (id :Text) -> (cap :Client);
  # Return a new submission endpoint for client "id".
  # Returns an error if "id" is already registered.

  removeClient @1 (id :Text) -> (cap :Client);
  # Remove a client, so that the sturdy-ref can no longer be used to connect.

  listClients  @2 () -> (clients :List(Text));
}