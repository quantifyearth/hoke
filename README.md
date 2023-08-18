hoke
----

> When I hoked there, I would find <br>
> An acorn and a rusted bolt.
>
>   -- Seamus Heaney

A daemon of sorts for [obuilder](https://github.com/ocurrent/obuilder). For now it mainly supports running a remote shell over a capnp endpoint or sending an obuilder build specification (and optional git endpoint) to a remote builder.

You can start the daemon with

```sh
hoke daemon --store=zfs:obuilder-zfs --capnp-secret-key-file=secret-key.pem --capnp-listen-address=unix:/tmp/ocurrent2.sock
```

And then you can get a new submission capability.

```sh
hoke add --connect ./secrets/admin.cap alice > secrets/submission.cap
```

Then it is probably easiest to put an Obuilder specification into a file. For example:

```
((from alpine)
 (run (shell "echo 'hello world'")))
```

Then send that to the daemon to build.

```sh
cat example.spec | hoke build --connect=./secrets/submission.cap
```

And then connect to the resulting ID with:

```sh
hoke shell --connect=./secrets/submission.cap --id=<hash>
```


