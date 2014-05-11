# ocaml-bt

OCaml-BT is a small BitTorrent library and client written in OCaml.  It uses Lwt for concurrency.

## Installation

The easiest way is to use [OPAM](http://opam.ocaml.org).
```sh
opam install bt
```

Alternatively, clone from git and install manually:
```sh
cd ~/tmp
git clone https://github.com/nojb/ocaml-bt
cd ocaml-bt
make
make install
```

Either way the end-result will be a OCaml library (findlib name: `bt`) and a executable `otorrent`. 

### Usage

Right now the only supported way to download torrents is via the use of magnet
links, but other ways will hopefully be added soon. For example, the following
will download the "Ubuntu 13.10 Desktop Live ISO amd64" torrent to your current
working directory.

```sh
otorrent "magnet:?xt=urn:btih:e3811b9539cacff680e418124272177c47477157&dn=Ubuntu+13.10+Desktop+Live+ISO+amd64&tr=udp%3A//tracker.openbittorrent.com%3A80&tr=udp%3A//tracker.publicbt.com%3A80&tr=udp%3A//tracker.istole.it%3A6969&tr=udp%3A//tracker.ccc.de%3A80&tr=udp%3A//open.demonii.com%3A1337"
```

### TODO

- DHT
- uTP
- Fast Resume
- <del>Encryption</del>
- <del>PEX</del>
- LPD
- UPnP
- NAT-PMP
- SOCKS

## Comments

Comments, bug reports and feature requests are very welcome: n.oje.bar@gmail.com.
