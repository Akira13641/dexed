# Samples

Few things are directly usable as showcase in IZ.

## Dictionnary suffix array

### Command

    dub dictionnary_suffixarray.d

### Usage

Under linux only.

Type some partial words to get possible completions.
Type MM to switch to _the match mode_, then type full words to test their inclusion.
Type MP to switch back to _the partial mode_.
Type help for more information.

## Shm (IPC server/client)

### Commands and usage

In a first console, launch the server

    dub shm1.d

In a second console, launch the client

    dub shm2.d

Client will ask 64 times for 16 bytes keys and stop the server automatically.

