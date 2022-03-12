# vCard Studio

> A contact management application with support for vCard file format (.vcf). 

Forked from https://app.zdechov.net/vcard-studio. Please consider using the
upstream project!

Also, see the [original readme](Read Me.txt)

## Building via Docker

### Creating a .deb package

Requirements:

- Docker and docker-compose must be installed

From the root directory of this repository, run:

```bash
docker-compose build debian-lazbuild
docker-compose run debian-lazbuild
```

This will produce a `*.deb` file in the `dist` directory, which you can install
on Debian or Ubuntu by running:

```bash
dpkg -i dist/*.dpkg
```

