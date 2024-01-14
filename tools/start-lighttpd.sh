#!/bin/sh
vagrant up
vagrant ssh -c "docker compose --project-directory /vagrant up"