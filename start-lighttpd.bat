@echo off
vagrant up
vagrant ssh -c "docker compose up --project-directory /vagrant"