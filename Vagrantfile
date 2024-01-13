# -*- mode: ruby -*-
# vi: set ft=ruby :

# All Vagrant configuration is done below. The "2" in Vagrant.configure
# configures the configuration version (we support older styles for
# backwards compatibility). Please don't change it unless you know what
# you're doing.
Vagrant.configure("2") do |config|
  #config.vm.box = "generic/rocky9"
  #config.vm.box = "rockylinux/9"
  config.vm.box = "rad29ndrei/rocky9"
  config.vm.network "forwarded_port", guest: 8443, host: 8443

  config.vm.provision "ansible_local" do |ansible|
    ansible.playbook = "provisioner.yaml"
    ansible.provisioning_path = "/vagrant/vagrant-environment/ansible"
  end
end
