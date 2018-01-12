#!/usr/bin/env sh

nix-shell -p 'haskellPackages.ghcWithPackages (self: [self.mtl self.validation])'
