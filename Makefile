# Makefile -  The Makefile for cybernetic-artworld.
# Copyright (C) 2009  Rob Myers rob@robmyers.org
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

default: all

cyberartist:	aesthetic-package.lisp \
		aesthetic.lisp \
		cyberartist-package.lisp \
		cyberartist.lisp \
		cyberartist.asd
	sbcl --noinform \
	--eval "(require 'asdf)" \
	--eval "(asdf:oos 'asdf:load-op 'cyberartist)" \
	--eval "(sb-ext:save-lisp-and-die \"cyberartist\" \
                                  	  :executable t \
                                          :toplevel #'cyberartist:run)"

cybercurator:	aesthetic-package.lisp \
		aesthetic.lisp \
		cybercurator-package.lisp \
		cybercurator.lisp \
		cybercurator.asd
	sbcl --noinform \
	--eval "(require 'asdf)" \
	--eval "(asdf:oos 'asdf:load-op 'cybercurator)" \
	--eval "(sb-ext:save-lisp-and-die \"cybercurator\" \
                                  	  :executable t \
                                          :toplevel #'cybercurator:run)"

cybercritic:	aesthetic-package.lisp \
		aesthetic.lisp \
		cybercritic-package.lisp \
		cybercritic.lisp \
		cybercritic.asd
	sbcl --noinform \
	--eval "(require 'asdf)" \
	--eval "(asdf:oos 'asdf:load-op 'cybercritic)" \
	--eval "(sb-ext:save-lisp-and-die \"cybercritic\" \
                                  	  :executable t \
                                          :toplevel #'cybercritic:run)"

cybercollector:	aesthetic-package.lisp \
		aesthetic.lisp \
		cybercollector-package.lisp \
		cybercollector.lisp \
		cybercollector.asd
	sbcl --noinform \
	--eval "(require 'asdf)" \
	--eval "(asdf:oos 'asdf:load-op 'cybercollector)" \
	--eval "(sb-ext:save-lisp-and-die \"cybercollector\" \
                                  	  :executable t \
                                          :toplevel #'cybercollector:run)"

all: cyberartist cybercurator cybercritic cybercollector

clean:
	rm -f cyberartist
	rm -f cybercurator
	rm -f cybercritic
	rm -f cybercollector
	rm -f *.fasl

distclean: clean
	rm -f random-state
