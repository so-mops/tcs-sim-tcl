FROM ubuntu:18.04


COPY activetcl/ activetcl/
COPY src/ simulator/

workdir simulator/

RUN chmod +x /activetcl/auto-install.sh
RUN tar -xvf /activetcl/atcl.tar -C /activetcl/atcl --strip-components=1 && /activetcl/auto-install.sh

expose 5750

CMD /opt/ActiveTcl-8.6/bin/tclsh tcs-sim.tcl V06
