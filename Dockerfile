FROM ubuntu:18.04

ARG DISPLAY=local
ENV DISPLAY ${DISPLAY}
ENV DEBIAN_FRONTEND=noninteractive

RUN mkdir /usr/esru \
&& apt-get update \
&& apt-get install -y apt-utils \
&& apt-get install -y expat \
&& apt-get install -y libexpat1-dev \
&& apt-get install -y nano \
&& apt-get install -y gcc \
&& apt-get install -y gfortran \
&& apt-get install -y g++ \
&& apt-get install -y build-essential \
&& apt-get install -y libxml2-dev \
&& apt-get install -y libxslt-dev \
&& apt-get install -y git \
&& apt-get install -y subversion \
&& apt-get install -y sudo \
&& apt-get install -y libx11-dev \
&& apt-get install -y xauth \
&& apt-get install -y xorg \
&& apt-get install -y x11-apps \
&& apt-get install -y libxft-dev \
&& apt-get install -y xterm \
&& apt-get install -y wget

# Run clean up
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /downloads/*

# Configure the user
RUN useradd -ms /bin/bash user
RUN chown -R user: /home
RUN chown -R user: /usr/esru

# Switch to User and configure bashrc
USER user

#Add Git support and color to bash
RUN cp /usr/lib/git-core/git-sh-prompt ~/.git-prompt.sh \
&& echo 'source ~/.git-prompt.sh' >> ~/.bashrc \
&& echo 'red=$(tput setaf 1) && green=$(tput setaf 2) && yellow=$(tput setaf 3) &&  blue=$(tput setaf 4) && magenta=$(tput setaf 5) && reset=$(tput sgr0) && bold=$(tput bold)' >> ~/.bashrc \
&& echo PS1=\''\[$green\]\u\[$reset\]@\[$green\]\h\[$reset\]:\[$green\]\w\[$reset\]\[$yellow\][$(__git_ps1 "%s")]\[$reset\]\$'\' >> ~/.bashrc

CMD ["/bin/bash"]