FROM alpine

RUN apk add --no-cache avahi augeas

WORKDIR /app

ADD avahi-daemon.conf /etc/avahi/avahi-daemon.conf

COPY entrypoint.sh entrypoint.sh

RUN chmod +x entrypoint.sh

ENTRYPOINT ./entrypoint.sh
