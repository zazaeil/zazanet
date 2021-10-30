FROM alpine

RUN apk add --no-cache avahi augeas

WORKDIR /app

ADD avahi-daemon.conf /etc/avahi/avahi-daemon.conf

COPY Avahi.Dockerfile.Entrypoint.sh Avahi.Dockerfile.Entrypoint.sh

RUN chmod +x Avahi.Dockerfile.Entrypoint.sh

ENTRYPOINT ./Avahi.Dockerfile.Entrypoint.sh
