#!/bin/sh

xml_zazanet_backend='<?xml version="1.0" standalone="no"?>
    <!DOCTYPE service-group SYSTEM "avahi-service.dtd">
    <service-group>
    <name replace-wildcards="yes">zazanet backend</name>
    <service protocol="ipv4">
    <type>_http._tcp</type>'
xml_zazanet_backend="${xml_zazanet_backend}<port>${PORT}</port>"
xml_zazanet_backend="${xml_zazanet_backend}
    <txt-record>zazanet-service=backend</txt-record>
    <txt-record>zazanet-service-version=0.0.1</txt-record>
    </service>
    </service-group>"

echo $xml_zazanet_backend > /etc/avahi/services/zazanet_backend.service

xml_zazanet_logs='<?xml version="1.0" standalone="no"?>
    <!DOCTYPE service-group SYSTEM "avahi-service.dtd">
    <service-group>
    <name replace-wildcards="yes">zazanet logs</name>
    <service protocol="ipv4">
    <type>_http._tcp</type>'
xml_zazanet_logs="${xml_zazanet_logs}<port>9200</port>"
xml_zazanet_logs="${xml_zazanet_logs}
    <txt-record>zazanet-service=logs</txt-record>
    </service>
    </service-group>"

echo $xml_zazanet_logs > /etc/avahi/services/elasticsearch.service

avahi-daemon
