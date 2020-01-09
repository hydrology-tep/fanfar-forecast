FROM docker-co.terradue.com/hydrology-tep/fanfar-forecast:1.3
MAINTAINER "Blasco Brauzzi" <blasco.brauzzi@terradue.com>

RUN yum downgrade proj -y
