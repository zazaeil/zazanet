# How to start?
Just run the following command:
```
sudo docker-compose build && sudo docker-compose PORT=[0..65535] LOGGER_LEVEL=[debug|notice|error] docker-compose up
```
It will run both the backend and mDNS to advertise it.
