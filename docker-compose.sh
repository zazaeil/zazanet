eval "export $(cat .env.src | paste -s -d" ")"
eval "export $(cat .env.prod.src | paste -s -d" ")"
env | grep ZNET_ | paste -s -d"\n" > .env
sudo docker-compose up --build
