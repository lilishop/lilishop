### IM 配置文档

#### websocket连接中，ws=http协议，wss=https协议，在前端配置时也许注意同步修改（PS：微信小程序只支持https）

微信小程序配置需额外配置：前往微信平台，登陆自己的小程序
https://mp.weixin.qq.com/ 

配置内容：  开发  ->  开发设置  ->  服务器域名  ->  socket合法域名  ->  添加 https://im-api.pickmall.cn(参考)

##### https NGINX配置参考

```
server {
listen       443 ssl;
ssl_certificate "/etc/nginx/ssl/pickmall.cn.pem";
ssl_certificate_key "/etc/nginx/ssl/pickmall.cn.key";
ssl_session_cache shared:SSL:1m;
ssl_session_timeout  10m;
ssl_ciphers HIGH:!aNULL:!MD5;
ssl_prefer_server_ciphers on;
include /etc/nginx/default.d/*.conf;
server_name  im-api.pickmall.cn;

location / {
           proxy_http_version 1.1;
           proxy_set_header Upgrade $http_upgrade;
           proxy_set_header Connection "Upgrade";
           proxy_set_header X-real-ip $remote_addr;
           proxy_set_header X-Forwarded-For $remote_addr;
           proxy_pass http://127.0.0.1:8885;
     }
}
```

##### http 参考配置

```
server {
listen       8885;
ssl_session_cache shared:SSL:1m;
ssl_session_timeout  10m;
ssl_ciphers HIGH:!aNULL:!MD5;
ssl_prefer_server_ciphers on;
include /etc/nginx/default.d/*.conf;
server_name  im-api.pickmall.cn;

location / {
           proxy_http_version 1.1;
           proxy_set_header Upgrade $http_upgrade;
           proxy_set_header Connection "Upgrade";
           proxy_set_header X-real-ip $remote_addr;
           proxy_set_header X-Forwarded-For $remote_addr;
           proxy_pass http://127.0.0.1:8885;
     }
}
```
