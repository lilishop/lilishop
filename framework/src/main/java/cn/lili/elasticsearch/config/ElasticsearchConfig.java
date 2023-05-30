package cn.lili.elasticsearch.config;

import cn.hutool.core.convert.Convert;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.HttpHost;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.conn.ConnectionKeepAliveStrategy;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.nio.reactor.IOReactorConfig;
import org.elasticsearch.client.RestClient;
import org.elasticsearch.client.RestClientBuilder;
import org.elasticsearch.client.RestHighLevelClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.elasticsearch.config.AbstractElasticsearchConfiguration;

import javax.annotation.PreDestroy;
import java.io.IOException;
import java.util.List;

/**
 * elasticsearch 配置
 *
 * @author paulG
 * @since 2020/10/13
 **/
@Slf4j
@Configuration
public class ElasticsearchConfig extends AbstractElasticsearchConfiguration {

    @Autowired
    private ElasticsearchProperties elasticsearchProperties;

    private RestHighLevelClient client;

    @Override
    @Bean
    public RestHighLevelClient elasticsearchClient() {
        RestClientBuilder restBuilder = RestClient
                .builder(this.getHttpHosts());
        restBuilder.setHttpClientConfigCallback(httpClientBuilder ->
                httpClientBuilder
                        .setKeepAliveStrategy(getConnectionKeepAliveStrategy())
                        .setMaxConnPerRoute(10).
                        setDefaultIOReactorConfig(IOReactorConfig.custom().setIoThreadCount(1).build()));
        String username = elasticsearchProperties.getAccount().getUsername();
        String password = elasticsearchProperties.getAccount().getPassword();
        if (username != null && password != null) {
            final CredentialsProvider credential = new BasicCredentialsProvider();
            credential.setCredentials(AuthScope.ANY, new UsernamePasswordCredentials(username, password));
            restBuilder.setHttpClientConfigCallback(httpClientBuilder ->
                    httpClientBuilder
                            .setDefaultCredentialsProvider(credential)
                            .setKeepAliveStrategy(getConnectionKeepAliveStrategy())
                            .setMaxConnPerRoute(10)
                            .setDefaultIOReactorConfig(IOReactorConfig.custom().setIoThreadCount(Runtime.getRuntime().availableProcessors()).build()));
        }

        restBuilder.setRequestConfigCallback(requestConfigBuilder ->
                requestConfigBuilder.setConnectTimeout(1000) //time until a connection with the server is established.
                        .setSocketTimeout(12 * 1000) //time of inactivity to wait for packets[data] to receive.
                        .setConnectionRequestTimeout(-1)); //time to fetch a connection from the connection pool 0 for infinite.

        client = new RestHighLevelClient(restBuilder);
        return client;
    }

    private HttpHost[] getHttpHosts() {
        List<String> clusterNodes = elasticsearchProperties.getClusterNodes();
        HttpHost[] httpHosts = new HttpHost[clusterNodes.size()];
        for (int i = 0; i < clusterNodes.size(); i++) {
            String[] node = clusterNodes.get(i).split(":");
            httpHosts[i] = new HttpHost(node[0], Convert.toInt(node[1]), elasticsearchProperties.getSchema());
        }
        return httpHosts;
    }

    private ConnectionKeepAliveStrategy getConnectionKeepAliveStrategy() {
        return (response, context) -> 2 * 60 * 1000;
    }

    /**
     * it gets called when bean instance is getting removed from the context if
     * scope is not a prototype
     * If there is a method named shutdown or close then spring container will try
     * to automatically configure them as callback methods when bean is being
     * destroyed
     */
    @PreDestroy
    public void clientClose() {
        try {
            this.client.close();
        } catch (IOException e) {
            log.error("es clientClose错误", e);
        }
    }

}
