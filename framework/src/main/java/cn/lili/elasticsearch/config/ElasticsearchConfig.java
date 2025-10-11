package cn.lili.elasticsearch.config;

import cn.hutool.core.convert.Convert;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.HttpHost;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.conn.ConnectionKeepAliveStrategy;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.nio.client.HttpAsyncClientBuilder;
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
        String username = elasticsearchProperties.getAccount().getUsername();
        String password = elasticsearchProperties.getAccount().getPassword();
        final CredentialsProvider credential = createCredentialsIfNotNull(username, password);

        RestClientBuilder restBuilder = createRestClientBuilderWithConfig(credential);

        client = new RestHighLevelClient(restBuilder);
        return client;
    }

    private CredentialsProvider createCredentialsIfNotNull(String username, String password) {
        if (username == null || password == null) {
            return null;
        }
        final CredentialsProvider credentialsProvider = new BasicCredentialsProvider();
        credentialsProvider.setCredentials(AuthScope.ANY, new UsernamePasswordCredentials(username, password));
        return credentialsProvider;
    }

    private RestClientBuilder createRestClientBuilderWithConfig(CredentialsProvider credentialsProvider) {
        return RestClient
                .builder(this.getHttpHosts())
                .setHttpClientConfigCallback(httpClientBuilder -> configureHttpClientBuilder(httpClientBuilder, credentialsProvider))
                .setRequestConfigCallback(requestConfigBuilder ->
                        requestConfigBuilder
                                .setConnectTimeout(1000)
                                .setSocketTimeout(12 * 1000)
                                .setConnectionRequestTimeout(1000));
    }

    private HttpAsyncClientBuilder configureHttpClientBuilder(HttpAsyncClientBuilder httpClientBuilder,
                                                              CredentialsProvider credentialsProvider) {
        httpClientBuilder
                .setKeepAliveStrategy(getConnectionKeepAliveStrategy())
                .setMaxConnPerRoute(50)
                .setMaxConnTotal(200)
                .setDefaultIOReactorConfig(
                        IOReactorConfig
                                .custom()
                                .setIoThreadCount(Runtime.getRuntime().availableProcessors())
                                .build());
        if (credentialsProvider != null) {
            httpClientBuilder.setDefaultCredentialsProvider(credentialsProvider);
        }
        return httpClientBuilder;
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
