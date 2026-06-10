package cn.lili.elasticsearch.config;

import lombok.extern.slf4j.Slf4j;
import org.apache.http.conn.ConnectionKeepAliveStrategy;
import org.apache.http.impl.nio.reactor.IOReactorConfig;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.data.elasticsearch.client.ClientConfiguration;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchClients;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchConfiguration;
import org.springframework.data.elasticsearch.repository.config.EnableElasticsearchRepositories;

/**
 * elasticsearch 配置
 *
 * @author paulG
 * @since 2020/10/13
 **/
@Slf4j
@Configuration
@ConditionalOnProperty(value = "lili.data.elasticsearch.enabled", havingValue = "true", matchIfMissing = true)
@EnableElasticsearchRepositories(basePackages = "cn.lili.modules.*.repository")
public class ElasticsearchConfig extends ElasticsearchConfiguration {

    @Autowired
    private ElasticsearchProperties elasticsearchProperties;

    /**
     * Checks if the required credentials are present.
     *
     * @param username The username to check.
     * @param password The password to check.
     * @return True if both the username and password are not null, false otherwise.
     */
    private boolean isRequiredCredentialsPresent(String username, String password) {
        return username != null && password != null;
    }

    /**
     * Returns the ConnectionKeepAliveStrategy for maintaining persistent connections with the server.
     *
     * @return The ConnectionKeepAliveStrategy.
     */
    private ConnectionKeepAliveStrategy getConnectionKeepAliveStrategy() {
        return (response, context) -> 2 * 60 * 1000;
    }


    @NotNull
    @Override
    public ClientConfiguration clientConfiguration() {

        String username = elasticsearchProperties.getAccount().getUsername();
        String password = elasticsearchProperties.getAccount().getPassword();

        ClientConfiguration.MaybeSecureClientConfigurationBuilder builder = ClientConfiguration.builder().connectedTo(elasticsearchProperties.getClusterNodes());

        if (isRequiredCredentialsPresent(username, password)) {
            builder.withBasicAuth(username, password);
        }

        if (elasticsearchProperties.getSchema().equals("https")) {
            builder.usingSsl();
        }

        builder.withConnectTimeout(elasticsearchProperties.getConnectTimeout());
        builder.withSocketTimeout(elasticsearchProperties.getSocketTimeout());

        builder.withClientConfigurer(
                ElasticsearchClients.ElasticsearchHttpClientConfigurationCallback.from(clientBuilder -> {
                            clientBuilder.setKeepAliveStrategy(getConnectionKeepAliveStrategy());
                            clientBuilder.setMaxConnTotal(elasticsearchProperties.getMaxConnectTotal());
                            clientBuilder.setMaxConnPerRoute(10);
                            clientBuilder.setDefaultIOReactorConfig(IOReactorConfig.custom().setIoThreadCount(Runtime.getRuntime().availableProcessors()).build());
                            return clientBuilder;
                        }
                ));
        return builder.build();
    }

}
