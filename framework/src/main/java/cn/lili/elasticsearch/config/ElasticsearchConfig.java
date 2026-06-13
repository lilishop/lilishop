package cn.lili.elasticsearch.config;

import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.elasticsearch.client.ClientConfiguration;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchConfiguration;
import org.springframework.data.elasticsearch.repository.config.EnableElasticsearchRepositories;
import org.springframework.data.elasticsearch.support.HttpHeaders;

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

    private static final String ES_COMPATIBLE_MEDIA_TYPE = "application/vnd.elasticsearch+json;compatible-with=";

    @Autowired
    private ElasticsearchProperties elasticsearchProperties;

    private boolean isRequiredCredentialsPresent(String username, String password) {
        return username != null && password != null;
    }

    @NotNull
    @Override
    public ClientConfiguration clientConfiguration() {

        String username = elasticsearchProperties.getAccount().getUsername();
        String password = elasticsearchProperties.getAccount().getPassword();

        ClientConfiguration.MaybeSecureClientConfigurationBuilder builder = ClientConfiguration.builder()
                .connectedTo(elasticsearchProperties.getClusterNodes());

        if (isRequiredCredentialsPresent(username, password)) {
            builder.withBasicAuth(username, password);
        }

        if ("https".equals(elasticsearchProperties.getSchema())) {
            builder.usingSsl();
        }

        Integer compatibilityVersion = elasticsearchProperties.getCompatibilityVersion();
        if (compatibilityVersion != null && compatibilityVersion > 0) {
            HttpHeaders compatibilityHeaders = new HttpHeaders();
            String compatibleMediaType = ES_COMPATIBLE_MEDIA_TYPE + compatibilityVersion;
            compatibilityHeaders.add("Accept", compatibleMediaType);
            compatibilityHeaders.add("Content-Type", compatibleMediaType);
            builder.withDefaultHeaders(compatibilityHeaders);
            log.info("Elasticsearch REST API compatibility mode enabled: version {}", compatibilityVersion);
        }

        builder.withConnectTimeout(elasticsearchProperties.getConnectTimeout());
        builder.withSocketTimeout(elasticsearchProperties.getSocketTimeout());

        return builder.build();
    }

}
