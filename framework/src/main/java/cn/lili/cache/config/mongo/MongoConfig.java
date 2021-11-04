package cn.lili.cache.config.mongo;

import com.mongodb.MongoClientSettings;
import com.mongodb.MongoCredential;
import com.mongodb.ServerAddress;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.mongodb.config.AbstractMongoClientConfiguration;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;

import java.util.ArrayList;
import java.util.List;

/**
 * @author paulG
 * @since 2020/10/22
 **/
@Configuration
@EnableMongoRepositories
public class MongoConfig extends AbstractMongoClientConfiguration {

    @Value("${spring.data.mongodb.database}")
    private String databaseName;

    @Value("${spring.data.mongodb.uri}")
    private List<String> uri = new ArrayList<>();

    @Value("${spring.data.mongodb.username}")
    private String username;

    @Value("${spring.data.mongodb.password}")
    private String password;

    @Value("${spring.data.mongodb.authentication-database}")
    private String authenticationDatabase;

    @Override
    protected String getDatabaseName() {
        return databaseName;
    }

    @Override
    protected void configureClientSettings(MongoClientSettings.Builder builder) {
        builder.credential(MongoCredential.createCredential(username, authenticationDatabase, password.toCharArray()))
                .applyToClusterSettings(settings -> {
                    List<ServerAddress> serverAddresses = new ArrayList<>();
                    for (String s : uri) {
                        String[] node = s.split(":");
                        serverAddresses.add(new ServerAddress(node[0], Integer.parseInt(node[1])));
                    }
                    settings.hosts(serverAddresses);
                });
    }

}
