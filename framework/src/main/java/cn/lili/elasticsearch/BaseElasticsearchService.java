package cn.lili.elasticsearch;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;

/**
 * @author paulG
 * @since 2020/10/14
 **/
@Slf4j
public abstract class BaseElasticsearchService {

    @Autowired
    public ElasticsearchOperations client;

}
