package cn.lili.elasticsearch;

import cn.hutool.core.bean.BeanUtil;
import cn.lili.elasticsearch.config.ElasticsearchProperties;
import lombok.extern.slf4j.Slf4j;
import org.elasticsearch.ElasticsearchException;
import org.elasticsearch.action.ActionListener;
import org.elasticsearch.action.admin.indices.delete.DeleteIndexRequest;
import org.elasticsearch.action.delete.DeleteRequest;
import org.elasticsearch.action.index.IndexRequest;
import org.elasticsearch.action.search.SearchRequest;
import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.action.support.master.AcknowledgedResponse;
import org.elasticsearch.action.update.UpdateRequest;
import org.elasticsearch.client.HttpAsyncResponseConsumerFactory;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.client.indices.CreateIndexRequest;
import org.elasticsearch.client.indices.CreateIndexResponse;
import org.elasticsearch.client.indices.GetIndexRequest;
import org.elasticsearch.client.indices.PutMappingRequest;
import org.elasticsearch.common.settings.Settings;
import org.elasticsearch.common.xcontent.XContentType;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.builder.SearchSourceBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import java.io.IOException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

/**
 * @author paulG
 * @since 2020/10/14
 **/
@Slf4j
public abstract class BaseElasticsearchService {

    protected static final RequestOptions COMMON_OPTIONS;

    static {
        RequestOptions.Builder builder = RequestOptions.DEFAULT.toBuilder();

        //默认缓冲限制为100MB，此处修改为30MB。
        builder.setHttpAsyncResponseConsumerFactory(new HttpAsyncResponseConsumerFactory.HeapBufferedResponseConsumerFactory(30 * 1024 * 1024));
        COMMON_OPTIONS = builder.build();
    }

    @Autowired
    @Qualifier("elasticsearchClient")
    protected RestHighLevelClient client;

    @Autowired
    private ElasticsearchProperties elasticsearchProperties;

    /**
     * build DeleteIndexRequest
     *
     * @param index elasticsearch index name
     * @author fxbin
     */
    private static DeleteIndexRequest buildDeleteIndexRequest(String index) {
        return new DeleteIndexRequest(index);
    }

    /**
     * build IndexRequest
     *
     * @param index  elasticsearch index name
     * @param id     request object id
     * @param object request object
     * @return {@link IndexRequest}
     * @author fxbin
     */
    protected static IndexRequest buildIndexRequest(String index, String id, Object object) {
        return new IndexRequest(index).id(id).source(BeanUtil.beanToMap(object), XContentType.JSON);
    }

    /**
     * create elasticsearch index (asyc)
     *
     * @param index elasticsearch index
     * @author fxbin
     */
    protected void createIndexRequest(String index) {
        try {
            CreateIndexRequest request = new CreateIndexRequest(index);
            //Settings for this index
            request.settings(Settings.builder()
                    .put("index.number_of_shards", elasticsearchProperties.getIndex().getNumberOfShards())
                    .put("index.number_of_replicas", elasticsearchProperties.getIndex().getNumberOfReplicas())
                    .put("index.mapping.total_fields.limit", 2000));

            //创建索引
            CreateIndexResponse createIndexResponse = client.indices().create(request, COMMON_OPTIONS);
            createMapping(index);
            log.info(" whether all of the nodes have acknowledged the request : {}", createIndexResponse.isAcknowledged());
            log.info(" Indicates whether the requisite number of shard copies were started for each shard in the index before timing out :{}", createIndexResponse.isShardsAcknowledged());
        } catch (Exception e) {
            log.error("创建索引错误",e);
            throw new ElasticsearchException("创建索引 {" + index + "} 失败：" + e.getMessage());
        }
    }

    public void createMapping(String index) throws Exception {
        String source =
                " {\n" +
                        "    \"properties\": {\n" +
                        "      \"_class\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"attrList\": {\n" +
                        "        \"type\": \"nested\",\n" +
                        "        \"properties\": {\n" +
                        "          \"name\": {\n" +
                        "            \"type\": \"keyword\"\n" +
                        "          },\n" +
                        "          \"type\": {\n" +
                        "            \"type\": \"long\"\n" +
                        "          },\n" +
                        "          \"value\": {\n" +
                        "            \"type\": \"keyword\"\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"brandId\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fielddata\": true,\n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"brandName\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fielddata\": true,\n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"brandUrl\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fielddata\": true,\n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"buyCount\": {\n" +
                        "        \"type\": \"long\"\n" +
                        "      },\n" +
                        "      \"releaseTime\": {\n" +
                        "        \"type\": \"date\",\n" +
                        "        \"format\": \"yyyy-MM-dd HH:mm:ss||yyyy-MM-dd||epoch_millis\"\n" +
                        "      },\n" +
                        "      \"categoryPath\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fielddata\": true,\n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"categoryNamePath\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fielddata\": true,\n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"commentNum\": {\n" +
                        "        \"type\": \"long\"\n" +
                        "      },\n" +
                        "      \"skuSource\": {\n" +
                        "        \"type\": \"long\"\n" +
                        "      },\n" +
                        "      \"goodsId\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"goodsName\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fielddata\": true, \n" +
                        "        \"analyzer\": \"ik_max_word\",\n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"grade\": {\n" +
                        "        \"type\": \"float\"\n" +
                        "      },\n" +
                        "      \"highPraiseNum\": {\n" +
                        "        \"type\": \"long\"\n" +
                        "      },\n" +
                        "      \"id\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"intro\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"authFlag\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"marketEnable\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fielddata\": true, \n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"mobileIntro\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"point\": {\n" +
                        "        \"type\": \"long\"\n" +
                        "      },\n" +
                        "      \"price\": {\n" +
                        "        \"type\": \"float\"\n" +
                        "      },\n" +
                        "      \"salesModel\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"recommend\": {\n" +
                        "        \"type\": \"boolean\"\n" +
                        "      },\n" +
                        "      \"selfOperated\": {\n" +
                        "        \"type\": \"boolean\"\n" +
                        "      },\n" +
                        "      \"sellerId\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"sellerName\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fielddata\": true, \n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"shopCategoryPath\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fielddata\": true, \n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"sn\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      },\n" +
                        "      \"promotionMapJson\": {\n" +
                        "        \"type\": \"text\"\n" +
                        "      },\n" +
                        "      \"thumbnail\": {\n" +
                        "        \"type\": \"text\",\n" +
                        "        \"fields\": {\n" +
                        "          \"keyword\": {\n" +
                        "            \"type\": \"keyword\",\n" +
                        "            \"ignore_above\": 256\n" +
                        "          }\n" +
                        "        }\n" +
                        "      }\n" +
                        "    }\n" +
                        "  }\n";

        PutMappingRequest request = new PutMappingRequest(index)
                        .source(source, XContentType.JSON);
        CountDownLatch latch = new CountDownLatch(1);
        AtomicReference<AcknowledgedResponse> response = new AtomicReference<>();
        client.indices().putMappingAsync(
                request,
                RequestOptions.DEFAULT,
                new ActionListener<AcknowledgedResponse>() {
                    @Override
                    public void onResponse(AcknowledgedResponse r) {
                        response.set(r);
                        latch.countDown();
                    }

                    @Override
                    public void onFailure(Exception e) {
                        latch.countDown();
                    }
                });
        latch.await(10, TimeUnit.SECONDS);
    }

    /**
     * Description: 判断某个index是否存在
     *
     * @param index index名
     * @return boolean
     * @author fanxb
     * @since 2019/7/24 14:57
     */
    public boolean indexExist(String index) {
        try {
            GetIndexRequest request = new GetIndexRequest(index);
            request.local(false);
            request.humanReadable(true);
            request.includeDefaults(false);
            return client.indices().exists(request, RequestOptions.DEFAULT);
        } catch (Exception e) {
            throw new ElasticsearchException("获取索引 {" + index + "} 是否存在失败：" + e.getMessage());
        }
    }

    /**
     * delete elasticsearch index
     *
     * @param index elasticsearch index name
     * @author fxbin
     */
    protected void deleteIndexRequest(String index) {
        DeleteIndexRequest deleteIndexRequest = buildDeleteIndexRequest(index);
        try {
            client.indices().delete(deleteIndexRequest, COMMON_OPTIONS);
        } catch (IOException e) {
            throw new ElasticsearchException("删除索引 {" + index + "} 失败：" + e.getMessage());
        }
    }

    /**
     * exec updateRequest
     *
     * @param index  elasticsearch index name
     * @param id     Document id
     * @param object request object
     * @author fxbin
     */
    protected void updateRequest(String index, String id, Object object) {
        try {
            UpdateRequest updateRequest = new UpdateRequest(index, id).doc(BeanUtil.beanToMap(object), XContentType.JSON);
            client.update(updateRequest, COMMON_OPTIONS);
        } catch (IOException e) {
            throw new ElasticsearchException("更新索引 {" + index + "} 数据 {" + object + "} 失败: " + e.getMessage());
        }
    }

    /**
     * exec deleteRequest
     *
     * @param index elasticsearch index name
     * @param id    Document id
     * @author fxbin
     */
    protected void deleteRequest(String index, String id) {
        try {
            DeleteRequest deleteRequest = new DeleteRequest(index, id);
            client.delete(deleteRequest, COMMON_OPTIONS);
        } catch (IOException e) {
            throw new ElasticsearchException("删除索引 {" + index + "} 数据id {" + id + "} 失败: " + e.getMessage());
        }
    }

    /**
     * search all
     *
     * @param index elasticsearch index name
     * @return {@link SearchResponse}
     * @author fxbin
     */
    protected SearchResponse search(String index) {
        SearchRequest searchRequest = new SearchRequest(index);
        SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
        searchSourceBuilder.query(QueryBuilders.matchAllQuery());
        searchRequest.source(searchSourceBuilder);
        SearchResponse searchResponse = null;
        try {
            searchResponse = client.search(searchRequest, COMMON_OPTIONS);
        } catch (IOException e) {
            log.error("es 搜索错误",e);
        }
        return searchResponse;
    }


}
