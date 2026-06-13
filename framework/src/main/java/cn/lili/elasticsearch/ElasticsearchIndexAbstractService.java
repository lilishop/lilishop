package cn.lili.elasticsearch;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.exception.RetryException;
import co.elastic.clients.elasticsearch._types.FieldValue;
import co.elastic.clients.elasticsearch._types.query_dsl.QueryBuilders;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.elasticsearch.client.elc.NativeQuery;
import org.springframework.data.elasticsearch.client.elc.NativeQueryBuilder;
import org.springframework.data.elasticsearch.core.MultiGetItem;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.query.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * elasticsearch 索引抽象类
 * <p>
 * 定义一些索引常用方法
 *
 * @author paulG
 * @since 2022/8/5
 **/
@Slf4j
public abstract class ElasticsearchIndexAbstractService extends BaseElasticsearchService {

    private static final String IGNORE_FIELD = "serialVersionUID,promotionMap,id,goodsId";


    /**
     * 保存索引信息
     *
     * @param t 索引信息
     */
    public <T> T saveIndex(T t) {
        return this.client.save(t);
    }

    /**
     * 批量保存索引信息
     *
     * @param t 索引信息列表
     */
    public <T> Iterable<T> saveIndex(Iterable<T> t) {
        return this.client.save(t);
    }

    /**
     * 更新商品索引的的部分属性
     *
     * @param queryFields  查询字段
     * @param updateFields 更新字段
     * @param clazz        索引类
     */
    public <T> void updateIndexFields(Map<String, Object> queryFields, Map<String, Object> updateFields, Class<T> clazz) {
        if (queryFields == null || queryFields.isEmpty() || updateFields == null || updateFields.isEmpty()) {
            return;
        }
        NativeQueryBuilder builder = NativeQuery.builder();
        List<co.elastic.clients.elasticsearch._types.query_dsl.Query> queries = new ArrayList<>();
        for (Map.Entry<String, Object> entry : queryFields.entrySet()) {
            if (entry.getValue() instanceof String) {
                queries.add(QueryBuilders.terms(
                        t -> t.field(entry.getKey()).terms(
                                e -> e.value(List.of(FieldValue.of(entry.getValue().toString()))))
                ));
            }
            // 判断values是否为数组
            if (entry.getValue() instanceof List) {
                List<?> list = (List<?>) entry.getValue();
                List<FieldValue> fieldValues = new ArrayList<>();
                for (Object o : list) {
                    fieldValues.add(FieldValue.of(o.toString()));
                }
                queries.add(QueryBuilders.terms(
                        t -> t.field(entry.getKey()).terms(
                                e -> e.value(fieldValues))
                ));
            }
        }
        builder.withQuery(
                q -> q.bool(
                        t -> t.must(queries)
                )
        );

        StringBuilder script = new StringBuilder();
        for (Map.Entry<String, Object> entry : updateFields.entrySet()) {
            if (entry.getValue() instanceof String) {
                script.append("ctx._source.").append(entry.getKey()).append("=").append("'").append(entry.getValue()).append("'").append(";");
            } else {
                script.append("ctx._source.").append(entry.getKey()).append("=").append(entry.getValue()).append(";");
            }
        }


        log.debug("更新商品索引的的部分属性，查询条件：{}，更新字段：{}, 类型: {}", queryFields, updateFields, clazz);
        UpdateQuery updateQuery = UpdateQuery.builder(builder.build()).withScript(script.toString()).withLang("painless")
                .withAbortOnVersionConflict(false).build();
        try {
            ByQueryResponse byQueryResponse = this.client.updateByQuery(updateQuery, this.client.getIndexCoordinatesFor(clazz));
            if (byQueryResponse.getVersionConflicts() > 0) {
                throw new RetryException("更新商品索引失败，es内容版本冲突");
            }
        } catch (Exception e) {
            log.error("更新商品索引异常", e);
        }
    }

    /**
     * 批量更新
     *
     * @param updateQueries 更新列表
     * @param clazz         索引类
     * @param <T>           索引类
     */
    public <T> void updateBatch(List<UpdateQuery> updateQueries, Class<T> clazz) {
        this.client.bulkUpdate(updateQueries, clazz);
    }

    /**
     * 根据id获取索引信息
     *
     * @param id    id
     * @param clazz 索引类
     * @return 商品索引信息
     */
    public <T> T findById(String id, Class<T> clazz) {
        return this.client.get(id, clazz);
    }

    /**
     * 批量获取索引信息
     *
     * @param query 查询条件
     * @param clazz 索引类
     * @return 索引信息列表
     */
    public <T> List<T> findByQuery(Query query, Class<T> clazz) {
        List<T> indexList = new ArrayList<>();
        for (SearchHit<T> search : this.client.search(query, clazz)) {
            indexList.add(search.getContent());
        }
        return indexList;
    }

    /**
     * 批量获取索引信息
     *
     * @param ids   索引ids
     * @param clazz 索引类
     * @return 索引信息列表
     */
    public <T> List<T> findByQuery(List<String> ids, Class<T> clazz) {
        return this.client.multiGet(this.client.idsQuery(ids), clazz).stream().map(MultiGetItem::getItem).toList();
    }

    /**
     * 根据字段查询删除索引
     *
     * @param queryFields 删除条件的字段（key为字段名，value为字段值）
     * @param clazz       索引类
     */
    public void deleteIndex(Map<String, Object> queryFields, Class<?> clazz) {
        NativeQueryBuilder builder = NativeQuery.builder();

        for (Map.Entry<String, Object> entry : queryFields.entrySet()) {
            //根据分类ID删除商品索引
            if (CharSequenceUtil.equals(entry.getKey(), "categoryId")) {
                builder.withQuery(
                        q -> q.bool(
                                t -> t.should(
                                        s -> s.wildcard(
                                                w -> w.field("categoryPath").value("*" + entry.getValue() + "*")
                                        )
                                )
                        )
                );
            } else {
                builder.withQuery(
                        q -> q.terms(
                                t -> t.field(entry.getKey()).terms(
                                        e -> e.value(List.of(FieldValue.of(entry.getValue().toString()))))
                        )
                );
            }
        }

        try {
            DeleteQuery deleteQuery = DeleteQuery.builder(builder.build()).build();
            ByQueryResponse delete = client.delete(deleteQuery, clazz);
            log.info("删除索引成功，删除数量：{}", delete.getDeleted());
            if (delete.getVersionConflicts() > 0) {
                throw new RetryException("删除索引失败，es内容版本冲突");
            }
        } catch (Exception e) {
            log.error("删除索引出现异常", e);
        }
    }


    /**
     * 根据索引id删除索引
     *
     * @param id    索引id
     * @param clazz 索引类
     * @param <T>   索引类
     * @return 删除结果
     */
    public <T> String deleteIndexById(String id, Class<T> clazz) {
        return this.client.delete(id, clazz);
    }


    /**
     * 根据索引id批量删除索引
     *
     * @param ids   索引id列表
     * @param clazz 索引类
     * @param <T>   索引类
     */
    public <T> void deleteIndexByIds(List<String> ids, Class<T> clazz) {
        try {
            DeleteQuery dq = DeleteQuery.builder(this.client.idsQuery(ids)).build();
            ByQueryResponse deleteResponse = this.client.delete(dq, clazz);
            if (!deleteResponse.getFailures().isEmpty()) {
                log.error("删除索引出现错误：{}", deleteResponse.getFailures());
            }
        } catch (Exception e) {
            log.error("删除索引出现异常", e);
        }
    }

}
