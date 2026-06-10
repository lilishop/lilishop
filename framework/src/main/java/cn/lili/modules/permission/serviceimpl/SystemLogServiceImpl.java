package cn.lili.modules.permission.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.permission.entity.vo.SystemLogVO;
import cn.lili.modules.permission.repository.SystemLogRepository;
import cn.lili.modules.permission.service.SystemLogService;
import co.elastic.clients.elasticsearch._types.query_dsl.Query;
import co.elastic.clients.elasticsearch._types.query_dsl.QueryBuilders;
import co.elastic.clients.elasticsearch._types.query_dsl.TextQueryType;
import co.elastic.clients.json.JsonData;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.elasticsearch.client.elc.NativeQuery;
import org.springframework.data.elasticsearch.client.elc.NativeQueryBuilder;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.stereotype.Service;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;

import java.util.ArrayList;
import java.util.List;

/**
 * 系统日志
 *
 * @author Chopper
 * @since 2020/11/17 3:45 下午
 */
@Service
@ConditionalOnProperty(value = "lili.data.elasticsearch.enabled", havingValue = "true", matchIfMissing = true)
public class SystemLogServiceImpl implements SystemLogService {

    @Autowired
    private SystemLogRepository systemLogRepository;

    /**
     * ES
     */
    @Autowired
    private ElasticsearchOperations restTemplate;

    @Override
    public void saveLog(SystemLogVO systemLogVO) {
        systemLogRepository.save(systemLogVO);
    }

    @Override
    public void deleteLog(List<String> id) {
        for (String s : id) {
            systemLogRepository.deleteById(s);
        }
    }

    @Override
    public void flushAll() {
        systemLogRepository.deleteAll();
    }

    @Override
    public IPage<SystemLogVO> queryLog(String storeId, String operatorName, String key, SearchVO searchVo, PageVO pageVO) {
        pageVO.setNotConvert(true);
        IPage<SystemLogVO> iPage = new Page<>();
        NativeQueryBuilder filterBuilder = createFilterBuilder(storeId, operatorName, key, searchVo);

        filterBuilder.withPageable(PageRequest.of(pageVO.getPageNumber() - 1, pageVO.getPageSize()));

        if (CharSequenceUtil.isNotEmpty(pageVO.getOrder()) && CharSequenceUtil.isNotEmpty(pageVO.getSort())) {
            filterBuilder.withSort(Sort.by(Sort.Direction.valueOf(pageVO.getOrder().toUpperCase()), pageVO.getSort()));
        } else {
            filterBuilder.withSort(Sort.by(Sort.Direction.DESC, "createTime"));
        }

        SearchHits<SystemLogVO> searchResult = restTemplate.search(filterBuilder.build(), SystemLogVO.class);
        iPage.setTotal(searchResult.getTotalHits());
        iPage.setRecords(searchResult.getSearchHits().stream().map(SearchHit::getContent).toList());
        return iPage;
    }


    private NativeQueryBuilder createFilterBuilder(String storeId, String operatorName, String key, SearchVO searchVo) {
        NativeQueryBuilder builder = NativeQuery.builder();
        List<Query> queries = new ArrayList<>();

        if (CharSequenceUtil.isNotEmpty(storeId)) {
            queries.add(QueryBuilders.match(m -> m.field("storeId").query(storeId)));
        }

        if (CharSequenceUtil.isNotEmpty(operatorName)) {
            queries.add(QueryBuilders.match(m -> m.field("username").query(operatorName)));
        }

        if (CharSequenceUtil.isNotEmpty(key)) {
            queries.add(QueryBuilders.multiMatch(m -> m.fields("requestUrl", "requestParam", "responseBody", "name", "customerLog", "ipInfo").query(key).fuzziness("AUTO").tieBreaker(0.3d).type(TextQueryType.BestFields)));
        }

        if (searchVo.getConvertStartDate() != null && searchVo.getConvertEndDate() != null) {
            queries.add(co.elastic.clients.elasticsearch._types.query_dsl.RangeQueryBuilders.date(d -> d.field("createTime")
                .gte(java.time.Instant.ofEpochMilli(searchVo.getConvertStartDate().getTime()).toString())
                .lte(java.time.Instant.ofEpochMilli(searchVo.getConvertEndDate().getTime()).toString()))._toQuery());
        }

        if (!queries.isEmpty()) {
            builder.withQuery(QueryBuilders.bool(b -> b.must(queries)));
        } else {
            builder.withQuery(q -> q.matchAll(a -> a));
        }
        return builder;
    }

}
