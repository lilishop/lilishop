package cn.lili.modules.permission.repository;

import cn.lili.modules.permission.entity.vo.SystemLogVO;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;

/**
 * 日志
 *
 * @author paulG
 * @since 2021/12/13
 **/
public interface SystemLogRepository extends ElasticsearchRepository<SystemLogVO, String> {

}
