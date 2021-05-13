package cn.lili.modules.search.repository;

import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;

/**
 * @author paulG
 * @date 2020/10/15
 **/
public interface EsGoodsIndexRepository extends ElasticsearchRepository<EsGoodsIndex, String> {

}
