package cn.lili.modules.goods.mapper;


import cn.lili.modules.goods.entity.dos.Specification;
import cn.lili.modules.goods.entity.vos.SpecificationVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;

import java.util.List;
import java.util.Map;

/**
 * 规格数据处理层
 *
 * @author pikachu
 * @date 2020-02-18 15:18:56
 */
public interface SpecificationMapper extends BaseMapper<Specification> {

    /**
     * 查询规格信息列表
     * @param param
     * @return
     */
    List<SpecificationVO> findSpecList(Map<String,Object> param);
}