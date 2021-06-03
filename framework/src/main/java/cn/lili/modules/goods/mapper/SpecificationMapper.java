package cn.lili.modules.goods.mapper;


import cn.lili.modules.goods.entity.dos.Specification;
import cn.lili.modules.goods.entity.vos.SpecificationVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 规格数据处理层
 *
 * @author pikachu
 * @date 2020-02-18 15:18:56
 */
public interface SpecificationMapper extends BaseMapper<Specification> {

    /**
     * 查询规格信息列表
     * @return
     */
    @Select("SELECT s.id, s.spec_name,s.create_time ,GROUP_CONCAT(sv.spec_value SEPARATOR ',') AS spec_value" +
            " FROM li_specification s LEFT JOIN li_spec_values sv ON s.id = sv.spec_id ${ew.customSqlSegment} ")
    List<SpecificationVO> findSpecList(@Param(Constants.WRAPPER) Wrapper queryWrapper);
}