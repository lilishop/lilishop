package cn.lili.modules.goods.mapper;

import cn.lili.modules.goods.entity.dos.GoodsSearchParam;
import cn.lili.modules.goods.entity.dto.GoodsSearchParamFacetRow;
import cn.lili.modules.goods.entity.dto.GoodsSkuDTO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 商品参数倒排数据处理层
 *
 * @author Mike
 * @date 2026-09-03
 */
public interface GoodsSearchParamMapper extends BaseMapper<GoodsSearchParam> {

    /**
     * 参数 selector：内层 SKU 候选集与列表共用 WHERE，外层按参数名/值聚合。
     * 已选参数名只在输出侧 NOT IN 隐藏，不影响候选集。
     */
    List<GoodsSearchParamFacetRow> selectSelectorParams(
            @Param(Constants.WRAPPER) Wrapper<GoodsSkuDTO> queryWrapper,
            @Param("excludeNames") List<String> excludeNames);
}
