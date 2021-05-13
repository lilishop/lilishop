package cn.lili.modules.statistics.service;

import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.statistics.model.dto.GoodsStatisticsQueryParam;
import cn.lili.modules.statistics.model.vo.CategoryStatisticsDataVO;
import cn.lili.modules.statistics.model.vo.GoodsStatisticsDataVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 商品统计业务层
 *
 * @author Bulbasaur
 * @date 2020/12/9 11:06
 */
public interface GoodsStatisticsDataService extends IService<StoreFlow> {

    /**
     * 查询热卖商品
     * 查询TOP100的商品
     *
     * @param goodsStatisticsQueryParam 查询参数
     * @param num                       数量
     * @return
     */
    List<GoodsStatisticsDataVO> getGoodsStatisticsData(GoodsStatisticsQueryParam goodsStatisticsQueryParam, Integer num);

    /**
     * 查询行业统计
     * 根据商品一级分类ID查询
     *
     * @param goodsStatisticsQueryParam 查询参数
     * @return
     */
    List<CategoryStatisticsDataVO> getCategoryStatisticsData(GoodsStatisticsQueryParam goodsStatisticsQueryParam);

}