package cn.lili.modules.statistics.service;

import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.statistics.entity.dto.GoodsStatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.CategoryStatisticsDataVO;
import cn.lili.modules.statistics.entity.vo.GoodsStatisticsDataVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 商品统计业务层
 *
 * @author Bulbasaur
 * @since 2020/12/9 11:06
 */
public interface GoodsStatisticsService extends IService<Goods> {

    /**
     * 获取所有的已上架的商品数量
     *
     * @param goodsAuthEnum   商品审核枚举
     * @param goodsStatusEnum 商品状态枚举
     * @return 所有的已上架的商品数量
     */
    Integer goodsNum(GoodsStatusEnum goodsStatusEnum, GoodsAuthEnum goodsAuthEnum);

    /**
     * 获取今天的已上架的商品数量
     *
     * @return 今天的已上架的商品数量
     */
    Integer todayUpperNum();
}