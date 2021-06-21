package cn.lili.modules.statistics.serviceimpl;

import cn.lili.common.utils.StringUtils;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.order.order.entity.enums.FlowTypeEnum;
import cn.lili.modules.statistics.mapper.GoodsStatisticsDataMapper;
import cn.lili.modules.statistics.model.dto.GoodsStatisticsQueryParam;
import cn.lili.modules.statistics.model.enums.StatisticsQuery;
import cn.lili.modules.statistics.model.vo.CategoryStatisticsDataVO;
import cn.lili.modules.statistics.model.vo.GoodsStatisticsDataVO;
import cn.lili.modules.statistics.service.GoodsStatisticsDataService;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

/**
 * 商品统计业务层实现
 *
 * @author Bulbasaur
 * @date 2020/12/9 11:30
 */
@Service
public class GoodsStatisticsDataServiceImpl extends ServiceImpl<GoodsStatisticsDataMapper, StoreFlow> implements GoodsStatisticsDataService {

    @Override
    public List<GoodsStatisticsDataVO> getGoodsStatisticsData(GoodsStatisticsQueryParam goodsStatisticsQueryParam, Integer num) {
        //获取查询条件
        QueryWrapper queryWrapper = getQueryWrapper(goodsStatisticsQueryParam);
        //根据商品分组
        queryWrapper.groupBy("goods_id");
        queryWrapper.groupBy("goods_name");

        queryWrapper.eq(!StringUtils.isEmpty(goodsStatisticsQueryParam.getStoreId()), "store_id", goodsStatisticsQueryParam.getStoreId());
        //查询前X记录
        Page page = new Page<GoodsStatisticsDataVO>(1, num);
        return this.baseMapper.getGoodsStatisticsData(page, queryWrapper);
    }

    @Override
    public List<CategoryStatisticsDataVO> getCategoryStatisticsData(GoodsStatisticsQueryParam goodsStatisticsQueryParam) {
        //获取查询条件
        QueryWrapper queryWrapper = getQueryWrapper(goodsStatisticsQueryParam);
        //根据分类分组
        queryWrapper.groupBy("category_id");
        return this.baseMapper.getCateGoryStatisticsData(queryWrapper);
    }


    private QueryWrapper getQueryWrapper(GoodsStatisticsQueryParam goodsStatisticsQueryParam) {

        QueryWrapper queryWrapper = Wrappers.query();
        //判断搜索类型是：年、月
        Date[] date = StatisticsDateUtil.getDateArray(goodsStatisticsQueryParam);
        queryWrapper.between("create_time", date[0], date[1]);

        //判断是按照数量统计还是按照金额统计
        if (goodsStatisticsQueryParam.getType().equals(StatisticsQuery.PRICE.name())) {
            queryWrapper.orderByDesc("price");
        } else {
            queryWrapper.orderByDesc("num");
        }
        //设置为付款查询
        queryWrapper.eq("flow_type", FlowTypeEnum.PAY.name());
        return queryWrapper;
    }

}
