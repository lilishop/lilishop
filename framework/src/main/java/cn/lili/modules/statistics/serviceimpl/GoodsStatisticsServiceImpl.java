package cn.lili.modules.statistics.serviceimpl;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.statistics.mapper.GoodsStatisticsMapper;
import cn.lili.modules.statistics.service.GoodsStatisticsService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.Objects;

/**
 * 商品统计业务层实现
 *
 * @author Bulbasaur
 * @since 2020/12/9 11:30
 */
@Service
public class GoodsStatisticsServiceImpl extends ServiceImpl<GoodsStatisticsMapper, Goods> implements GoodsStatisticsService {

    @Override
    public long goodsNum(GoodsStatusEnum goodsStatusEnum, GoodsAuthEnum goodsAuthEnum) {
        LambdaQueryWrapper<Goods> queryWrapper = Wrappers.lambdaQuery();

        queryWrapper.eq(Goods::getDeleteFlag, false);

        if (goodsStatusEnum != null) {
            queryWrapper.eq(Goods::getMarketEnable, goodsStatusEnum.name());
        }
        if (goodsAuthEnum != null) {
            queryWrapper.eq(Goods::getAuthFlag, goodsAuthEnum.name());
        }
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        queryWrapper.eq(CharSequenceUtil.equals(currentUser.getRole().name(), UserEnums.STORE.name()),
                Goods::getStoreId, currentUser.getStoreId());

        return this.count(queryWrapper);
    }

    @Override
    public long todayUpperNum() {
        LambdaQueryWrapper<Goods> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(Goods::getMarketEnable, GoodsStatusEnum.UPPER.name());
        queryWrapper.ge(Goods::getCreateTime, DateUtil.beginOfDay(new DateTime()));
        return this.count(queryWrapper);
    }
}
