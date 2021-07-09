package cn.lili.modules.member.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.common.rocketmq.tags.GoodsTagsEnum;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.member.entity.dos.GoodsCollection;
import cn.lili.modules.member.entity.vo.GoodsCollectionVO;
import cn.lili.modules.member.mapper.GoodsCollectionMapper;
import cn.lili.modules.member.service.GoodsCollectionService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

/**
 * 会员收藏业务层实现
 *
 * @author Chopper
 * @date 2020/11/18 2:25 下午
 */
@Service
@Transactional(rollbackFor = Exception.class)

public class GoodsCollectionServiceImpl extends ServiceImpl<GoodsCollectionMapper, GoodsCollection> implements GoodsCollectionService {

    /**
     * rocketMq
     */
    @Autowired
    private RocketMQTemplate rocketMQTemplate;
    /**
     * rocketMq配置
     */
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Override
    public IPage<GoodsCollectionVO> goodsCollection(PageVO pageVo) {
        QueryWrapper<GoodsCollectionVO> queryWrapper = new QueryWrapper();
        queryWrapper.eq("gc.member_id", UserContext.getCurrentUser().getId());
        queryWrapper.groupBy("gc.id");
        queryWrapper.orderByDesc("gc.create_time");
        return this.baseMapper.goodsCollectionVOList(PageUtil.initPage(pageVo), queryWrapper);
    }

    @Override
    public boolean isCollection(String skuId) {
        QueryWrapper<GoodsCollection> queryWrapper = new QueryWrapper();
        queryWrapper.eq("member_id", UserContext.getCurrentUser().getId());
        queryWrapper.eq(skuId != null, "sku_id", skuId);
        return Optional.ofNullable(this.getOne(queryWrapper)).isPresent();
    }

    @Override
    public GoodsCollection addGoodsCollection(String skuId) {
        GoodsCollection goodsCollection = this.getOne(new LambdaUpdateWrapper<GoodsCollection>()
                .eq(GoodsCollection::getMemberId, UserContext.getCurrentUser().getId())
                .eq(GoodsCollection::getSkuId, skuId));
        if (goodsCollection == null) {
            goodsCollection = new GoodsCollection(UserContext.getCurrentUser().getId(), skuId);

            this.save(goodsCollection);
            //商品收藏消息
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.GOODS_COLLECTION.name();
            //发送mq消息
            rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(skuId), RocketmqSendCallbackBuilder.commonCallback());
            return goodsCollection;
        }
        throw new ServiceException(ResultCode.USER_COLLECTION_EXIST);
    }

    @Override
    public boolean deleteGoodsCollection(String skuId) {
        QueryWrapper<GoodsCollection> queryWrapper = new QueryWrapper();
        queryWrapper.eq("member_id", UserContext.getCurrentUser().getId());
        queryWrapper.eq(skuId != null, "sku_id", skuId);
        return this.remove(queryWrapper);
    }

    @Override
    public boolean deleteGoodsCollection(List<String> goodsIds) {
        QueryWrapper<GoodsCollection> queryWrapper = new QueryWrapper();
        queryWrapper.in("sku_id", goodsIds);
        return this.remove(queryWrapper);
    }

    @Override
    public boolean deleteSkuCollection(List<String> skuIds) {
        QueryWrapper<GoodsCollection> queryWrapper = new QueryWrapper();
        queryWrapper.in("sku_id", skuIds);
        return this.remove(queryWrapper);
    }
}