package cn.lili.modules.promotion.serviceimpl;


import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.trigger.enums.DelayTypeEnums;
import cn.lili.common.trigger.interfaces.TimeTrigger;
import cn.lili.common.trigger.message.PromotionMessage;
import cn.lili.common.trigger.model.TimeExecuteConstant;
import cn.lili.common.trigger.model.TimeTriggerMsg;
import cn.lili.common.trigger.util.DelayQueueTools;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.entity.dos.KanJiaActivityGoods;
import cn.lili.modules.promotion.entity.dto.KanJiaActivityGoodsDTO;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.entity.vos.KanJiaActivityGoodsParams;
import cn.lili.modules.promotion.entity.vos.PointsGoodsVO;
import cn.lili.modules.promotion.mapper.KanJiaActivityGoodsMapper;
import cn.lili.modules.promotion.service.KanJiaActivityGoodsService;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

/**
 * 砍价业务层实现
 *
 * @author qiuqiu
 * @date 2021/7/1
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class KanJiaActivityGoodsServiceImpl extends ServiceImpl<KanJiaActivityGoodsMapper, KanJiaActivityGoods> implements KanJiaActivityGoodsService {

    //规格商品
    @Autowired
    private GoodsSkuService goodsSkuService;

    //Rocketmq
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    //延时任务
    @Autowired
    private TimeTrigger timeTrigger;


    @Override
    public boolean add(List<KanJiaActivityGoodsDTO> kanJiaActivityGoodsDTOS) {
        List<KanJiaActivityGoods> kanJiaActivityGoodsList = new ArrayList<>();
        for (KanJiaActivityGoodsDTO kanJiaActivityGoodsDTO : kanJiaActivityGoodsDTOS) {
            //根据skuId查询商品信息
            GoodsSku goodsSku = this.checkSkuExist(kanJiaActivityGoodsDTO.getSkuId());
            //校验商品是否存在
            if (goodsSku == null) {
                throw new ServiceException(ResultCode.PROMOTION_GOODS_NOT_EXIT);
            }
            //校验商品状态
            if (goodsSku.getMarketEnable().equals(GoodsStatusEnum.DOWN.name())) {
                throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
            }
            //校验活动库存是否超出此sku的库存
            if (goodsSku.getQuantity() < kanJiaActivityGoodsDTO.getStock()) {
                throw new ServiceException(ResultCode.POINT_GOODS_ACTIVE_STOCK_ERROR);
            }
            //校验最低购买金额不能高于商品金额
            if (goodsSku.getPrice() < kanJiaActivityGoodsDTO.getPurchasePrice()) {
                throw new ServiceException(ResultCode.KANJIIA_GOODS_ACTIVE_PRICE_ERROR);
            }
            //检测同一时间段是否存在相同的商品
            PromotionTools.checkPromotionTime(kanJiaActivityGoodsDTO.getStartTime().getTime(), kanJiaActivityGoodsDTO.getEndTime().getTime());
            //赋值保存
            KanJiaActivityGoods kanJiaActivityGoods = new KanJiaActivityGoods();
            kanJiaActivityGoods.setSkuId(kanJiaActivityGoodsDTO.getSkuId());
            kanJiaActivityGoods.setThumbnail(goodsSku.getThumbnail());
            kanJiaActivityGoods.setPurchasePrice(kanJiaActivityGoodsDTO.getPurchasePrice());
            kanJiaActivityGoods.setSettlementPrice(kanJiaActivityGoodsDTO.getSettlementPrice());
            kanJiaActivityGoods.setGoodsName(goodsSku.getGoodsName());
            kanJiaActivityGoods.setStock(kanJiaActivityGoodsDTO.getStock());
            kanJiaActivityGoods.setHighestPrice(kanJiaActivityGoodsDTO.getHighestPrice());
            kanJiaActivityGoods.setLowestPrice(kanJiaActivityGoodsDTO.getLowestPrice());
            kanJiaActivityGoodsList.add(kanJiaActivityGoods);
        }
        Boolean result = this.saveBatch(kanJiaActivityGoodsList);
        if (result) {
            //发送砍价延迟任务消息
            for (KanJiaActivityGoods kanJiaActivityGoods : kanJiaActivityGoodsList) {
                this.addKanJiaGoodsPromotionTask(kanJiaActivityGoods);
            }
        }
        return result;
    }


    /**
     * 添加砍价商品mq任务
     *
     * @param kanJiaActivityGoods 砍价商品信息
     */
    private void addKanJiaGoodsPromotionTask(KanJiaActivityGoods kanJiaActivityGoods) {
        PromotionMessage promotionMessage = new PromotionMessage(kanJiaActivityGoods.getId(), PromotionTypeEnum.KAN_JIA.name(),
                PromotionStatusEnum.START.name(),
                kanJiaActivityGoods.getStartTime(), kanJiaActivityGoods.getEndTime());
        TimeTriggerMsg timeTriggerMsg = new TimeTriggerMsg(TimeExecuteConstant.PROMOTION_EXECUTOR,
                promotionMessage.getStartTime().getTime(),
                promotionMessage,
                DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (promotionMessage.getPromotionType() + promotionMessage.getPromotionId())),
                rocketmqCustomProperties.getPromotionTopic());
        //发送促销活动开始的延时任务
        this.timeTrigger.addDelay(timeTriggerMsg);
    }

    @Override
    public IPage<KanJiaActivityGoods> getForPage(KanJiaActivityGoodsParams kanJiaActivityGoodsParams, PageVO pageVO) {
        QueryWrapper<KanJiaActivityGoods> queryWrapper = kanJiaActivityGoodsParams.wrapper();
        return page(PageUtil.initPage(pageVO), queryWrapper);
    }


    /**
     * 检查商品Sku是否存
     *
     * @param skuId skuId
     * @return 商品sku
     */
    private GoodsSku checkSkuExist(String skuId) {
        GoodsSku goodsSku = this.goodsSkuService.getGoodsSkuByIdFromCache(skuId);
        if (goodsSku == null) {
            log.error("商品ID为" + skuId + "的商品不存在！");
            throw new ServiceException();
        }
        return goodsSku;
    }
}