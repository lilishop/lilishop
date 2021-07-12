package cn.lili.modules.promotion.serviceimpl;


import cn.hutool.core.util.StrUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.trigger.enums.DelayTypeEnums;
import cn.lili.common.trigger.interfaces.TimeTrigger;
import cn.lili.common.trigger.message.PromotionMessage;
import cn.lili.common.trigger.model.TimeExecuteConstant;
import cn.lili.common.trigger.model.TimeTriggerMsg;
import cn.lili.common.trigger.util.DelayQueueTools;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.entity.dos.KanJiaActivityGoods;
import cn.lili.modules.promotion.entity.dto.KanJiaActivityGoodsDTO;
import cn.lili.modules.promotion.entity.dto.KanJiaActivityGoodsOperationDTO;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.entity.vos.KanJiaActivityGoodsParams;
import cn.lili.modules.promotion.mapper.KanJiaActivityGoodsMapper;
import cn.lili.modules.promotion.service.KanJiaActivityGoodsService;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
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

    //Mongo
    @Autowired
    private MongoTemplate mongoTemplate;


    @Override
    public boolean add(KanJiaActivityGoodsOperationDTO kanJiaActivityGoodsOperationDTO) {
        List<KanJiaActivityGoods> kanJiaActivityGoodsList = new ArrayList<>();
        for (KanJiaActivityGoodsDTO kanJiaActivityGoodsDTO : kanJiaActivityGoodsOperationDTO.getPromotionGoodsList()) {
            //根据skuId查询商品信息
            GoodsSku goodsSku = this.checkSkuExist(kanJiaActivityGoodsDTO.getSkuId());
            //参数检测
            this.checkParam(kanJiaActivityGoodsDTO, goodsSku);
            //检测同一时间段是否存在相同的商品
            PromotionTools.checkPromotionTime(kanJiaActivityGoodsOperationDTO.getStartTime().getTime(), kanJiaActivityGoodsOperationDTO.getEndTime().getTime());
            kanJiaActivityGoodsDTO.setStartTime(kanJiaActivityGoodsOperationDTO.getStartTime());
            kanJiaActivityGoodsDTO.setEndTime(kanJiaActivityGoodsOperationDTO.getEndTime());
            //检测同一时间段不能允许添加相同的商品
            if (this.checkSkuDuplicate(goodsSku.getId(), kanJiaActivityGoodsDTO) != null) {
                throw new ServiceException("商品id为" + goodsSku.getId() + "的商品已参加砍价商品活动！");
            }
            kanJiaActivityGoodsDTO.setGoodsSku(goodsSku);
            kanJiaActivityGoodsDTO.setSkuId(kanJiaActivityGoodsDTO.getSkuId());
            kanJiaActivityGoodsDTO.setThumbnail(goodsSku.getThumbnail());
            kanJiaActivityGoodsDTO.setGoodsName(goodsSku.getGoodsName());
            kanJiaActivityGoodsDTO.setPromotionStatus(PromotionStatusEnum.NEW.name());
            kanJiaActivityGoodsList.add(kanJiaActivityGoodsDTO);
        }
        Boolean result = this.saveBatch(kanJiaActivityGoodsList);
        if (result) {
            //发送砍价延迟任务消息
            for (KanJiaActivityGoodsDTO kanJiaActivityGoodsDTO : kanJiaActivityGoodsOperationDTO.getPromotionGoodsList()) {
                this.mongoTemplate.save(kanJiaActivityGoodsDTO);
                this.addKanJiaGoodsPromotionTask(kanJiaActivityGoodsDTO);
            }
        }
        return result;
    }


    /**
     * 添加砍价商品mq任务
     *
     * @param kanJiaActivityGoods 砍价商品信息
     */
    private void addKanJiaGoodsPromotionTask(KanJiaActivityGoodsDTO kanJiaActivityGoods) {
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
    public IPage<KanJiaActivityGoodsDTO> getForPage(KanJiaActivityGoodsParams kanJiaActivityGoodsParams, PageVO pageVO) {
        IPage<KanJiaActivityGoodsDTO> kanJiaActivityGoodsDTOIPage = new Page<>();
        Query query = kanJiaActivityGoodsParams.mongoQuery();
        if (pageVO != null) {
            PromotionTools.mongoQueryPageParam(query, pageVO);
            kanJiaActivityGoodsDTOIPage.setSize(pageVO.getPageSize());
            kanJiaActivityGoodsDTOIPage.setCurrent(pageVO.getPageNumber());
        }
        List<KanJiaActivityGoodsDTO> kanJiaActivityGoodsDTOS = this.mongoTemplate.find(query, KanJiaActivityGoodsDTO.class);
        kanJiaActivityGoodsDTOIPage.setRecords(kanJiaActivityGoodsDTOS);
        kanJiaActivityGoodsDTOIPage.setTotal(this.mongoTemplate.count(kanJiaActivityGoodsParams.mongoQuery(), KanJiaActivityGoodsDTO.class));
        return kanJiaActivityGoodsDTOIPage;

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

    /**
     * 检查参与砍价商品参数
     *
     * @param kanJiaActivityGoodsDTO 砍价商品信息
     * @param goodsSku               商品sku信息
     */
    private void checkParam(KanJiaActivityGoodsDTO kanJiaActivityGoodsDTO, GoodsSku goodsSku) {
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
            throw new ServiceException(ResultCode.KANJIA_GOODS_ACTIVE_PRICE_ERROR);
        }
        //校验结算价格不能超过商品金额
        if (goodsSku.getPrice() < kanJiaActivityGoodsDTO.getSettlementPrice()) {
            throw new ServiceException(ResultCode.KANJIA_GOODS_ACTIVE_SETTLEMENT_PRICE_ERROR);
        }
        //校验最高砍价金额
        if (kanJiaActivityGoodsDTO.getHighestPrice() > goodsSku.getPrice() || kanJiaActivityGoodsDTO.getHighestPrice() <= 0) {
            throw new ServiceException(ResultCode.KANJIA_GOODS_ACTIVE_HIGHEST_PRICE_ERROR);
        }
        //校验最低砍价金额
        if (kanJiaActivityGoodsDTO.getLowestPrice() > goodsSku.getPrice() || kanJiaActivityGoodsDTO.getLowestPrice() <= 0) {
            throw new ServiceException(ResultCode.KANJIA_GOODS_ACTIVE_LOWEST_PRICE_ERROR);
        }
        //校验最低砍价金额不能高与最低砍价金额
        if (kanJiaActivityGoodsDTO.getLowestPrice() > kanJiaActivityGoodsDTO.getHighestPrice()) {
            throw new ServiceException(ResultCode.KANJIA_GOODS_ACTIVE_LOWEST_PRICE_ERROR);
        }
    }

    /**
     * 检查砍价商品是否重复存在
     *
     * @param skuId                  商品SkuId
     * @param kanJiaActivityGoodsDTO 砍价商品
     * @return 积分商品信息
     */
    private KanJiaActivityGoods checkSkuDuplicate(String skuId, KanJiaActivityGoodsDTO kanJiaActivityGoodsDTO) {
        LambdaQueryWrapper<KanJiaActivityGoods> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(KanJiaActivityGoods::getSkuId, skuId);
        if (kanJiaActivityGoodsDTO != null && StrUtil.isNotEmpty(kanJiaActivityGoodsDTO.getId())) {
            queryWrapper.ne(KanJiaActivityGoods::getId, kanJiaActivityGoodsDTO.getId());
        }
        queryWrapper.ne(KanJiaActivityGoods::getPromotionStatus, PromotionStatusEnum.END.name());

        queryWrapper.ge(KanJiaActivityGoods::getStartTime, kanJiaActivityGoodsDTO.getStartTime());

        queryWrapper.le(KanJiaActivityGoods::getEndTime, kanJiaActivityGoodsDTO.getEndTime());

        return this.getOne(queryWrapper);

    }

    @Override
    public KanJiaActivityGoodsDTO getKanJiaGoodsDetail(String goodsId) {
        KanJiaActivityGoodsDTO kanJiaActivityGoodsDTO = this.mongoTemplate.findById(goodsId, KanJiaActivityGoodsDTO.class);
        if (kanJiaActivityGoodsDTO == null) {
            log.error("id为" + goodsId + "的砍价商品不存在！");
            throw new ServiceException();
        }
        return kanJiaActivityGoodsDTO;
    }

    @Override
    public boolean updateKanJiaActivityGoods(KanJiaActivityGoodsDTO kanJiaActivityGoodsDTO) {
        //校验砍价商品是否存在
        KanJiaActivityGoodsDTO dbKanJiaActivityGoods = this.getKanJiaGoodsDetail(kanJiaActivityGoodsDTO.getId());
        //校验当前活动是否已经开始,只有新建的未开始的活动可以编辑
        if (!dbKanJiaActivityGoods.getPromotionStatus().equals(PromotionStatusEnum.NEW.name())) {
            throw new ServiceException(ResultCode.PROMOTION_UPDATE_ERROR);
        }
        //获取当前sku信息
        GoodsSku goodsSku = this.checkSkuExist(kanJiaActivityGoodsDTO.getSkuId());
        //校验商品状态
        if (goodsSku.getMarketEnable().equals(GoodsStatusEnum.DOWN.name())) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        //常规校验砍价商品参数
        this.checkParam(kanJiaActivityGoodsDTO, goodsSku);
        //检测开始结束时间是否正确
        PromotionTools.checkPromotionTime(kanJiaActivityGoodsDTO.getStartTime().getTime(), kanJiaActivityGoodsDTO.getEndTime().getTime());
        //检测同一时间段不能允许添加相同的商品
        if (this.checkSkuDuplicate(goodsSku.getId(), kanJiaActivityGoodsDTO) != null) {
            throw new ServiceException("商品id为" + goodsSku.getId() + "的商品已参加砍价商品活动！");
        }
        //修改数据库
        boolean result = this.updateById(kanJiaActivityGoodsDTO);
        //如果校验成功则发送修改延迟任务消息
        if (result) {
            this.mongoTemplate.save(kanJiaActivityGoodsDTO);
            if (dbKanJiaActivityGoods.getStartTime().getTime() != kanJiaActivityGoodsDTO.getStartTime().getTime()) {
                PromotionMessage promotionMessage = new PromotionMessage(kanJiaActivityGoodsDTO.getId(), PromotionTypeEnum.KAN_JIA.name(), PromotionStatusEnum.START.name(), kanJiaActivityGoodsDTO.getStartTime(), kanJiaActivityGoodsDTO.getEndTime());
                //更新延时任务
                this.timeTrigger.edit(TimeExecuteConstant.PROMOTION_EXECUTOR,
                        promotionMessage,
                        kanJiaActivityGoodsDTO.getStartTime().getTime(),
                        kanJiaActivityGoodsDTO.getStartTime().getTime(),
                        DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (promotionMessage.getPromotionType() + promotionMessage.getPromotionId())),
                        DateUtil.getDelayTime(kanJiaActivityGoodsDTO.getStartTime().getTime()),
                        rocketmqCustomProperties.getPromotionTopic());
            }
        }
        return result;
    }

    @Override
    public boolean deleteKanJiaGoods(List<String> ids) {
        List<String> skuIds = new ArrayList<>();
        for (String id : ids) {
            KanJiaActivityGoodsDTO kanJiaActivityGoodsDTO = this.getKanJiaGoodsDetail(id);
            this.timeTrigger.delete(TimeExecuteConstant.PROMOTION_EXECUTOR,
                    kanJiaActivityGoodsDTO.getStartTime().getTime(),
                    DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (PromotionTypeEnum.KAN_JIA.name() + kanJiaActivityGoodsDTO.getId())),
                    rocketmqCustomProperties.getPromotionTopic());
            skuIds.add(kanJiaActivityGoodsDTO.getSkuId());
        }
        boolean result = this.removeByIds(ids);
        if (result) {
            Query query = new Query();
            query.addCriteria(new Criteria("id").in(ids));
            this.mongoTemplate.remove(query, KanJiaActivityGoodsDTO.class);
        }
        return result;
    }


    @Override
    public KanJiaActivityGoodsDTO getKanJiaGoodsBySku(String skuId) {
        //构建查询条件
        KanJiaActivityGoodsParams kanJiaActivityGoodsParams = new KanJiaActivityGoodsParams();
        kanJiaActivityGoodsParams.setSkuId(skuId);
        kanJiaActivityGoodsParams.setPromotionStatus(PromotionStatusEnum.START.name());
        //查询相关数据
        Query query = kanJiaActivityGoodsParams.mongoQuery();
        List<KanJiaActivityGoodsDTO> kanJiaActivityGoodsDTOS = this.mongoTemplate.find(query, KanJiaActivityGoodsDTO.class);
        // 为了担心会查到多条数据 所以查询集合  正常情况下只会查询到一条
        if (kanJiaActivityGoodsDTOS.size() > 0) {
            return kanJiaActivityGoodsDTOS.get(0);
        }
        return null;
    }
}