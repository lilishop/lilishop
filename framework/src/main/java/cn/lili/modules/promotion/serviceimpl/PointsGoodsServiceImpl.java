package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.util.StrUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.trigger.util.DelayQueueTools;
import cn.lili.common.trigger.enums.DelayTypeEnums;
import cn.lili.common.trigger.message.PromotionMessage;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.trigger.interfaces.TimeTrigger;
import cn.lili.common.trigger.model.TimeExecuteConstant;
import cn.lili.common.trigger.model.TimeTriggerMsg;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.entity.dos.PointsGoods;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.entity.vos.PointsGoodsSearchParams;
import cn.lili.modules.promotion.entity.vos.PointsGoodsVO;
import cn.lili.modules.promotion.mapper.PointsGoodsMapper;
import cn.lili.modules.promotion.service.PointsGoodsService;
import cn.lili.modules.promotion.tools.PromotionTools;
import cn.lili.modules.search.service.EsGoodsIndexService;
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
import java.util.Collections;
import java.util.List;

/**
 * 积分商品业务层实现
 *
 * @author paulG
 * @date 2020/8/21
 **/
@Service
@Transactional(rollbackFor = Exception.class)
public class PointsGoodsServiceImpl extends ServiceImpl<PointsGoodsMapper, PointsGoods> implements PointsGoodsService {

    /**
     * 延时任务
     */
    @Autowired
    private TimeTrigger timeTrigger;
    /**
     * Mongo
     */
    @Autowired
    private MongoTemplate mongoTemplate;
    /**
     * RocketMQ
     */
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
    /**
     * 规格商品
     */
    @Autowired
    private GoodsSkuService goodsSkuService;
    /**
     * Es商品
     */
    @Autowired
    private EsGoodsIndexService goodsIndexService;

    /**
     * 批量添加积分商品
     *
     * @param pointsGoodsList 积分商品列表
     * @return 是否添加成功
     */
    @Override
    public boolean addPointsGoods(List<PointsGoodsVO> pointsGoodsList) {
        List<PointsGoods> pointsGoodsList1 = new ArrayList<>();
        for (PointsGoodsVO pointsGoods : pointsGoodsList) {
            GoodsSku goodsSku = this.checkSkuExist(pointsGoods.getSkuId());
            this.checkParam(pointsGoods, goodsSku);
            pointsGoods.setGoodsSku(goodsSku);
            PromotionTools.checkPromotionTime(pointsGoods.getStartTime().getTime(), pointsGoods.getEndTime().getTime());
            if (this.checkSkuDuplicate(pointsGoods.getSkuId(), null) == null) {
                pointsGoods.setPromotionStatus(PromotionStatusEnum.NEW.name());
                pointsGoods.setPromotionName("积分商品活动");
                pointsGoodsList1.add(pointsGoods);
            } else {
                throw new ServiceException("商品id为" + pointsGoods.getSkuId() + "的商品已参加积分商品活动！");
            }
        }
        this.saveBatch(pointsGoodsList1);
        for (PointsGoodsVO pointsGoods : pointsGoodsList) {
            this.mongoTemplate.save(pointsGoods);
            this.addPointsGoodsPromotionTask(pointsGoods);
        }
        return true;
    }

    /**
     * 更新一个积分商品
     *
     * @param pointsGoods 编辑的积分商品信息
     * @return 是否更新成功
     */
    @Override
    public boolean updatePointsGoods(PointsGoodsVO pointsGoods) {
        boolean result = false;
        PointsGoodsVO pointsGoodsVO = this.checkExist(pointsGoods.getId());
        GoodsSku goodsSku = this.checkSkuExist(pointsGoods.getSkuId());
        this.checkParam(pointsGoods, goodsSku);
        pointsGoods.setGoodsSku(goodsSku);
        if (this.checkSkuDuplicate(pointsGoods.getSkuId(), pointsGoods.getId()) == null) {
            if (PromotionStatusEnum.START.name().equals(pointsGoods.getPromotionStatus()) || PromotionStatusEnum.END.name().equals(pointsGoods.getPromotionStatus())) {
                throw new ServiceException(ResultCode.PROMOTION_UPDATE_ERROR);
            }
            PromotionTools.checkPromotionTime(pointsGoods.getStartTime().getTime(), pointsGoods.getEndTime().getTime());
            result = this.updateById(pointsGoods);
            this.mongoTemplate.save(pointsGoods);
            if (pointsGoods.getStartTime().getTime() != pointsGoodsVO.getStartTime().getTime()) {
                PromotionMessage promotionMessage = new PromotionMessage(pointsGoods.getId(), PromotionTypeEnum.POINTS_GOODS.name(), PromotionStatusEnum.START.name(), pointsGoods.getStartTime(), pointsGoods.getEndTime());
                //更新延时任务
                this.timeTrigger.edit(TimeExecuteConstant.PROMOTION_EXECUTOR,
                        promotionMessage,
                        pointsGoodsVO.getStartTime().getTime(),
                        pointsGoods.getStartTime().getTime(),
                        DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (promotionMessage.getPromotionType() + promotionMessage.getPromotionId())),
                        DateUtil.getDelayTime(pointsGoods.getStartTime().getTime()),
                        rocketmqCustomProperties.getPromotionTopic());
            }

        }
        return result;
    }

    /**
     * 批量更新积分商品状态
     *
     * @param ids             积分商品id集合
     * @param promotionStatus 更新的状态
     * @return 是否更新成功
     */
    @Override
    public boolean updatePointsGoodsPromotionStatus(List<String> ids, String promotionStatus) {
        for (String id : ids) {
            PointsGoodsVO pointsGoodsVO = this.checkExist(id);
            pointsGoodsVO.setPromotionStatus(PromotionStatusEnum.valueOf(promotionStatus).name());
            this.updateById(pointsGoodsVO);
            this.mongoTemplate.save(pointsGoodsVO);
            if (promotionStatus.equals(PromotionStatusEnum.START.name())) {
                this.addPointsGoodsPromotionTask(pointsGoodsVO);
            } else {
                this.goodsIndexService.deleteEsGoodsPromotionIndexByList(Collections.singletonList(pointsGoodsVO.getSkuId()), PromotionTypeEnum.POINTS_GOODS);
                this.timeTrigger.delete(TimeExecuteConstant.PROMOTION_EXECUTOR,
                        pointsGoodsVO.getStartTime().getTime(),
                        DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (PromotionTypeEnum.POINTS_GOODS.name() + pointsGoodsVO.getId())),
                        rocketmqCustomProperties.getPromotionTopic());
            }
        }
        return true;
    }

    /**
     * 批量删除积分商品
     *
     * @param ids 积分商品id集合
     * @return 是否删除成功
     */
    @Override
    public boolean deletePointsGoods(List<String> ids) {
        List<String> skuIds = new ArrayList<>();
        for (String id : ids) {
            PointsGoodsVO pointsGoodsVO = this.checkExist(id);
            this.timeTrigger.delete(TimeExecuteConstant.PROMOTION_EXECUTOR,
                    pointsGoodsVO.getStartTime().getTime(),
                    DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (PromotionTypeEnum.POINTS_GOODS.name() + pointsGoodsVO.getId())),
                    rocketmqCustomProperties.getPromotionTopic());
            skuIds.add(pointsGoodsVO.getSkuId());
        }
        boolean result = this.removeByIds(ids);
        this.goodsIndexService.deleteEsGoodsPromotionIndexByList(skuIds, PromotionTypeEnum.POINTS_GOODS);
        Query query = new Query();
        query.addCriteria(new Criteria("id").in(ids));
        this.mongoTemplate.remove(query, PointsGoodsVO.class);
        return result;
    }

    /**
     * 根据ID获取积分详情
     *
     * @param id 积分商品id
     * @return 积分详情
     */
    @Override
    public PointsGoodsVO getPointsGoodsDetail(String id) {
        return this.checkExist(id);
    }

    /**
     * 根据SkuID获取积分详情
     *
     * @param skuId@return 积分详情
     */
    @Override
    public PointsGoods getPointsGoodsDetailBySkuId(String skuId) {
        LambdaQueryWrapper<PointsGoods> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(PointsGoods::getSkuId, skuId).eq(PointsGoods::getPromotionStatus, PromotionStatusEnum.START.name());
        List<PointsGoods> list = this.list(queryWrapper);
        if (list.size() == 1) {
            return list.get(0);
        }
        return null;
    }

    /**
     * 根据条件查询积分商品
     *
     * @param searchParams 积分商品查询参数
     * @param page         分页参数
     * @return 积分商品查询结果
     */
    @Override
    public IPage<PointsGoodsVO> getPointsGoodsByPage(PointsGoodsSearchParams searchParams, PageVO page) {
        IPage<PointsGoodsVO> pointsGoodsPage = new Page<>();
        Query query = searchParams.mongoQuery();
        if (page != null) {
            PromotionTools.mongoQueryPageParam(query, page);
            pointsGoodsPage.setSize(page.getPageSize());
            pointsGoodsPage.setCurrent(page.getPageNumber());
        }
        List<PointsGoodsVO> pointsGoodsVOS = this.mongoTemplate.find(query, PointsGoodsVO.class);
        pointsGoodsPage.setRecords(pointsGoodsVOS);
        pointsGoodsPage.setTotal(this.mongoTemplate.count(searchParams.mongoQuery(), PointsGoodsVO.class));
        return pointsGoodsPage;
    }


    /**
     * 添加积分商品mq任务
     *
     * @param pointsGoods 积分商品信息
     */
    private void addPointsGoodsPromotionTask(PointsGoodsVO pointsGoods) {
        PromotionMessage promotionMessage = new PromotionMessage(pointsGoods.getId(), PromotionTypeEnum.POINTS_GOODS.name(),
                PromotionStatusEnum.START.name(),
                pointsGoods.getStartTime(), pointsGoods.getEndTime());
        TimeTriggerMsg timeTriggerMsg = new TimeTriggerMsg(TimeExecuteConstant.PROMOTION_EXECUTOR,
                promotionMessage.getStartTime().getTime(),
                promotionMessage,
                DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (promotionMessage.getPromotionType() + promotionMessage.getPromotionId())),
                rocketmqCustomProperties.getPromotionTopic());
        //发送促销活动开始的延时任务
        this.timeTrigger.addDelay(timeTriggerMsg);
    }

    /**
     * 检查mongo中积分商品存在
     *
     * @param id 积分商品id
     * @return 积分商品信息
     */
    private PointsGoodsVO checkExist(String id) {
        PointsGoodsVO pointsGoodsVO = this.mongoTemplate.findById(id, PointsGoodsVO.class);
        if (pointsGoodsVO == null) {
            log.error("id为" + id + "的积分商品不存在！");
            throw new ServiceException();
        }
        return pointsGoodsVO;
    }

    /**
     * 检查积分商品是否重复存在
     *
     * @param skuId 商品SkuId
     * @param id    积分商品I（可选）
     * @return 积分商品信息
     */
    private PointsGoods checkSkuDuplicate(String skuId, String id) {
        LambdaQueryWrapper<PointsGoods> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(PointsGoods::getSkuId, skuId);
        if (StrUtil.isNotEmpty(id)) {
            queryWrapper.ne(PointsGoods::getId, id);
        }
        queryWrapper.ne(PointsGoods::getPromotionStatus, PromotionStatusEnum.END.name());
        return this.getOne(queryWrapper);
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
     * 检查参与积分商品参数
     *
     * @param pointsGoods 积分商品信息
     * @param goodsSku    商品sku信息
     */
    private void checkParam(PointsGoods pointsGoods, GoodsSku goodsSku) {
        if (pointsGoods.getActiveStock() > goodsSku.getQuantity()) {
            throw new ServiceException(ResultCode.POINT_GOODS_ACTIVE_STOCK_ERROR);
        }
    }
}
