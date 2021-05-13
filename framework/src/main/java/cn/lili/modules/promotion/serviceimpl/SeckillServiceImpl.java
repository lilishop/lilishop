package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.delayqueue.DelayQueueTools;
import cn.lili.common.delayqueue.DelayQueueType;
import cn.lili.common.delayqueue.PromotionMessage;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.trigger.interfaces.TimeTrigger;
import cn.lili.common.trigger.model.TimeExecuteConstant;
import cn.lili.common.trigger.model.TimeTriggerMsg;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.dos.SeckillApply;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.entity.vos.SeckillSearchParams;
import cn.lili.modules.promotion.entity.vos.SeckillVO;
import cn.lili.modules.promotion.mapper.SeckillMapper;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.service.SeckillApplyService;
import cn.lili.modules.promotion.service.SeckillService;
import cn.lili.modules.promotion.tools.PromotionTools;
import cn.lili.modules.search.service.EsGoodsIndexService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

/**
 * 限时抢购业务层实现
 *
 * @author Chopper
 * @date 2020/8/21
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class SeckillServiceImpl extends ServiceImpl<SeckillMapper, Seckill> implements SeckillService {

    //延时任务
    @Autowired
    private TimeTrigger timeTrigger;
    //Mongo
    @Autowired
    private MongoTemplate mongoTemplate;
    //Rocketmq
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
    //商品索引
    @Autowired
    private EsGoodsIndexService goodsIndexService;
    //促销商品
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    //秒杀申请
    @Autowired
    private SeckillApplyService seckillApplyService;

    @Override
    public IPage<Seckill> getSeckillByPageFromMysql(SeckillSearchParams queryParam, PageVO pageVo) {
        QueryWrapper<Seckill> queryWrapper = queryParam.wrapper();
        return page(PageUtil.initPage(pageVo), queryWrapper);
    }

    /**
     * 从mongo中根据条件获取限时抢购分页列表
     *
     * @param queryParam 查询参数
     * @param pageVo     分页参数
     * @return 限时抢购分页列表
     */
    @Override
    public IPage<SeckillVO> getSeckillByPageFromMongo(SeckillSearchParams queryParam, PageVO pageVo) {
        IPage<SeckillVO> seckill = new Page<>(pageVo.getPageNumber(), pageVo.getPageSize());
        if (queryParam == null) {
            queryParam = new SeckillSearchParams();
        }
        Query query = queryParam.mongoQuery();
        pageVo.setNotConvert(true);
        PromotionTools.mongoQueryPageParam(query, pageVo);
        seckill.setCurrent(pageVo.getPageNumber());
        seckill.setSize(pageVo.getPageSize());
        List<SeckillVO> seckillVOS = this.mongoTemplate.find(query, SeckillVO.class);
        seckill.setRecords(seckillVOS);
        seckill.setTotal(this.mongoTemplate.count(queryParam.mongoQuery(), SeckillVO.class));
        return seckill;
    }

    /**
     * 从mongo中获取限时抢购信息
     *
     * @param id 限时抢购id
     * @return 限时抢购信息
     */
    @Override
    public SeckillVO getSeckillByIdFromMongo(String id) {
        return this.checkSeckillExist(id);
    }

    @Override
    public boolean saveSeckill(SeckillVO seckill) {
        // 检查限时抢购参数
        checkSeckillParam(seckill, seckill.getStoreId());
        seckill.setPromotionStatus(PromotionStatusEnum.NEW.name());
        // 保存到MYSQL中
        boolean result = this.save(seckill);
        // 保存到MONGO中
        this.mongoTemplate.save(seckill);
        this.addSeckillStartTask(seckill);
        return result;
    }

    @Override
    public void storeApply(String storeId, String seckillId) {
        Seckill seckill = this.getById(seckillId);
        String storeIds;
        if (!StringUtils.isEmpty(seckill.getStoreIds())) {
            storeIds = seckill.getStoreIds() + storeId + ",";
        } else {
            storeIds = storeId + ",";
        }
        seckill.setStoreIds(storeIds);
        this.updateById(seckill);
    }

    @Override
    public boolean modifySeckill(SeckillVO seckillVO) {
        // 检查该限时抢购是否存在
        SeckillVO seckill = checkSeckillExist(seckillVO.getId());
        if (PromotionStatusEnum.START.name().equals(seckillVO.getPromotionStatus())) {
            throw new ServiceException("活动已经开始，不能进行编辑删除操作");
        }
        // 检查限时抢购参数
        this.checkSeckillParam(seckillVO, seckillVO.getStoreId());

        // 更新到MYSQL中
        boolean result = this.updateById(seckillVO);
        // 保存到MONGO中
        this.mongoTemplate.save(seckillVO);
        if (seckill.getStartTime().getTime() != seckillVO.getStartTime().getTime()) {
            PromotionMessage promotionMessage = new PromotionMessage(seckillVO.getId(), PromotionTypeEnum.SECKILL.name(), PromotionStatusEnum.START.name(), seckillVO.getStartTime(), seckillVO.getEndTime());
            // 更新延时任务
            this.timeTrigger.edit(TimeExecuteConstant.PROMOTION_EXECUTOR,
                    promotionMessage,
                    seckill.getStartTime().getTime(),
                    seckillVO.getStartTime().getTime(),
                    DelayQueueTools.wrapperUniqueKey(DelayQueueType.PROMOTION, (promotionMessage.getPromotionType() + promotionMessage.getPromotionId())),
                    DateUtil.getDelayTime(seckillVO.getStartTime().getTime()),
                    rocketmqCustomProperties.getPromotionTopic());
        }
        return result;
    }

    @Override
    public void deleteSeckill(String id) {
        Seckill seckill = checkSeckillExist(id);
        if (PromotionStatusEnum.CLOSE.name().equals(seckill.getPromotionStatus()) || PromotionStatusEnum.END.name().equals(seckill.getPromotionStatus())) {
            // 更新限时抢购状态为关闭，标示删除标志
            LambdaUpdateWrapper<Seckill> updateWrapper = new LambdaUpdateWrapper<Seckill>().eq(Seckill::getId, id).set(Seckill::getDeleteFlag, true).set(Seckill::getPromotionStatus, PromotionStatusEnum.CLOSE.name());
            this.update(updateWrapper);
            LambdaUpdateWrapper<SeckillApply> seckillApplyLambdaUpdateWrapper = new LambdaUpdateWrapper<SeckillApply>().eq(SeckillApply::getSeckillId, id).set(SeckillApply::getDeleteFlag, true);
            this.seckillApplyService.update(seckillApplyLambdaUpdateWrapper);
            this.mongoTemplate.remove(new Query().addCriteria(Criteria.where("id").is(id)), SeckillVO.class);
            LambdaUpdateWrapper<PromotionGoods> promotionGoodsQueryWrapper = new LambdaUpdateWrapper<PromotionGoods>().eq(PromotionGoods::getPromotionId, id).set(PromotionGoods::getDeleteFlag, true);
            this.promotionGoodsService.update(promotionGoodsQueryWrapper);
            this.timeTrigger.delete(TimeExecuteConstant.PROMOTION_EXECUTOR,
                    seckill.getStartTime().getTime(),
                    DelayQueueTools.wrapperUniqueKey(DelayQueueType.PROMOTION, (PromotionTypeEnum.SECKILL.name() + seckill.getId())),
                    rocketmqCustomProperties.getPromotionTopic());
        } else {
            throw new ServiceException("该限时抢购活动的状态不能删除");
        }
    }

    /**
     * 开启一个限时抢购
     *
     * @param id 限时抢购编号
     */
    @Override
    public void openSeckill(String id) {
        SeckillVO seckillVO = checkSeckillExist(id);
        PromotionTools.checkPromotionTime(seckillVO.getStartTime().getTime(), seckillVO.getEndTime().getTime());
        if (PromotionStatusEnum.NEW.name().equals(seckillVO.getPromotionStatus()) || PromotionStatusEnum.CLOSE.name().equals(seckillVO.getPromotionStatus())) {
            LambdaUpdateWrapper<Seckill> updateWrapper = new LambdaUpdateWrapper<Seckill>().eq(Seckill::getId, id).set(Seckill::getPromotionStatus, PromotionStatusEnum.START.name());
            this.update(updateWrapper);
            seckillVO.setPromotionStatus(PromotionStatusEnum.START.name());
            this.mongoTemplate.save(seckillVO);
            this.addSeckillStartTask(seckillVO);
        }
    }

    @Override
    public void closeSeckill(String id) {
        SeckillVO seckillVO = checkSeckillExist(id);
        if (PromotionStatusEnum.NEW.name().equals(seckillVO.getPromotionStatus()) || PromotionStatusEnum.START.name().equals(seckillVO.getPromotionStatus())) {
            LambdaUpdateWrapper<Seckill> updateWrapper = new LambdaUpdateWrapper<Seckill>().eq(Seckill::getId, id).set(Seckill::getPromotionStatus, PromotionStatusEnum.CLOSE.name());
            this.update(updateWrapper);
            seckillVO.setPromotionStatus(PromotionStatusEnum.CLOSE.name());
            this.mongoTemplate.save(seckillVO);
            if (PromotionStatusEnum.CLOSE.name().equals(seckillVO.getPromotionStatus())) {
                LambdaQueryWrapper<PromotionGoods> deleteWrapper = new LambdaQueryWrapper<>();
                deleteWrapper.eq(PromotionGoods::getPromotionId, seckillVO.getId());
                promotionGoodsService.remove(deleteWrapper);
                if (seckillVO.getSeckillApplyList() != null) {
                    List<String> skuIds = seckillVO.getSeckillApplyList().stream().map(SeckillApply::getSkuId).collect(Collectors.toList());
                    this.goodsIndexService.deleteEsGoodsPromotionIndexByList(skuIds, PromotionTypeEnum.SECKILL);
                }
                this.timeTrigger.delete(TimeExecuteConstant.PROMOTION_EXECUTOR,
                        seckillVO.getStartTime().getTime(),
                        DelayQueueTools.wrapperUniqueKey(DelayQueueType.PROMOTION, (PromotionTypeEnum.SECKILL.name() + seckillVO.getId())),
                        rocketmqCustomProperties.getPromotionTopic());
            }
        } else {
            throw new ServiceException("该限时抢购活动的状态不能关闭");
        }
    }

    @Override
    public Integer getApplyNum() {
        LambdaQueryWrapper<Seckill> queryWrapper = Wrappers.lambdaQuery();
        //秒杀申请时间未超过当前时间
        queryWrapper.le(Seckill::getApplyEndTime, cn.hutool.core.date.DateUtil.date());
        queryWrapper.eq(Seckill::getPromotionStatus, PromotionStatusEnum.NEW.name());
        return this.count(queryWrapper);
    }

    private void addSeckillStartTask(SeckillVO seckill) {
        PromotionMessage promotionMessage = new PromotionMessage(seckill.getId(), PromotionTypeEnum.SECKILL.name(), PromotionStatusEnum.START.name(), seckill.getStartTime(), seckill.getEndTime());
        TimeTriggerMsg timeTriggerMsg = new TimeTriggerMsg(TimeExecuteConstant.PROMOTION_EXECUTOR,
                seckill.getStartTime().getTime(),
                promotionMessage,
                DelayQueueTools.wrapperUniqueKey(DelayQueueType.PROMOTION, (promotionMessage.getPromotionType() + promotionMessage.getPromotionId())),
                rocketmqCustomProperties.getPromotionTopic());
        // 发送促销活动开始的延时任务
        this.timeTrigger.addDelay(timeTriggerMsg, DateUtil.getDelayTime(seckill.getStartTime().getTime()));
    }

    /**
     * 检查该限时抢购是否存在
     *
     * @param id 限时抢购编号
     * @return 限时抢购信息
     */
    private SeckillVO checkSeckillExist(String id) {
        SeckillVO seckill = this.mongoTemplate.findById(id, SeckillVO.class);
        if (seckill == null) {
            throw new ServiceException("当前限时抢购活动不存在");
        }
        return seckill;
    }

    /**
     * 检查限时抢购参数
     *
     * @param seckill 限时抢购信息
     * @param storeId 卖家编号
     */
    private void checkSeckillParam(SeckillVO seckill, String storeId) {
        seckill.checkTime();
        // 同一时间段内相同的活动
        QueryWrapper<Seckill> queryWrapper = PromotionTools.checkActiveTime(seckill.getStartTime(), seckill.getEndTime(), PromotionTypeEnum.SECKILL, storeId, seckill.getId());
        int sameNum = this.count(queryWrapper);
        // 当前时间段是否存在同类活动
        if (sameNum > 0) {
            throw new ServiceException("当前时间内已存在同类活动");
        }
    }
}