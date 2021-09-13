package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.dos.SeckillApply;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.enums.SeckillApplyStatusEnum;
import cn.lili.modules.promotion.entity.vos.SeckillSearchParams;
import cn.lili.modules.promotion.entity.vos.SeckillVO;
import cn.lili.modules.promotion.mapper.SeckillMapper;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.service.SeckillApplyService;
import cn.lili.modules.promotion.service.SeckillService;
import cn.lili.modules.promotion.tools.PromotionTools;
import cn.lili.modules.search.service.EsGoodsIndexService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.SeckillSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.trigger.enums.DelayTypeEnums;
import cn.lili.trigger.interfaces.TimeTrigger;
import cn.lili.trigger.message.PromotionMessage;
import cn.lili.trigger.model.TimeExecuteConstant;
import cn.lili.trigger.model.TimeTriggerMsg;
import cn.lili.trigger.util.DelayQueueTools;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.google.gson.Gson;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 秒杀活动业务层实现
 *
 * @author Chopper
 * @since 2020/8/21
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class SeckillServiceImpl extends ServiceImpl<SeckillMapper, Seckill> implements SeckillService {


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
     * 商品索引
     */
    @Autowired
    private EsGoodsIndexService goodsIndexService;
    /**
     * 促销商品
     */
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    /**
     * 秒杀申请
     */
    @Autowired
    private SeckillApplyService seckillApplyService;
    /**
     * 设置
     */
    @Autowired
    private SettingService settingService;

    @Override
    public IPage<Seckill> getSeckillByPageFromMysql(SeckillSearchParams queryParam, PageVO pageVo) {
        QueryWrapper<Seckill> queryWrapper = queryParam.wrapper();
        return page(PageUtil.initPage(pageVo), queryWrapper);
    }

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

    @Override
    public SeckillVO getSeckillByIdFromMongo(String id) {
        return this.checkSeckillExist(id);
    }

    @Override
    public void init() {
        //清除演示数据

        List<Seckill> seckillList = list();
        for (Seckill seckill : seckillList) {
            this.timeTrigger.delete(TimeExecuteConstant.PROMOTION_EXECUTOR,
                    seckill.getStartTime().getTime(),
                    DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (PromotionTypeEnum.SECKILL.name() + seckill.getId())),
                    rocketmqCustomProperties.getPromotionTopic());
            this.removeById(seckill.getId());
        }

        Setting setting = settingService.get(SettingEnum.SECKILL_SETTING.name());
        SeckillSetting seckillSetting = new Gson().fromJson(setting.getSettingValue(), SeckillSetting.class);

        for (int i = 1; i <= PRE_CREATION; i++) {
            Seckill seckill = new Seckill(i, seckillSetting.getHours(), seckillSetting.getSeckillRule());
            this.saveSeckill(seckill);
        }
    }

    @Override
    public boolean saveSeckill(Seckill seckill) {

        SeckillVO seckillVO = new SeckillVO();
        BeanUtil.copyProperties(seckill, seckillVO);

        seckillVO.setSeckillApplyStatus(SeckillApplyStatusEnum.NOT_APPLY.name());
        seckillVO.setSeckillApplyList(null);
        //检查秒杀活动参数
        checkSeckillParam(seckillVO);
        //保存到MYSQL中
        boolean result = this.save(seckillVO);
        //保存到MONGO中
        this.mongoTemplate.save(seckillVO);
        //添加秒杀延时任务
        this.addSeckillStartTask(seckillVO);
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
        //检查该秒杀活动是否存在
        SeckillVO seckill = checkSeckillExist(seckillVO.getId());
        if (PromotionStatusEnum.START.name().equals(seckillVO.getPromotionStatus())) {
            throw new ServiceException(ResultCode.PROMOTION_UPDATE_ERROR);
        }
        PromotionTools.checkPromotionTime(seckillVO.getStartTime().getTime(), seckillVO.getEndTime().getTime());
        //更新到MYSQL中
        boolean result = this.updateById(seckillVO);
        //保存到MONGO中
        this.mongoTemplate.save(seckillVO);
        //如果编辑后活动时间不一致，则编辑延时任务
        if (seckill.getStartTime().getTime() != seckillVO.getStartTime().getTime()) {
            if (seckillVO.getEndTime() == null) {
                seckillVO.setEndTime(cn.hutool.core.date.DateUtil.endOfDay(new Date()));
            }
            PromotionMessage promotionMessage = new PromotionMessage(seckillVO.getId(), PromotionTypeEnum.SECKILL.name(), PromotionStatusEnum.START.name(), seckillVO.getStartTime(), seckillVO.getEndTime());
            //更新延时任务
            this.timeTrigger.edit(TimeExecuteConstant.PROMOTION_EXECUTOR,
                    promotionMessage,
                    seckill.getStartTime().getTime(),
                    seckillVO.getStartTime().getTime(),
                    DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (promotionMessage.getPromotionType() + promotionMessage.getPromotionId())),
                    DateUtil.getDelayTime(seckillVO.getStartTime().getTime()),
                    rocketmqCustomProperties.getPromotionTopic());
        }
        return result;
    }

    @Override
    public void deleteSeckill(String id) {
        Seckill seckill = checkSeckillExist(id);
        if (PromotionStatusEnum.CLOSE.name().equals(seckill.getPromotionStatus()) || PromotionStatusEnum.END.name().equals(seckill.getPromotionStatus())) {
            //更新秒杀活动状态为关闭，标示删除标志
            LambdaUpdateWrapper<Seckill> updateWrapper = new LambdaUpdateWrapper<Seckill>().eq(Seckill::getId, id).set(Seckill::getDeleteFlag, true).set(Seckill::getPromotionStatus, PromotionStatusEnum.CLOSE.name());
            this.update(updateWrapper);
            LambdaUpdateWrapper<SeckillApply> seckillApplyLambdaUpdateWrapper = new LambdaUpdateWrapper<SeckillApply>().eq(SeckillApply::getSeckillId, id).set(SeckillApply::getDeleteFlag, true);
            this.seckillApplyService.update(seckillApplyLambdaUpdateWrapper);
            this.mongoTemplate.remove(new Query().addCriteria(Criteria.where("id").is(id)), SeckillVO.class);
            LambdaUpdateWrapper<PromotionGoods> promotionGoodsQueryWrapper = new LambdaUpdateWrapper<PromotionGoods>().eq(PromotionGoods::getPromotionId, id).set(PromotionGoods::getDeleteFlag, true);
            this.promotionGoodsService.update(promotionGoodsQueryWrapper);
            this.timeTrigger.delete(TimeExecuteConstant.PROMOTION_EXECUTOR,
                    seckill.getStartTime().getTime(),
                    DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (PromotionTypeEnum.SECKILL.name() + seckill.getId())),
                    rocketmqCustomProperties.getPromotionTopic());
        } else {
            throw new ServiceException(ResultCode.SECKILL_DELETE_ERROR);
        }
    }

    /**
     * 开启一个秒杀活动
     *
     * @param id 秒杀活动编号
     */
    @Override
    public void openSeckill(String id) {
        SeckillVO seckillVO = checkSeckillExist(id);
        if (seckillVO.getEndTime() == null) {
            seckillVO.setEndTime(cn.hutool.core.date.DateUtil.endOfDay(seckillVO.getStartTime()));
        }
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
                        DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (PromotionTypeEnum.SECKILL.name() + seckillVO.getId())),
                        rocketmqCustomProperties.getPromotionTopic());
            }
        } else {
            throw new ServiceException(ResultCode.SECKILL_CLOSE_ERROR);
        }
    }

    @Override
    public Integer getApplyNum() {
        LambdaQueryWrapper<Seckill> queryWrapper = Wrappers.lambdaQuery();
        //秒杀申请时间未超过当前时间
        queryWrapper.ge(Seckill::getApplyEndTime, cn.hutool.core.date.DateUtil.date());
        queryWrapper.eq(Seckill::getPromotionStatus, PromotionStatusEnum.NEW.name());
        return this.count(queryWrapper);
    }

    @Override
    public void updateSeckillGoodsNum(String seckillId) {
        this.baseMapper.updateSeckillGoodsNum(seckillId);
    }

    /**
     * 添加秒杀活动延时任务
     *
     * @param seckill 秒杀活动
     */
    public void addSeckillStartTask(SeckillVO seckill) {
        PromotionMessage promotionMessage = new PromotionMessage(seckill.getId(), PromotionTypeEnum.SECKILL.name(), PromotionStatusEnum.START.name(), seckill.getStartTime(), seckill.getEndTime());
        TimeTriggerMsg timeTriggerMsg = new TimeTriggerMsg(TimeExecuteConstant.PROMOTION_EXECUTOR,
                seckill.getStartTime().getTime(),
                promotionMessage,
                DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (promotionMessage.getPromotionType() + promotionMessage.getPromotionId())),
                rocketmqCustomProperties.getPromotionTopic());
        //发送促销活动开始的延时任务
        this.timeTrigger.addDelay(timeTriggerMsg);
    }

    /**
     * 检查该秒杀活动是否存在
     *
     * @param id 秒杀活动编号
     * @return 秒杀活动信息
     */
    private SeckillVO checkSeckillExist(String id) {
        SeckillVO seckill = this.mongoTemplate.findById(id, SeckillVO.class);
        if (seckill == null) {
            throw new ServiceException(ResultCode.SECKILL_NOT_EXIST_ERROR);
        }
        return seckill;
    }

    /**
     * 检查秒杀活动参数
     *
     * @param seckill 秒杀活动信息
     */
    private void checkSeckillParam(SeckillVO seckill) {
        //同一时间段内相同的活动
        QueryWrapper<Seckill> queryWrapper = PromotionTools.checkActiveTime(seckill.getStartTime(), seckill.getEndTime(), PromotionTypeEnum.SECKILL, null, seckill.getId());
        int sameNum = this.count(queryWrapper);
        //当前时间段是否存在同类活动
        if (sameNum > 0) {
            throw new ServiceException("当前时间内已存在同类活动:" + seckill.getStartTime());
        }
    }
}