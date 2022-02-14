package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.ArrayUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.dos.SeckillApply;
import cn.lili.modules.promotion.entity.dto.search.SeckillSearchParams;
import cn.lili.modules.promotion.entity.enums.PromotionsApplyStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.entity.vos.SeckillVO;
import cn.lili.modules.promotion.mapper.SeckillMapper;
import cn.lili.modules.promotion.service.SeckillApplyService;
import cn.lili.modules.promotion.service.SeckillService;
import cn.lili.modules.promotion.tools.PromotionTools;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.SeckillSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.google.gson.Gson;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.List;

/**
 * 秒杀活动业务层实现
 *
 * @author Chopper
 * @since 2020/8/21
 */
@Service
@Slf4j
public class SeckillServiceImpl extends AbstractPromotionsServiceImpl<SeckillMapper, Seckill> implements SeckillService {

    /**
     * 设置
     */
    @Autowired
    private SettingService settingService;

    @Autowired
    private SeckillApplyService seckillApplyService;

    /**
     * rocketMq配置
     */
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    /**
     * rocketMq
     */
    @Autowired
    private RocketMQTemplate rocketMQTemplate;


    @Override
    public SeckillVO getSeckillDetail(String id) {
        Seckill seckill = this.checkSeckillExist(id);
        SeckillVO seckillVO = new SeckillVO();
        BeanUtils.copyProperties(seckill, seckillVO);
        SeckillSearchParams searchParams = new SeckillSearchParams();
        searchParams.setSeckillId(id);
        seckillVO.setSeckillApplyList(this.seckillApplyService.getSeckillApplyList(searchParams));
        return seckillVO;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void init() {
        //清除演示数据

        List<Seckill> seckillList = this.list();
        for (Seckill seckill : seckillList) {
            seckill.setStartTime(null);
            seckill.setEndTime(null);
            this.updateEsGoodsIndex(seckill);
        }
        this.remove(new QueryWrapper<>());

        Setting setting = settingService.get(SettingEnum.SECKILL_SETTING.name());
        SeckillSetting seckillSetting = new Gson().fromJson(setting.getSettingValue(), SeckillSetting.class);

        for (int i = 1; i <= PRE_CREATION; i++) {
            Seckill seckill = new Seckill(i, seckillSetting.getHours(), seckillSetting.getSeckillRule());
            this.savePromotions(seckill);
        }
    }

    @Override
    public long getApplyNum() {
        DateTime now = DateUtil.date();
        LambdaQueryWrapper<Seckill> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.ge(Seckill::getApplyEndTime, now);
        queryWrapper.le(Seckill::getStartTime, now);
        queryWrapper.ge(Seckill::getEndTime, now);
        return this.count(queryWrapper);
    }

    @Override
    public void updateSeckillGoodsNum(String seckillId) {
        Seckill seckill = this.getById(seckillId);
        if (seckill != null) {
            SeckillSearchParams searchParams = new SeckillSearchParams();
            searchParams.setSeckillId(seckillId);
            LambdaUpdateWrapper<Seckill> updateWrapper = new LambdaUpdateWrapper<>();
            updateWrapper.eq(Seckill::getId, seckillId);
            updateWrapper.set(Seckill::getGoodsNum,
                    this.seckillApplyService.getSeckillApplyCount(searchParams));
            this.update(updateWrapper);

        }
    }

    /**
     * 更新商品索引限时抢购信息
     *
     * @param seckill 限时抢购信息
     */
    @Override
    public void updateEsGoodsSeckill(Seckill seckill, List<SeckillApply> seckillApplies) {
        if (seckillApplies != null && !seckillApplies.isEmpty()) {
            // 更新促销范围
            seckill.setScopeId(ArrayUtil.join(seckillApplies.stream().map(SeckillApply::getSkuId).toArray(), ","));
            UpdateWrapper<Seckill> updateWrapper = new UpdateWrapper<>();
            updateWrapper.eq("id", seckill.getId());
            updateWrapper.set("scope_id", seckill.getScopeId());
            this.update(updateWrapper);
            //循环秒杀商品数据，将数据按照时间段进行存储
            for (SeckillApply seckillApply : seckillApplies) {
                if (seckillApply.getPromotionApplyStatus().equals(PromotionsApplyStatusEnum.PASS.name())) {
                    this.setSeckillApplyTime(seckill, seckillApply);
                }
            }
            if (!seckillApplies.isEmpty()) {
                this.updateEsGoodsIndex(seckill);
            }
        }
    }

    /**
     * 删除商品索引限时抢购信息
     *
     * @param seckill 限时抢购信息
     * @param skuIds  商品skuId列表
     */
    @Override
    public void deleteEsGoodsSeckill(Seckill seckill, List<String> skuIds) {
        seckill.setScopeType(PromotionsScopeTypeEnum.PORTION_GOODS.name());
        seckill.setScopeId(ArrayUtil.join(skuIds.toArray(), ","));
        //删除商品促销消息
        String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.DELETE_GOODS_INDEX_PROMOTIONS.name();
        //发送mq消息
        rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(seckill), RocketmqSendCallbackBuilder.commonCallback());
    }

    @Override
    public void setSeckillApplyTime(Seckill seckill, SeckillApply seckillApply) {
        //下一个时间，默认为当天结束时间
        int nextHour = PromotionTools.nextHour(seckill.getHours().split(","), seckillApply.getTimeLine());

        String format = DateUtil.format(seckill.getStartTime(), DatePattern.NORM_DATE_PATTERN);
        DateTime parseStartTime = DateUtil.parse((format + " " + seckillApply.getTimeLine()), "yyyy-MM-dd HH");
        DateTime parseEndTime = DateUtil.parse((format + " " + nextHour), "yyyy-MM-dd HH");
        //如果是当天最后的时间段则设置到当天结束时间的59分59秒
        if (nextHour == seckillApply.getTimeLine()) {
            parseEndTime = DateUtil.parse((format + " " + nextHour + ":59:59"), DatePattern.NORM_DATETIME_PATTERN);
        }
        seckill.setStartTime(parseStartTime);
        //当时商品的秒杀活动活动结束时间为下个时间段的开始
        seckill.setEndTime(parseEndTime);
    }

    /**
     * 检查该秒杀活动是否存在
     *
     * @param id 秒杀活动编号
     * @return 秒杀活动信息
     */
    private Seckill checkSeckillExist(String id) {
        Seckill seckill = this.getById(id);
        if (seckill == null) {
            throw new ServiceException(ResultCode.SECKILL_NOT_EXIST_ERROR);
        }
        return seckill;
    }

    /**
     * 初始化促销字段
     *
     * @param promotions 促销实体
     */
    @Override
    public void initPromotion(Seckill promotions) {
        super.initPromotion(promotions);
        if (promotions.getStartTime() != null && promotions.getEndTime() == null) {
            promotions.setEndTime(DateUtil.endOfDay(promotions.getStartTime()));
        }
    }

    /**
     * 检查促销状态
     *
     * @param promotions 促销实体
     */
    @Override
    public void checkStatus(Seckill promotions) {
        super.checkStatus(promotions);
        if (promotions.getStartTime() != null && CharSequenceUtil.isNotEmpty(promotions.getHours())) {
            String[] split = promotions.getHours().split(",");
            Arrays.sort(split);
            String startTimeStr = DateUtil.format(promotions.getStartTime(), DatePattern.NORM_DATE_PATTERN) + " " + split[0] + ":00";
            promotions.setStartTime(DateUtil.parse(startTimeStr, DatePattern.NORM_DATETIME_MINUTE_PATTERN));
            promotions.setEndTime(DateUtil.endOfDay(promotions.getStartTime()));
        }
        if (promotions.getStartTime() != null && promotions.getEndTime() != null) {
            //同一时间段内相同的活动
            QueryWrapper<Seckill> queryWrapper = PromotionTools.checkActiveTime(promotions.getStartTime(), promotions.getEndTime(), PromotionTypeEnum.SECKILL, null, promotions.getId());
            long sameNum = this.count(queryWrapper);
            //当前时间段是否存在同类活动
            if (sameNum > 0) {
                throw new ServiceException(ResultCode.PROMOTION_SAME_ACTIVE_EXIST);
            }
        }


    }

    /**
     * 当前促销类型
     *
     * @return 当前促销类型
     */
    @Override
    public PromotionTypeEnum getPromotionType() {
        return PromotionTypeEnum.SECKILL;
    }
}