package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.convert.Convert;
import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.dos.SeckillApply;
import cn.lili.modules.promotion.entity.dto.search.SeckillSearchParams;
import cn.lili.modules.promotion.entity.vos.SeckillVO;
import cn.lili.modules.promotion.mapper.SeckillMapper;
import cn.lili.modules.promotion.service.SeckillApplyService;
import cn.lili.modules.promotion.service.SeckillService;
import cn.lili.modules.promotion.support.SeckillMetadataSupport;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.SeckillSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.google.gson.Gson;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.Date;
import java.util.List;

/**
 * 秒杀活动业务层实现
 * 活动与申请共用基础维护组件，申请服务不再反向依赖本服务。
 *
 * @author Chopper
 * @since 2020/8/21
 */
@Service
public class SeckillServiceImpl extends AbstractPromotionsServiceImpl<SeckillMapper, Seckill> implements SeckillService {

    @Autowired
    private SettingService settingService;
    @Autowired
    private SeckillApplyService seckillApplyService;
    @Autowired
    private SeckillMetadataSupport seckillMetadata;


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
            this.updateGoodsIndex(seckill);
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

    /**
     * 按现有申请查询条件统计商品数量，不按SKU去重或限定审核状态，沿用调用方事务。
     *
     * @param seckillId 活动主键，不存在时跳过更新
     */
    @Override
    public void updateSeckillGoodsNum(String seckillId) {
        seckillMetadata.updateSeckillGoodsNum(seckillId);
    }

    /**
     * 检查活动状态、促销参数和报名截止时间后更新活动，再同步申请时间及促销商品。
     * 报名截止时间不得早于当前时间或晚于活动开始时间；关联数据库修改在同一事务中回滚。
     *
     * @param promotions 促销信息
     * @return 是否更新成功
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean updatePromotions(Seckill promotions) {
        this.checkStatus(promotions);
        this.checkPromotions(promotions);
        //如果申请结束时间在当前时间之前
        if (promotions.getApplyEndTime().before(new Date()) || promotions.getApplyEndTime().after(promotions.getStartTime())) {
            throw new ServiceException(ResultCode.APPLY_END_TIME_ERROR);
        }
        boolean result = this.updateById(promotions);
        seckillApplyService.updateSeckillApplyTime(promotions);
        return result;
    }


    /**
     * 同步秒杀活动商品范围
     * 非空申请列表按原顺序覆盖范围；空列表保留原范围。数据库写入参与当前事务。
     *
     * @param seckill 限时抢购信息
     * @param seckillApplies 参与活动的申请列表，可为null
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void syncSeckillScope(Seckill seckill, List<SeckillApply> seckillApplies) {
        seckillMetadata.syncSeckillScope(seckill, seckillApplies);
        if (seckillApplies != null && !seckillApplies.isEmpty()) {
            this.updateGoodsIndex(seckill);
        }
    }

    /**
     * 移除秒杀活动中的 SKU（Elasticsearch 已移除，保留接口兼容）
     *
     * @param seckill 限时抢购信息
     * @param skuIds  商品skuId列表
     */
    @Override
    public void removeSeckillSku(Seckill seckill, List<String> skuIds) {
        // Elasticsearch 已移除
    }

    /**
     * 按申请场次回填活动对象的起止时间，不写入数据库，边界规则由基础维护组件统一处理。
     *
     * @param seckill 提供日期和场次配置、接收计算结果的活动对象
     * @param seckillApply 提供场次小时的申请记录
     */
    @Override
    public void setSeckillApplyTime(Seckill seckill, SeckillApply seckillApply) {
        seckillMetadata.setSeckillApplyTime(seckill, seckillApply);
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
        if (promotions.getApplyEndTime() == null && promotions.getStartTime() != null) {
            promotions.setApplyEndTime(promotions.getStartTime());
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
            Integer[] split = Convert.toIntArray(promotions.getHours().split(","));
            Arrays.sort(split);
            String startTimeStr = DateUtil.format(promotions.getStartTime(), DatePattern.NORM_DATE_PATTERN) + " " + split[0] + ":00";
            promotions.setStartTime(DateUtil.parse(startTimeStr, DatePattern.NORM_DATETIME_MINUTE_PATTERN));
            promotions.setEndTime(DateUtil.endOfDay(promotions.getStartTime()));
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
