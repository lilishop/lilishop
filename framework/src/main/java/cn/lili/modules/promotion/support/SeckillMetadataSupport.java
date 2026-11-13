package cn.lili.modules.promotion.support;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.ArrayUtil;
import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.dos.SeckillApply;
import cn.lili.modules.promotion.entity.dto.search.SeckillSearchParams;
import cn.lili.modules.promotion.entity.enums.PromotionsApplyStatusEnum;
import cn.lili.modules.promotion.mapper.SeckillApplyMapper;
import cn.lili.modules.promotion.mapper.SeckillMapper;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 秒杀活动基础读取、商品数量和参与范围维护，供活动与申请服务共用，避免服务相互回调。
 * 范围同步沿用调用方事务，无事务时开启事务；搜索实时组装促销信息，不维护外部搜索索引。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class SeckillMetadataSupport {

    private final SeckillMapper seckillMapper;
    private final SeckillApplyMapper applyMapper;

    /**
     * 注入活动和申请存储，不依赖促销业务服务。
     *
     * @param seckillMapper 秒杀活动存储
     * @param applyMapper 秒杀申请存储
     */
    public SeckillMetadataSupport(SeckillMapper seckillMapper, SeckillApplyMapper applyMapper) {
        this.seckillMapper = seckillMapper;
        this.applyMapper = applyMapper;
    }

    /**
     * 按主键读取秒杀活动，沿用存储层逻辑删除规则。
     *
     * @param seckillId 活动主键
     * @return 活动，不存在时为null
     */
    public Seckill getById(String seckillId) {
        return seckillMapper.selectById(seckillId);
    }

    /**
     * 按调用方条件读取活动，不额外添加活动状态或时段过滤。
     *
     * @param queryWrapper 活动查询条件
     * @return 匹配的活动列表
     */
    public List<Seckill> list(Wrapper<Seckill> queryWrapper) {
        return seckillMapper.selectList(queryWrapper);
    }

    /**
     * 活动存在时按未删除的申请记录数回填商品数量，不按SKU去重或限定审核状态。
     * 沿用调用方事务，活动不存在时跳过，不修改店铺数量。
     *
     * @param seckillId 活动主键
     */
    public void updateSeckillGoodsNum(String seckillId) {
        if (getById(seckillId) != null) {
            SeckillSearchParams searchParams = new SeckillSearchParams();
            searchParams.setSeckillId(seckillId);
            LambdaUpdateWrapper<Seckill> updateWrapper = new LambdaUpdateWrapper<>();
            updateWrapper.eq(Seckill::getId, seckillId);
            updateWrapper.set(Seckill::getGoodsNum, applyMapper.selectCount(searchParams.queryWrapper()));
            seckillMapper.update(null, updateWrapper);
        }
    }

    /**
     * 将传入申请的SKU按原顺序拼接为活动范围；列表为空时保留原范围，不额外去重或过滤审核状态。
     * 保存范围后，仅对审核通过的申请计算时段并修改传入活动对象，不将时段写回活动表。
     * 写入参与调用方事务，异常时回滚；内存对象的修改不随数据库事务自动恢复。
     *
     * @param seckill 秒杀活动及供调用方继续使用的时段对象
     * @param seckillApplies 待同步的申请列表，可为null
     */
    @Transactional(rollbackFor = Exception.class)
    public void syncSeckillScope(Seckill seckill, List<SeckillApply> seckillApplies) {
        if (seckillApplies != null && !seckillApplies.isEmpty()) {
            seckill.setScopeId(ArrayUtil.join(seckillApplies.stream().map(SeckillApply::getSkuId).toArray(), ","));
            UpdateWrapper<Seckill> updateWrapper = new UpdateWrapper<>();
            updateWrapper.eq("id", seckill.getId());
            updateWrapper.set("scope_id", seckill.getScopeId());
            seckillMapper.update(null, updateWrapper);
            for (SeckillApply seckillApply : seckillApplies) {
                if (seckillApply.getPromotionApplyStatus().equals(PromotionsApplyStatusEnum.PASS.name())) {
                    setSeckillApplyTime(seckill, seckillApply);
                }
            }
        }
    }

    /**
     * 在活动开始日期上按申请场次计算起止时间，普通场次结束于下一场次开始。
     * 最后一场按原规则结束于该场次小时的59分59秒，仅修改传入活动对象，不持久化。
     *
     * @param seckill 提供日期和场次配置、接收计算结果的活动对象
     * @param seckillApply 提供场次小时的申请记录
     */
    public void setSeckillApplyTime(Seckill seckill, SeckillApply seckillApply) {
        int nextHour = PromotionTools.nextHour(seckill.getHours().split(","), seckillApply.getTimeLine());
        String format = DateUtil.format(seckill.getStartTime(), DatePattern.NORM_DATE_PATTERN);
        DateTime parseStartTime = DateUtil.parse((format + " " + seckillApply.getTimeLine()), "yyyy-MM-dd HH");
        DateTime parseEndTime = DateUtil.parse((format + " " + nextHour), "yyyy-MM-dd HH");
        if (nextHour == seckillApply.getTimeLine()) {
            parseEndTime = DateUtil.parse((format + " " + nextHour + ":59:59"), DatePattern.NORM_DATETIME_PATTERN);
        }
        seckill.setStartTime(parseStartTime);
        seckill.setEndTime(parseEndTime);
    }
}
