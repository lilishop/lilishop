package cn.lili.modules.stock.serviceimpl;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.stock.entity.dos.StockFlow;
import cn.lili.modules.stock.entity.dto.StockFlowMessage;
import cn.lili.modules.stock.entity.params.StockFlowSearchParams;
import cn.lili.modules.stock.mapper.StockFlowMapper;
import cn.lili.modules.stock.service.StockFlowService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;

/**
 * 库存流水业务实现类
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Service
public class StockFlowServiceImpl extends ServiceImpl<StockFlowMapper, StockFlow> implements StockFlowService {

    @Override
    public IPage<StockFlow> page(StockFlowSearchParams params) {
        LambdaQueryWrapper<StockFlow> queryWrapper = new LambdaQueryWrapper<>();
        if (CharSequenceUtil.isNotEmpty(params.getStoreId())) {
            queryWrapper.eq(StockFlow::getStoreId, params.getStoreId());
        }
        if (CharSequenceUtil.isNotEmpty(params.getGoodsName())) {
            queryWrapper.like(StockFlow::getGoodsName, params.getGoodsName());
        }
        if (CharSequenceUtil.isNotEmpty(params.getSkuId())) {
            queryWrapper.eq(StockFlow::getSkuId, params.getSkuId());
        }
        if (CharSequenceUtil.isNotEmpty(params.getFlowType())) {
            queryWrapper.eq(StockFlow::getFlowType, params.getFlowType());
        }
        if (CharSequenceUtil.isNotEmpty(params.getDirection())) {
            queryWrapper.eq(StockFlow::getDirection, params.getDirection());
        }
        if (CharSequenceUtil.isNotEmpty(params.getBizSn())) {
            queryWrapper.like(StockFlow::getBizSn, params.getBizSn());
        }
        if (CharSequenceUtil.isNotEmpty(params.getStartDate())) {
            Date start = DateUtil.beginOfDay(DateUtil.parse(params.getStartDate()));
            queryWrapper.ge(StockFlow::getCreateTime, start);
        }
        if (CharSequenceUtil.isNotEmpty(params.getEndDate())) {
            Date end = DateUtil.endOfDay(DateUtil.parse(params.getEndDate()));
            queryWrapper.le(StockFlow::getCreateTime, end);
        }
        queryWrapper.orderByDesc(StockFlow::getCreateTime);
        return this.page(PageUtil.initPage(params), queryWrapper);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveFlowFromMessage(StockFlowMessage message) {
        if (message == null || CharSequenceUtil.isEmpty(message.getSkuId())) {
            log.warn("忽略空库存流水消息: {}", message);
            return;
        }
        StockFlow flow = new StockFlow();
        flow.setStoreId(message.getStoreId());
        flow.setGoodsId(message.getGoodsId());
        flow.setSkuId(message.getSkuId());
        flow.setGoodsName(message.getGoodsName());
        flow.setSpecs(message.getSpecs());
        flow.setFlowType(message.getFlowType());
        flow.setDirection(message.getDirection());
        flow.setQuantity(message.getQuantity());
        flow.setBeforeStock(message.getBeforeStock());
        flow.setAfterStock(message.getAfterStock());
        flow.setBizSn(message.getBizSn());
        flow.setOperator(message.getOperator());
        flow.setRemark(message.getRemark());
        flow.setCreateTime(new Date());

        try {
            this.save(flow);
            log.info("库存流水写入成功: skuId={}, flowType={}, bizSn={}, qty={}",
                    flow.getSkuId(), flow.getFlowType(), flow.getBizSn(), flow.getQuantity());
        } catch (DuplicateKeyException e) {
            log.warn("库存流水已存在，忽略重复消费: flowType={}, bizSn={}, skuId={}",
                    flow.getFlowType(), flow.getBizSn(), flow.getSkuId());
        }
    }
}
