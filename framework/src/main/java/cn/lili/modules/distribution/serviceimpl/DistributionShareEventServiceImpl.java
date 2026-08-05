package cn.lili.modules.distribution.serviceimpl;

import cn.lili.modules.distribution.entity.dos.DistributionShareEvent;
import cn.lili.modules.distribution.entity.dos.DistributionVisitEvent;
import cn.lili.modules.distribution.entity.dto.DistributionShareEventDTO;
import cn.lili.modules.distribution.mapper.DistributionShareEventMapper;
import cn.lili.modules.distribution.mapper.DistributionVisitEventMapper;
import cn.lili.modules.distribution.service.DistributionShareEventService;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.IdUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;

/**
 * 分销商品分享行为记录服务实现
 *
 * @author pikachu
 */
@Service
public class DistributionShareEventServiceImpl extends ServiceImpl<DistributionShareEventMapper, DistributionShareEvent>
        implements DistributionShareEventService {

    @Autowired
    private DistributionVisitEventMapper visitEventMapper;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void recordShare(DistributionShareEventDTO dto) {
        String eventId = CharSequenceUtil.isNotBlank(dto.getEventId()) ? dto.getEventId() : dto.getShareId();
        if (CharSequenceUtil.isBlank(eventId)) {
            return;
        }
        // eventId 幂等：已存在则跳过
        long exists = count(new LambdaQueryWrapper<DistributionShareEvent>()
                .eq(DistributionShareEvent::getEventId, eventId));
        if (exists > 0) {
            return;
        }
        DistributionShareEvent event = new DistributionShareEvent();
        event.setEventId(eventId);
        event.setShareId(CharSequenceUtil.isNotBlank(dto.getShareId()) ? dto.getShareId() : eventId);
        event.setDistributionId(dto.getDistributionId());
        event.setGoodsId(dto.getGoodsId());
        event.setSkuId(dto.getSkuId());
        event.setChannel(dto.getChannel());
        event.setTriggerPage(dto.getTriggerPage());
        event.setShareButtonType(dto.getShareButtonType());
        event.setShareTime(new Date());
        save(event);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String recordGoodsShare(String distributionId, String goodsId, String skuId,
                                   String channel, String triggerPage, String shareButtonType) {
        if (CharSequenceUtil.hasBlank(distributionId, goodsId, skuId)) {
            return null;
        }
        String shareId = IdUtil.simpleUUID();
        DistributionShareEvent event = new DistributionShareEvent();
        event.setEventId(shareId);
        event.setShareId(shareId);
        event.setDistributionId(distributionId);
        event.setGoodsId(goodsId);
        event.setSkuId(skuId);
        event.setChannel(CharSequenceUtil.blankToDefault(channel, "WECHAT_MINI_PROGRAM"));
        event.setTriggerPage(triggerPage);
        event.setShareButtonType(shareButtonType);
        event.setShareTime(new Date());
        save(event);
        return shareId;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void recordGoodsVisit(String distributionId, String goodsId, String skuId,
                                 String shareId, String visitorId, String memberId) {
        if (CharSequenceUtil.hasBlank(distributionId, goodsId, skuId, visitorId)) {
            return;
        }
        // 同一访客、同一分销员、同一商品、同一天仅记一条访问
        String eventId = visitorId + ":" + distributionId + ":" + goodsId + ":" + DateUtil.formatDate(new Date());
        long exists = visitEventMapper.selectCount(new LambdaQueryWrapper<DistributionVisitEvent>()
                .eq(DistributionVisitEvent::getEventId, eventId));
        if (exists > 0) {
            return;
        }
        DistributionVisitEvent event = new DistributionVisitEvent();
        event.setEventId(eventId);
        event.setShareId(CharSequenceUtil.blankToDefault(shareId, distributionId));
        event.setDistributionId(distributionId);
        event.setGoodsId(goodsId);
        event.setVisitorId(visitorId);
        event.setMemberId(memberId);
        event.setVisitTime(new Date());
        visitEventMapper.insert(event);
    }
}
