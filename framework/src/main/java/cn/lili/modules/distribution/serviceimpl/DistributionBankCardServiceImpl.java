package cn.lili.modules.distribution.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionBankCard;
import cn.lili.modules.distribution.mapper.DistributionBankCardMapper;
import cn.lili.modules.distribution.service.DistributionBankCardService;
import cn.lili.modules.distribution.entity.dto.DistributionBankCardAddDTO;
import cn.lili.modules.distribution.service.DistributionService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 分销员银行卡业务层实现
 *
 * @author Bulbasaur
 * @since 2026-08-06
 */
@Service
public class DistributionBankCardServiceImpl
        extends ServiceImpl<DistributionBankCardMapper, DistributionBankCard>
        implements DistributionBankCardService {

    @Autowired
    private DistributionService distributionService;

    @Override
    public List<DistributionBankCard> listMyCards() {
        Distribution distribution = getCheckedDistribution();
        LambdaQueryWrapper<DistributionBankCard> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(DistributionBankCard::getDistributionId, distribution.getId())
               .orderByDesc(DistributionBankCard::getIsDefault)
               .orderByDesc(DistributionBankCard::getCreateTime);
        return this.list(wrapper);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public DistributionBankCard addCard(DistributionBankCardAddDTO dto) {
        Distribution distribution = getCheckedDistribution();

        long count = this.count(new LambdaQueryWrapper<DistributionBankCard>()
                .eq(DistributionBankCard::getDistributionId, distribution.getId())
                .eq(DistributionBankCard::getCardNo, dto.getCardNo()));
        if (count > 0) {
            throw new ServiceException("该银行卡号已存在");
        }

        DistributionBankCard card = new DistributionBankCard();
        card.setDistributionId(distribution.getId());
        card.setHolderName(dto.getHolderName());
        card.setBankName(dto.getBankName());
        card.setCardNo(dto.getCardNo());

        // 第一张卡自动设为默认
        boolean hasCards = this.count(new LambdaQueryWrapper<DistributionBankCard>()
                .eq(DistributionBankCard::getDistributionId, distribution.getId())) > 0;
        card.setIsDefault(!hasCards);

        this.save(card);
        return card;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteCard(String id) {
        Distribution distribution = getCheckedDistribution();
        DistributionBankCard card = getOwnCard(id, distribution.getId());

        this.removeById(id);

        // 若删除的是默认卡，自动将最新一张卡设为默认
        if (Boolean.TRUE.equals(card.getIsDefault())) {
            DistributionBankCard next = this.getOne(new LambdaQueryWrapper<DistributionBankCard>()
                    .eq(DistributionBankCard::getDistributionId, distribution.getId())
                    .orderByDesc(DistributionBankCard::getCreateTime)
                    .last("LIMIT 1"));
            if (next != null) {
                next.setIsDefault(true);
                this.updateById(next);
            }
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void setDefault(String id) {
        Distribution distribution = getCheckedDistribution();
        // 权限校验
        getOwnCard(id, distribution.getId());

        // 先清除该分销员所有默认标记
        this.update(new LambdaUpdateWrapper<DistributionBankCard>()
                .eq(DistributionBankCard::getDistributionId, distribution.getId())
                .set(DistributionBankCard::getIsDefault, false));

        // 再设置目标卡为默认
        this.update(new LambdaUpdateWrapper<DistributionBankCard>()
                .eq(DistributionBankCard::getId, id)
                .set(DistributionBankCard::getIsDefault, true));
    }

    /**
     * 获取当前登录且已通过审核的分销员。
     *
     * @return 分销员实体
     */
    private Distribution getCheckedDistribution() {
        Distribution distribution = distributionService.getDistribution();
        if (distribution == null) {
            throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
        }
        return distribution;
    }

    /**
     * 校验银行卡归属当前分销员。
     *
     * @param id             银行卡 ID
     * @param distributionId 分销员 ID
     * @return 银行卡实体
     */
    private DistributionBankCard getOwnCard(String id, String distributionId) {
        DistributionBankCard card = this.getById(id);
        if (card == null || !distributionId.equals(card.getDistributionId())) {
            throw new ServiceException(ResultCode.ILLEGAL_REQUEST_ERROR);
        }
        return card;
    }
}
