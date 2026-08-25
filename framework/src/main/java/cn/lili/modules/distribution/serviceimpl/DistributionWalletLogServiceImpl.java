package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dos.DistributionWalletLog;
import cn.lili.modules.distribution.mapper.DistributionWalletLogMapper;
import cn.lili.modules.distribution.service.DistributionWalletLogService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * 分销钱包流水服务实现
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Service
public class DistributionWalletLogServiceImpl extends ServiceImpl<DistributionWalletLogMapper, DistributionWalletLog>
        implements DistributionWalletLogService {

    /**
     * 分页查询分销员钱包流水。
     *
     * @param distributionId 分销员ID
     * @param bizType        业务类型
     * @param bizSn          业务单号
     * @param page           分页参数
     * @return 流水分页
     */
    @Override
    public IPage<DistributionWalletLog> pageByDistribution(String distributionId, String bizType, String bizSn, PageVO page) {
        LambdaQueryWrapper<DistributionWalletLog> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(CharSequenceUtil.isNotEmpty(distributionId), DistributionWalletLog::getDistributionId, distributionId);
        wrapper.eq(CharSequenceUtil.isNotEmpty(bizType), DistributionWalletLog::getBizType, bizType);
        wrapper.eq(CharSequenceUtil.isNotEmpty(bizSn), DistributionWalletLog::getBizSn, bizSn);
        wrapper.orderByDesc(DistributionWalletLog::getCreateTime);
        return this.page(PageUtil.initPage(page), wrapper);
    }
}
