package cn.lili.modules.distribution.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dos.DistributionWalletLog;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 分销钱包流水服务。
 * <p>
 * 查询分销员钱包资金变动明细，用于对账与审计。
 * </p>
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public interface DistributionWalletLogService extends IService<DistributionWalletLog> {

    /**
     * 分页查询指定分销员的钱包流水。
     *
     * @param distributionId 分销员 ID
     * @param bizType        业务类型（可选）
     * @param bizSn          业务单号（可选）
     * @param page           分页参数
     * @return 钱包流水分页
     */
    IPage<DistributionWalletLog> pageByDistribution(String distributionId, String bizType, String bizSn, PageVO page);
}
