package cn.lili.modules.statistics.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dos.DistributionCash;
import cn.lili.modules.distribution.entity.vos.DistributionCashSearchParams;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * 分销佣金统计
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
public interface DistributionCashStatisticsService extends IService<DistributionCash> {

    /**
     * 待处理分销员提现申请数量
     *
     * @return 待处理分销员提现申请数量
     */
    Integer newDistributionCash();
}