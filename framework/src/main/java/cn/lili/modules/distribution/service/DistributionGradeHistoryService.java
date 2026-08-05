package cn.lili.modules.distribution.service;

import cn.lili.modules.distribution.entity.dos.DistributionGradeHistory;
import cn.lili.modules.distribution.entity.dto.DistributionGradeHistorySearchParams;
import cn.lili.modules.distribution.entity.vos.DistributionGradeHistoryVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 分销等级变更历史服务
 *
 * @author Bulbasaur
 * @since 2026-08-05
 */
public interface DistributionGradeHistoryService extends IService<DistributionGradeHistory> {

    /**
     * 分页查询等级变更历史。
     *
     * @param params 查询参数
     * @return 历史分页
     */
    IPage<DistributionGradeHistoryVO> historyPage(DistributionGradeHistorySearchParams params);
}
