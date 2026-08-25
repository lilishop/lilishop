package cn.lili.controller.manager.distribution;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.DistributionWalletLog;
import cn.lili.modules.distribution.service.DistributionWalletLogService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端,分销钱包流水接口
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@RestController
@Tag(name = "管理端,分销钱包流水接口")
@RequestMapping("/manager/distribution/walletLog")
public class DistributionWalletLogManagerController {

    @Autowired
    private DistributionWalletLogService walletLogService;

    @Operation(summary = "钱包流水分页")
    @GetMapping
    public ResultMessage<IPage<DistributionWalletLog>> page(@RequestParam(required = false) String distributionId,
                                                            @RequestParam(required = false) String bizType,
                                                            @RequestParam(required = false) String bizSn,
                                                            PageVO page) {
        return ResultUtil.data(walletLogService.pageByDistribution(distributionId, bizType, bizSn, page));
    }
}
