package cn.lili.controller.wallet;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.trade.entity.vo.DepositQueryVO;
import cn.lili.modules.wallet.entity.dos.WalletLog;
import cn.lili.modules.wallet.service.WalletLogService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端,预存款充值记录接口
 *
 * @author pikachu
 * @since 2020/11/16 10:07 下午
 */
@RestController
@Api(tags = "管理端,预存款充值记录接口")
@RequestMapping("/manager/wallet/log")
public class WalletLogManagerController {
    @Autowired
    private WalletLogService walletLogService;

    @ApiOperation(value = "分页获取预存款充值记录")
    @GetMapping
    public ResultMessage<IPage<WalletLog>> getByPage(PageVO page, DepositQueryVO depositQueryVO) {
        //构建查询 返回数据
        IPage<WalletLog> depositLogPage = walletLogService.depositLogPage(page, depositQueryVO);
        return ResultUtil.data(depositLogPage);
    }
}
