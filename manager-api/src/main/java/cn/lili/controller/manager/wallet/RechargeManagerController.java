package cn.lili.controller.manager.wallet;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.trade.entity.vo.RechargeQueryVO;
import cn.lili.modules.wallet.entity.dos.Recharge;
import cn.lili.modules.wallet.service.RechargeService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "管理端,预存款充值记录接口")
@RequestMapping("/manager/wallet/recharge")
public class RechargeManagerController {
    @Autowired
    private RechargeService rechargeService;

    @Operation(summary = "分页获取预存款充值记录")
    @Parameter(name = "page", description = "分页参数", required = true)
    @Parameter(name = "rechargeQueryVO", description = "预存款充值记录查询参数", required = true)
    @GetMapping
    public ResultMessage<IPage<Recharge>> getByPage(PageVO page, RechargeQueryVO rechargeQueryVO) {
        //构建查询 返回数据
        IPage<Recharge> rechargePage = rechargeService.rechargePage(page, rechargeQueryVO);
        return ResultUtil.data(rechargePage);
    }

}
