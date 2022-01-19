package cn.lili.controller.wallet;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.trade.entity.vo.RechargeQueryVO;
import cn.lili.modules.wallet.entity.dos.Recharge;
import cn.lili.modules.wallet.service.RechargeService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 买家端,预存款充值记录接口
 *
 * @author pikachu
 * @since 2020/11/16 10:07 下午
 */
@RestController
@Api(tags = "买家端,预存款充值记录接口")
@RequestMapping("/buyer/member/recharge")
public class RechargeBuyerController {

    @Autowired
    private RechargeService rechargeService;

    @ApiOperation(value = "分页获取预存款充值记录")
    @GetMapping
    public ResultMessage<IPage<Recharge>> getByPage(PageVO page) {
        //构建查询参数
        RechargeQueryVO rechargeQueryVO = new RechargeQueryVO();
        rechargeQueryVO.setMemberId(UserContext.getCurrentUser().getId());
        //构建查询 返回数据
        IPage<Recharge> rechargePage = rechargeService.rechargePage(page, rechargeQueryVO);
        return ResultUtil.data(rechargePage);
    }
}
