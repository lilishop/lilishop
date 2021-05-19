package cn.lili.controller.trade;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.trade.entity.dos.Recharge;
import cn.lili.modules.order.trade.entity.vo.RechargeQueryVO;
import cn.lili.modules.order.trade.service.RechargeService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;


/**
 * 管理端,预存款充值记录接口
 *
 * @author pikachu
 * @date: 2020/11/16 10:07 下午
 */
@RestController
@Api(tags = "管理端,预存款充值记录接口")
@RequestMapping("/manager/recharge")
@Transactional(rollbackFor = Exception.class)
public class RechargeManagerController {
    @Autowired
    private RechargeService rechargeService;

    @ApiOperation(value = "分页获取预存款充值记录")
    @GetMapping
    public ResultMessage<IPage<Recharge>> getByPage(PageVO page, RechargeQueryVO rechargeQueryVO) {
        //构建查询 返回数据
        IPage<Recharge> rechargePage = rechargeService.rechargePage(page, rechargeQueryVO);
        return ResultUtil.data(rechargePage);
    }

}
