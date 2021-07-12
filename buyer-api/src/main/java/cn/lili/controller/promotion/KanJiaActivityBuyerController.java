package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.KanJiaActivity;
import cn.lili.modules.promotion.entity.dos.KanJiaActivityLog;
import cn.lili.modules.promotion.entity.dto.KanJiaActivityQuery;
import cn.lili.modules.promotion.service.KanJiaActivityService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 买家端,砍价活动
 *
 * @author qiuqiu
 * @date 2021/7/12
 **/
@RestController
@Api(tags = "买家端,砍价活动参与记录接口")
@RequestMapping("/buyer/promotion/kanJiaActivity")
public class KanJiaActivityBuyerController {

    @Autowired
    private KanJiaActivityService kanJiaActivityService;

    @GetMapping
    @ApiOperation(value = "分页获取为参与的砍价活动")
    public ResultMessage<IPage<KanJiaActivity>> getPointsGoodsPage(KanJiaActivityQuery kanJiaActivityQuery, PageVO page) {
        // 会员端查询到的肯定是已经开始的活动商品
        kanJiaActivityQuery.setMemberId(UserContext.getCurrentUser().getId());
        IPage<KanJiaActivity> kanJiaActivityIPage = kanJiaActivityService.getForPage(kanJiaActivityQuery, page);
        return ResultUtil.data(kanJiaActivityIPage);
    }

    @PostMapping
    @ApiOperation(value = "发起砍价活动")
    public ResultMessage<KanJiaActivityLog> launchKanJiaActivity(String skuId) {
        KanJiaActivityLog kanJiaActivityLog = kanJiaActivityService.add(skuId);
        return ResultUtil.data(kanJiaActivityLog);
    }

    @PostMapping("/help")
    @ApiOperation(value = "帮砍一刀")
    public ResultMessage<KanJiaActivityLog> helpKanJia(String kanJiaActivityId) {
        KanJiaActivityLog kanJiaActivityLog = kanJiaActivityService.helpKanJia(kanJiaActivityId);
        return ResultUtil.data(kanJiaActivityLog);
    }


}
