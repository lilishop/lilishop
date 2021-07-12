package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.KanJiaActivity;
import cn.lili.modules.promotion.entity.dos.KanJiaActivityLog;
import cn.lili.modules.promotion.entity.dto.KanJiaActivityLogQuery;
import cn.lili.modules.promotion.entity.dto.KanJiaActivityQuery;
import cn.lili.modules.promotion.service.KanJiaActivityLogService;
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
@Api(tags = "买家端,砍价活动帮砍记录接口")
@RequestMapping("/buyer/promotion/kanJiaActivityLog")
public class KanJiaActivityLogBuyerController {

    @Autowired
    private KanJiaActivityLogService kanJiaActivityLogService;

    @GetMapping
    @ApiOperation(value = "分页获取为参与的帮砍记录")
    public ResultMessage<IPage<KanJiaActivityLog>> getPointsGoodsPage(KanJiaActivityLogQuery kanJiaActivityLogQuery, PageVO page) {
        // 会员端查询到的肯定是已经开始的活动商品
        kanJiaActivityLogQuery.setMemberId(UserContext.getCurrentUser().getId());
        IPage<KanJiaActivityLog> kanJiaActivityLogIPage = kanJiaActivityLogService.getForPage(kanJiaActivityLogQuery, page);
        return ResultUtil.data(kanJiaActivityLogIPage);
    }


}
