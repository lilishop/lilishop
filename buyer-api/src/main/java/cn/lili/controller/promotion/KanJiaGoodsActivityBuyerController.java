package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dto.KanJiaActivityGoodsDTO;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.vos.KanJiaActivityGoodsParams;
import cn.lili.modules.promotion.service.KanJiaActivityGoodsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 买家端,砍价活动商品
 *
 * @author qiuqiu
 * @date 2021/7/12
 **/
@RestController
@Api(tags = "买家端,砍价商品接口")
@RequestMapping("/buyer/promotion/kanJiaGoods")
public class KanJiaGoodsActivityBuyerController {

    @Autowired
    private KanJiaActivityGoodsService kanJiaActivityGoodsService;

    @GetMapping
    @ApiOperation(value = "分页获取砍价商品")
    public ResultMessage<IPage<KanJiaActivityGoodsDTO>> getPointsGoodsPage(KanJiaActivityGoodsParams kanJiaActivityGoodsParams, PageVO page) {
        // 会员端查询到的肯定是已经开始的活动商品
        kanJiaActivityGoodsParams.setPromotionStatus(PromotionStatusEnum.START.name());
        IPage<KanJiaActivityGoodsDTO> kanJiaActivityGoodsDTOIPage = kanJiaActivityGoodsService.getForPage(kanJiaActivityGoodsParams, page);
        return ResultUtil.data(kanJiaActivityGoodsDTOIPage);
    }


}
