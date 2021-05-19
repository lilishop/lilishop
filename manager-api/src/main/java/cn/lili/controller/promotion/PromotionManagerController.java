package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dto.PromotionGoodsDTO;
import cn.lili.modules.promotion.service.PromotionService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

/**
 * 管理端,促销接口
 *
 * @author paulG
 * @date 2021/2/2
 **/
@RestController
@Api(tags = "管理端,促销接口")
@RequestMapping("/manager/promotion")
public class PromotionManagerController {

    @Autowired
    private PromotionService promotionService;
    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @GetMapping("/current")
    @ApiOperation(value = "获取当前进行中的促销活动")
    public ResultMessage<Map<String, Object>> getCurrentPromotion() {
        Map<String, Object> currentPromotion = promotionService.getCurrentPromotion();
        return ResultUtil.data(currentPromotion);
    }

    @GetMapping("/{promotionId}/goods")
    @ApiOperation(value = "获取当前进行中的促销活动商品")
    public ResultMessage<IPage<PromotionGoodsDTO>> getPromotionGoods(@PathVariable String promotionId, String promotionType, PageVO pageVO) {
        IPage<PromotionGoodsDTO> promotionGoods = promotionGoodsService.getCurrentPromotionGoods(promotionType, pageVO);
        return ResultUtil.data(promotionGoods);
    }


}
