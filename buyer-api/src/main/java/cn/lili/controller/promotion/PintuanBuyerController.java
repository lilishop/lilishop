package cn.lili.controller.promotion;

import cn.hutool.core.date.DateUtil;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dto.PromotionGoodsDTO;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.entity.vos.PintuanMemberVO;
import cn.lili.modules.promotion.entity.vos.PintuanShareVO;
import cn.lili.modules.promotion.entity.vos.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.service.PintuanService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 买家端,拼团接口
 *
 * @author paulG
 * @date 2021/2/20
 **/
@Api(tags = "买家端,拼团接口")
@RestController
@RequestMapping("/buyer/promotion/pintuan")
public class PintuanBuyerController {
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    @Autowired
    private PintuanService pintuanService;

    @ApiOperation(value = "获取拼团商品")
    @GetMapping
    public ResultMessage<IPage<PromotionGoodsDTO>> getPintuanCategory(String goodsName, String categoryPath, PageVO pageVo) {
        PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
        searchParams.setGoodsName(goodsName);
        searchParams.setPromotionType(PromotionTypeEnum.PINTUAN.name());
        searchParams.setPromotionStatus(PromotionStatusEnum.START.name());
        searchParams.setCategoryPath(categoryPath);
        searchParams.setEndTime(DateUtil.date().getTime());
        return ResultUtil.data(promotionGoodsService.getPromotionGoods(searchParams, pageVo));
    }


    @ApiOperation(value = "获取当前拼团活动的未成团的会员")
    @GetMapping("/{pintuanId}/members")
    public ResultMessage<List<PintuanMemberVO>> getPintuanMember(@PathVariable String pintuanId) {
        return ResultUtil.data(pintuanService.getPintuanMember(pintuanId));
    }

    @ApiOperation(value = "获取当前拼团订单的拼团分享信息")
    @GetMapping("/share")
    public ResultMessage<PintuanShareVO> share(String parentOrderSn, String skuId) {
        return ResultUtil.data(pintuanService.getPintuanShareInfo(parentOrderSn, skuId));
    }

}
