package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.Pintuan;
import cn.lili.modules.promotion.entity.dto.PromotionGoodsDTO;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.entity.vos.PintuanSearchParams;
import cn.lili.modules.promotion.entity.vos.PintuanVO;
import cn.lili.modules.promotion.entity.vos.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.service.PintuanService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Date;

/**
 * 管理端,平台拼团接口
 *
 * @author paulG
 * @date 2020/10/9
 **/
@RestController
@Api(tags = "管理端,平台拼团接口")
@RequestMapping("/manager/promotion/pintuan")
public class PintuanManagerController {
    @Autowired
    private PintuanService pintuanService;
    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @GetMapping(value = "/{id}")
    @ApiOperation(value = "通过id获取")
    public ResultMessage<Pintuan> get(@PathVariable String id) {
        Pintuan pintuan = pintuanService.getById(id);
        return ResultUtil.data(pintuan);
    }

    @GetMapping
    @ApiOperation(value = "根据条件分页查询拼团活动列表")
    public ResultMessage<IPage<PintuanVO>> getPintuanByPage(PintuanSearchParams queryParam, PageVO pageVo) {
        IPage<PintuanVO> pintuanByPageFromMongo = pintuanService.getPintuanByPageFromMongo(queryParam, pageVo);
        return ResultUtil.data(pintuanByPageFromMongo);
    }

    @GetMapping("/goods/{pintuanId}")
    @ApiOperation(value = "根据条件分页查询拼团活动商品列表")
    public ResultMessage<IPage<PromotionGoodsDTO>> getPintuanGoodsByPage(@PathVariable String pintuanId, PageVO pageVo) {
        PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
        searchParams.setPromotionId(pintuanId);
        searchParams.setPromotionType(PromotionTypeEnum.PINTUAN.name());
        IPage<PromotionGoodsDTO> promotionGoods = promotionGoodsService.getPromotionGoods(searchParams, pageVo);
        return ResultUtil.data(promotionGoods);
    }

    @PutMapping("/open/{pintuanId}")
    @ApiOperation(value = "手动开启拼团活动")
    public ResultMessage<String> openPintuan(@PathVariable String pintuanId, Long startTime, Long endTime) {
        if (pintuanService.openPintuan(pintuanId, new Date(startTime), new Date(endTime))) {
            return ResultUtil.success(ResultCode.PINTUAN_MANUAL_OPEN_SUCCESS);
        }
        throw new ServiceException(ResultCode.PINTUAN_MANUAL_OPEN_ERROR);

    }

    @PutMapping("/close/{pintuanId}")
    @ApiOperation(value = "手动关闭拼团活动")
    public ResultMessage<String> closePintuan(@PathVariable String pintuanId) {
        if (pintuanService.closePintuan(pintuanId)) {
            return ResultUtil.success(ResultCode.PINTUAN_MANUAL_CLOSE_SUCCESS);
        }
        throw new ServiceException(ResultCode.PINTUAN_MANUAL_CLOSE_ERROR);
    }


}
