package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
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
 * 店铺端,拼团管理接口
 *
 * @author paulG
 * @since 2020/10/9
 **/
@RestController
@Api(tags = "店铺端,拼团管理接口")
@RequestMapping("/store/promotion/pintuan")
public class PintuanStoreController {

    @Autowired
    private PintuanService pintuanService;
    @Autowired
    private PromotionGoodsService promotionGoodsService;


    @GetMapping
    @ApiOperation(value = "根据条件分页查询拼团活动列表")
    public ResultMessage<IPage<PintuanVO>> getPintuanByPage(PintuanSearchParams queryParam, PageVO pageVo) {
        AuthUser currentUser = UserContext.getCurrentUser();
        queryParam.setStoreId(currentUser.getStoreId());
        IPage<PintuanVO> pintuanByPageFromMongo = pintuanService.getPintuanByPageFromMongo(queryParam, pageVo);
        return ResultUtil.data(pintuanByPageFromMongo);
    }

    @GetMapping(value = "/{id}")
    @ApiOperation(value = "通过id获取")
    public ResultMessage<PintuanVO> get(@PathVariable String id) {
        return ResultUtil.data(pintuanService.getPintuanByIdFromMongo(id));
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

    @PostMapping(consumes = "application/json", produces = "application/json")
    @ApiOperation(value = "添加拼团活动")
    public ResultMessage<String> addPintuan(@RequestBody PintuanVO pintuan) {
        AuthUser currentUser = UserContext.getCurrentUser();
        pintuan.setStoreId(currentUser.getStoreId());
        pintuan.setStoreName(currentUser.getStoreName());
        if (pintuanService.addPintuan(pintuan)) {
            return ResultUtil.success(ResultCode.PINTUAN_ADD_SUCCESS);
        }
        throw new ServiceException(ResultCode.PINTUAN_ADD_ERROR);
    }

    @PutMapping(consumes = "application/json", produces = "application/json")
    @ApiOperation(value = "修改拼团活动")
    public ResultMessage<String> editPintuan(@RequestBody PintuanVO pintuan) {
        AuthUser currentUser = UserContext.getCurrentUser();
        pintuan.setStoreId(currentUser.getStoreId());
        pintuan.setStoreName(currentUser.getStoreName());
        if (pintuanService.modifyPintuan(pintuan)) {
            return ResultUtil.success(ResultCode.PINTUAN_EDIT_SUCCESS);
        }
        throw new ServiceException(ResultCode.PINTUAN_EDIT_ERROR);
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

    @DeleteMapping("/{pintuanId}")
    @ApiOperation(value = "手动删除拼团活动")
    public ResultMessage<String> deletePintuan(@PathVariable String pintuanId) {
        if (pintuanService.deletePintuan(pintuanId)) {
            return ResultUtil.success(ResultCode.PINTUAN_DELETE_SUCCESS);
        }
        throw new ServiceException(ResultCode.PINTUAN_DELETE_ERROR);
    }

}
