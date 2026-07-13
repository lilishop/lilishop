package cn.lili.controller.seller.promotion;

import cn.hutool.core.util.ArrayUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.Pintuan;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.search.PintuanSearchParams;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.entity.vos.PintuanVO;
import cn.lili.modules.promotion.service.PintuanService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * 店铺端,拼团管理接口
 *
 * @author paulG
 * @since 2020/10/9
 **/
@RestController
@Tag(name = "店铺端,拼团管理接口")
@RequestMapping("/store/promotion/pintuan")
public class PintuanStoreController {

    @Autowired
    private PintuanService pintuanService;
    @Autowired
    private PromotionGoodsService promotionGoodsService;


    @GetMapping
    @Operation(description = "根据条件分页查询拼团活动列表")
    @Parameter(name = "queryParam", description = "拼团活动查询参数")
    @Parameter(name = "pageVo", description = "分页参数")
    public ResultMessage<IPage<Pintuan>> getPintuanByPage(PintuanSearchParams queryParam, PageVO pageVo) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        queryParam.setStoreId(currentUser.getStoreId());
        return ResultUtil.data(pintuanService.pageFindAll(queryParam, pageVo));
    }

    @GetMapping("/{id}")
    @Operation(description = "通过id获取")
    @Parameter(name = "id", description = "拼团活动ID", required = true)
    public ResultMessage<PintuanVO> get(@PathVariable String id) {
        PintuanVO pintuan = OperationalJudgment.judgment(pintuanService.getPintuanVO(id));
        return ResultUtil.data(pintuan);
    }

    @GetMapping("/goods/{pintuanId}")
    @Operation(description = "根据条件分页查询拼团活动商品列表")
    @Parameter(name = "pintuanId", description = "拼团活动ID", required = true)
    @Parameter(name = "pageVo", description = "分页参数")
    public ResultMessage<IPage<PromotionGoods>> getPintuanGoodsByPage(@PathVariable String pintuanId, PageVO pageVo) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
        searchParams.setStoreId(currentUser.getStoreId());
        searchParams.setPromotionId(pintuanId);
        searchParams.setPromotionType(PromotionTypeEnum.PINTUAN.name());
        return ResultUtil.data(promotionGoodsService.pageFindAll(searchParams, pageVo));
    }

    @PostMapping(consumes = "application/json", produces = "application/json")
    @Operation(description = "添加拼团活动")
    @Parameter(name = "pintuan", description = "拼团活动VO", required = true)
    public ResultMessage<String> addPintuan(@RequestBody @Validated PintuanVO pintuan) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        pintuan.setStoreId(currentUser.getStoreId());
        pintuan.setStoreName(currentUser.getStoreName());
        if (pintuanService.savePromotions(pintuan)) {
            return ResultUtil.success(ResultCode.PINTUAN_ADD_SUCCESS);
        }
        throw new ServiceException(ResultCode.PINTUAN_ADD_ERROR);
    }

    @PutMapping(consumes = "application/json", produces = "application/json")
    @Operation(description = "修改拼团活动")
    @Parameter(name = "pintuan", description = "拼团活动VO", required = true)
    public ResultMessage<String> editPintuan(@RequestBody @Validated PintuanVO pintuan) {
        OperationalJudgment.judgment(pintuanService.getById(pintuan.getId()));
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        pintuan.setStoreId(currentUser.getStoreId());
        pintuan.setStoreName(currentUser.getStoreName());
        if (pintuan.getPromotionGoodsList() != null && !pintuan.getPromotionGoodsList().isEmpty()) {
            List<String> skuIds = pintuan.getPromotionGoodsList().stream().map(PromotionGoods::getSkuId).collect(Collectors.toList());
            pintuan.setScopeId(ArrayUtil.join(skuIds.toArray(), ","));
        } else {
            pintuan.setScopeId(null);
        }
        if (pintuanService.updatePromotions(pintuan)) {
            return ResultUtil.success(ResultCode.PINTUAN_EDIT_SUCCESS);
        }
        throw new ServiceException(ResultCode.PINTUAN_EDIT_ERROR);
    }

    @PutMapping("/status/{pintuanId}")
    @Operation(description = "操作拼团活动状态")
    @Parameter(name = "pintuanId", description = "拼团活动ID", required = true)
    @Parameter(name = "startTime", description = "拼团开始时间")
    @Parameter(name = "endTime", description = "拼团结束时间")
    public ResultMessage<String> openPintuan(@PathVariable String pintuanId, Long startTime, Long endTime) {
        OperationalJudgment.judgment(pintuanService.getById(pintuanId));
        if (pintuanService.updateStatus(Collections.singletonList(pintuanId), startTime, endTime)) {
            return ResultUtil.success(ResultCode.PINTUAN_MANUAL_OPEN_SUCCESS);
        }
        throw new ServiceException(ResultCode.PINTUAN_MANUAL_OPEN_ERROR);

    }

    @DeleteMapping("/{pintuanId}")
    @Operation(description = "手动删除拼团活动")
    @Parameter(name = "pintuanId", description = "拼团活动ID", required = true)
    public ResultMessage<String> deletePintuan(@PathVariable String pintuanId) {
        OperationalJudgment.judgment(pintuanService.getById(pintuanId));
        if (pintuanService.removePromotions(Collections.singletonList(pintuanId))) {
            return ResultUtil.success(ResultCode.PINTUAN_DELETE_SUCCESS);
        }
        throw new ServiceException(ResultCode.PINTUAN_DELETE_ERROR);
    }

}
