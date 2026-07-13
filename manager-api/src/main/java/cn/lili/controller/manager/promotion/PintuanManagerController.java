package cn.lili.controller.manager.promotion;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
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
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;

/**
 * 管理端,平台拼团接口
 *
 * @author paulG
 * @since 2020/10/9
 **/
@RestController
@Tag(name = "管理端,平台拼团接口")
@RequestMapping("/manager/promotion/pintuan")
public class PintuanManagerController {
    @Autowired
    private PintuanService pintuanService;
    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @Operation(summary = "通过id获取")
    @Parameter(name = "id", description = "拼团活动ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<PintuanVO> get(@PathVariable String id) {
        PintuanVO pintuan = pintuanService.getPintuanVO(id);
        return ResultUtil.data(pintuan);
    }

    @Operation(summary = "根据条件分页查询拼团活动列表")
    @Parameter(name = "queryParam", description = "拼团活动查询参数")
    @Parameter(name = "pageVo", description = "分页参数")
    @GetMapping
    public ResultMessage<IPage<Pintuan>> getPintuanByPage(PintuanSearchParams queryParam, PageVO pageVo) {
        IPage<Pintuan> pintuanIPage = pintuanService.pageFindAll(queryParam, pageVo);
        return ResultUtil.data(pintuanIPage);
    }

    @Operation(summary = "根据条件分页查询拼团活动商品列表")
    @Parameter(name = "pintuanId", description = "拼团活动ID", required = true)
    @Parameter(name = "pageVo", description = "分页参数")
    @GetMapping("/goods/{pintuanId}")
    public ResultMessage<IPage<PromotionGoods>> getPintuanGoodsByPage(@PathVariable String pintuanId, PageVO pageVo) {
        PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
        searchParams.setPromotionId(pintuanId);
        searchParams.setPromotionType(PromotionTypeEnum.PINTUAN.name());
        return ResultUtil.data(promotionGoodsService.pageFindAll(searchParams, pageVo));
    }

    @Operation(summary = "操作拼团活动状态")
    @Parameter(name = "pintuanIds", description = "拼团活动ID列表，多个ID用逗号分隔", required = true)
    @Parameter(name = "startTime", description = "拼团开始时间")
    @Parameter(name = "endTime", description = "拼团结束时间")
    @PutMapping("/status/{pintuanIds}")
    public ResultMessage<String> openPintuan(@PathVariable String pintuanIds, Long startTime, Long endTime) {
        if (pintuanService.updateStatus(Arrays.asList(pintuanIds.split(",")), startTime, endTime)) {
            return ResultUtil.success(ResultCode.PINTUAN_MANUAL_OPEN_SUCCESS);
        }
        throw new ServiceException(ResultCode.PINTUAN_MANUAL_OPEN_ERROR);

    }

}
