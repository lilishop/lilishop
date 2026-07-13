package cn.lili.controller.manager.promotion;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.PointsGoods;
import cn.lili.modules.promotion.entity.dto.search.PointsGoodsSearchParams;
import cn.lili.modules.promotion.entity.vos.PointsGoodsVO;
import cn.lili.modules.promotion.service.PointsGoodsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * 管理端,积分商品接口
 *
 * @author paulG
 * @since 2021/1/14
 **/
@RestController
@Tag(name = "管理端,积分商品接口")
@RequestMapping("/manager/promotion/pointsGoods")
public class PointsGoodsManagerController {
    @Autowired
    private PointsGoodsService pointsGoodsService;

    @Operation(summary = "添加积分商品")
    @Parameter(name = "pointsGoodsList", description = "积分商品列表", required = true)
    @PostMapping(consumes = "application/json", produces = "application/json")
    public ResultMessage<Object> addPointsGoods(@RequestBody List<PointsGoods> pointsGoodsList) {
        if (pointsGoodsService.savePointsGoodsBatch(pointsGoodsList)) {
            return ResultUtil.success();
        }
        return ResultUtil.error(ResultCode.POINT_GOODS_ERROR);
    }

    @Operation(summary = "修改积分商品")
    @Parameter(name = "pointsGoods", description = "积分商品VO", required = true)
    @PutMapping(consumes = "application/json", produces = "application/json")
    public ResultMessage<Object> updatePointsGoods(@RequestBody PointsGoodsVO pointsGoods) {
        Objects.requireNonNull(UserContext.getCurrentUser());
        pointsGoodsService.updatePromotions(pointsGoods);
        return ResultUtil.success();
    }

    @Operation(summary = "修改积分商品状态")
    @Parameter(name = "ids", description = "积分商品ID列表", required = true)
    @Parameter(name = "startTime", description = "开始时间", required = true)
    @Parameter(name = "endTime", description = "结束时间", required = true)
    @PutMapping("/status/{ids}")
    public ResultMessage<Object> updatePointsGoodsStatus(@PathVariable String ids, Long startTime, Long endTime) {
        if (pointsGoodsService.updateStatus(Arrays.asList(ids.split(",")), startTime, endTime)) {
            return ResultUtil.success();
        }
        return ResultUtil.error(ResultCode.POINT_GOODS_ERROR);
    }

    @Operation(summary = "删除积分商品")
    @Parameter(name = "ids", description = "积分商品ID列表", required = true)
    @DeleteMapping("/{ids}")
    public ResultMessage<Object> delete(@PathVariable String ids) {
        if (pointsGoodsService.removePromotions(Arrays.asList(ids.split(",")))) {
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.POINT_GOODS_ERROR);
    }

    @Operation(summary = "分页获取积分商品")
    @Parameter(name = "searchParams", description = "积分商品查询参数", required = true)
    @Parameter(name = "page", description = "分页参数", required = true)
    @GetMapping
    public ResultMessage<IPage<PointsGoods>> getPointsGoodsPage(PointsGoodsSearchParams searchParams, PageVO page) {
        IPage<PointsGoods> pointsGoodsByPage = pointsGoodsService.pageFindAll(searchParams, page);
        return ResultUtil.data(pointsGoodsByPage);
    }

    @Operation(summary = "获取积分商品详情")
    @Parameter(name = "id", description = "积分商品ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<Object> getPointsGoodsDetail(@PathVariable String id) {
        PointsGoodsVO pointsGoodsDetail = pointsGoodsService.getPointsGoodsDetail(id);
        return ResultUtil.data(pointsGoodsDetail);
    }

}
