package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.vos.PointsGoodsSearchParams;
import cn.lili.modules.promotion.entity.vos.PointsGoodsVO;
import cn.lili.modules.promotion.service.PointsGoodsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * 管理端,积分商品接口
 *
 * @author paulG
 * @date 2021/1/14
 **/
@RestController
@Api(tags = "管理端,积分商品接口")
@RequestMapping("/manager/promotion/pointsGoods")
public class PointsGoodsManagerController {
    @Autowired
    private PointsGoodsService pointsGoodsService;

    @PostMapping(consumes = "application/json", produces = "application/json")
    @ApiOperation(value = "添加积分商品")
    public ResultMessage<Object> addPointsGoods(@RequestBody List<PointsGoodsVO> pointsGoodsList) {
        List<PointsGoodsVO> collect = new ArrayList<>();
        for (PointsGoodsVO i : pointsGoodsList) {
            i.setStoreName(i.getGoodsSku().getStoreName());
            i.setStoreId(UserContext.getCurrentUser().getStoreId());
            collect.add(i);
        }
        pointsGoodsService.addPointsGoods(collect);
        return ResultUtil.success();
    }

    @PutMapping(consumes = "application/json", produces = "application/json")
    @ApiOperation(value = "修改积分商品")
    public ResultMessage<Object> updatePointsGoods(@RequestBody PointsGoodsVO pointsGoods) {
        AuthUser currentUser = UserContext.getCurrentUser();
        pointsGoods.setStoreId(currentUser.getId());
        pointsGoods.setStoreName("platform");
        pointsGoodsService.updatePointsGoods(pointsGoods);
        return ResultUtil.success();
    }

    @PutMapping("/{ids}")
    @ApiOperation(value = "修改积分商品状态")
    public ResultMessage<Object> updatePointsGoodsStatus(@PathVariable String ids, String promotionStatus) {
        if (pointsGoodsService.updatePointsGoodsPromotionStatus(Arrays.asList(ids.split(",")), promotionStatus)) {
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.POINT_GOODS_ERROR);
    }

    @DeleteMapping("/{ids}")
    @ApiOperation(value = "删除积分商品")
    public ResultMessage<Object> delete(@PathVariable String ids) {
        if (pointsGoodsService.deletePointsGoods(Arrays.asList(ids.split(",")))) {
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.POINT_GOODS_ERROR);
    }

    @GetMapping
    @ApiOperation(value = "分页获取积分商品")
    public ResultMessage<IPage<PointsGoodsVO>> getPointsGoodsPage(PointsGoodsSearchParams searchParams, PageVO page) {
        IPage<PointsGoodsVO> pointsGoodsByPage = pointsGoodsService.getPointsGoodsByPage(searchParams, page);
        return ResultUtil.data(pointsGoodsByPage);
    }

    @GetMapping("/{id}")
    @ApiOperation(value = "获取积分商品详情")
    public ResultMessage<Object> getPointsGoodsDetail(@PathVariable String id) {
        PointsGoodsVO pointsGoodsDetail = pointsGoodsService.getPointsGoodsDetail(id);
        return ResultUtil.data(pointsGoodsDetail);
    }

}
