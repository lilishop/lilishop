package cn.lili.controller.other.distribution;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dto.DistributionGoodsSearchParams;
import cn.lili.modules.distribution.entity.vos.DistributionGoodsVO;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import cn.lili.modules.distribution.service.DistributionSelectedGoodsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.constraints.NotNull;

/**
 * 买家端,分销商品接口
 *
 * @author Bulbasaur
 * @date: 2020/11/16 10:06 下午
 */
@RestController
@Api(tags = "买家端,分销商品接口")
@RequestMapping("/buyer/distributionGoods")
public class DistributionGoodsBuyerController {

    /**
     * 分销商品
     */
    @Autowired
    private DistributionGoodsService distributionGoodsService;
    /**
     * 选择分销商品
     */
    @Autowired
    private DistributionSelectedGoodsService distributionSelectedGoodsService;


    @ApiOperation(value = "获取分销商商品列表")
    @GetMapping
    public ResultMessage<IPage<DistributionGoodsVO>> distributionGoods(DistributionGoodsSearchParams distributionGoodsSearchParams) {
        return ResultUtil.data(distributionGoodsService.goodsPage(distributionGoodsSearchParams));
    }

    @ApiOperation(value = "选择分销商品")
    @ApiImplicitParam(name = "distributionGoodsId", value = "分销ID", required = true, dataType = "String", paramType = "path")
    @GetMapping(value = "/checked/{distributionGoodsId}")
    public ResultMessage<Object> distributionCheckGoods(
            @NotNull(message = "分销商品不能为空") @PathVariable("distributionGoodsId") String distributionGoodsId) {
        if(distributionSelectedGoodsService.add(distributionGoodsId)){
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.ERROR);
    }
}
