package cn.lili.controller.store;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.vos.StoreGoodsLabelVO;
import cn.lili.modules.goods.service.StoreGoodsLabelService;
import cn.lili.modules.store.entity.dto.StoreBankDTO;
import cn.lili.modules.store.entity.dto.StoreCompanyDTO;
import cn.lili.modules.store.entity.dto.StoreOtherInfoDTO;
import cn.lili.modules.store.entity.vos.*;
import cn.lili.modules.store.service.StoreDetailService;
import cn.lili.modules.store.service.StoreService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;
import java.util.List;


/**
 * 买家端,店铺接口
 *
 * @author Bulbasaur
 * @since 2020/11/17 2:32 下午
 */
@RestController
@RequestMapping("/buyer/store")
@Api(tags = "买家端,店铺接口")
public class StoreBuyerController {

    /**
     * 店铺
     */
    @Autowired
    private StoreService storeService;
    /**
     * 店铺商品分类
     */
    @Autowired
    private StoreGoodsLabelService storeGoodsLabelService;
    /**
     * 店铺详情
     */
    @Autowired
    private StoreDetailService storeDetailService;

    @ApiOperation(value = "获取店铺列表分页")
    @GetMapping
    public ResultMessage<IPage<StoreVO>> getByPage(StoreSearchParams entity, PageVO page) {
        return ResultUtil.data(storeService.findByConditionPage(entity, page));
    }

    @ApiOperation(value = "通过id获取店铺信息")
    @ApiImplicitParam(name = "id", value = "店铺ID", required = true, paramType = "path")
    @GetMapping(value = "/get/detail/{id}")
    public ResultMessage<StoreBasicInfoVO> detail(@NotNull @PathVariable String id) {
        return ResultUtil.data(storeDetailService.getStoreBasicInfoDTO(id));
    }

    @ApiOperation(value = "通过id获取店铺详细信息-营业执照")
    @ApiImplicitParam(name = "id", value = "店铺ID", required = true, paramType = "path")
    @GetMapping(value = "/get/licencePhoto/{id}")
    public ResultMessage<StoreOtherVO> licencePhoto(@NotNull @PathVariable String id) {
        return ResultUtil.data(storeDetailService.getStoreOtherVO(id));
    }

    @ApiOperation(value = "通过id获取店铺商品分类")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "店铺ID", required = true, paramType = "path")
    })
    @GetMapping(value = "/label/get/{id}")
    public ResultMessage<List<StoreGoodsLabelVO>> storeGoodsLabel(@NotNull @PathVariable String id) {
        return ResultUtil.data(storeGoodsLabelService.listByStoreId(id));
    }

    @ApiOperation(value = "申请店铺第一步-填写企业信息")
    @PutMapping(value = "/apply/first")
    public ResultMessage<Object> applyFirstStep(StoreCompanyDTO storeCompanyDTO) {
        storeService.applyFirstStep(storeCompanyDTO);
        return ResultUtil.success();
    }

    @ApiOperation(value = "申请店铺第二步-填写银行信息")
    @PutMapping(value = "/apply/second")
    public ResultMessage<Object> applyFirstStep(StoreBankDTO storeBankDTO) {
        storeService.applySecondStep(storeBankDTO);
        return ResultUtil.success();
    }

    @ApiOperation(value = "申请店铺第三步-填写其他信息")
    @PutMapping(value = "/apply/third")
    public ResultMessage<Object> applyFirstStep(StoreOtherInfoDTO storeOtherInfoDTO) {
        storeService.applyThirdStep(storeOtherInfoDTO);
        return ResultUtil.success();
    }

    @ApiOperation(value = "获取当前登录会员的店铺信息-入驻店铺")
    @GetMapping(value = "/apply")
    public ResultMessage<StoreDetailVO> apply() {
        return ResultUtil.data(storeDetailService.getStoreDetailVOByMemberId(UserContext.getCurrentUser().getId()));
    }
}
