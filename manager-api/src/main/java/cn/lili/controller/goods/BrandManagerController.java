package cn.lili.controller.goods;


import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Brand;
import cn.lili.modules.goods.entity.dto.BrandPageDTO;
import cn.lili.modules.goods.entity.vos.BrandVO;
import cn.lili.modules.goods.service.BrandService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;


/**
 * 管理端,品牌接口
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
@RestController
@Api(tags = "管理端,品牌接口")
@RequestMapping("/manager/goods/brand")
public class BrandManagerController {

    /**
     * 品牌
     */
    @Autowired
    private BrandService brandService;

    @ApiOperation(value = "通过id获取")
    @ApiImplicitParam(name = "id", value = "品牌ID", required = true, dataType = "String", paramType = "path")
    @GetMapping(value = "/get/{id}")
    public ResultMessage<Brand> get(@NotNull @PathVariable String id) {
        return ResultUtil.data(brandService.getById(id));
    }

    @GetMapping(value = "/all")
    @ApiOperation(value = "获取所有可用品牌")
    public List<Brand> getAll() {
        List<Brand> list = brandService.list(new QueryWrapper<Brand>().eq("delete_flag", 0));
        return list;
    }

    @ApiOperation(value = "分页获取")
    @GetMapping(value = "/getByPage")
    public ResultMessage<IPage<Brand>> getByPage(BrandPageDTO page) {
        return ResultUtil.data(brandService.getBrandsByPage(page));
    }

    @ApiOperation(value = "新增品牌")
    @PostMapping
    public ResultMessage<BrandVO> save(@Valid BrandVO brand) {
        if (brandService.addBrand(brand)) {
            return ResultUtil.data(brand);
        }
        throw new ServiceException(ResultCode.BRAND_SAVE_ERROR);
    }

    @ApiOperation(value = "更新数据")
    @ApiImplicitParam(name = "id", value = "品牌ID", required = true, dataType = "String", paramType = "path")
    @PutMapping("/{id}")
    public ResultMessage<BrandVO> update(@PathVariable String id, @Valid BrandVO brand) {
        brand.setId(id);
        if (brandService.updateBrand(brand)) {
            return ResultUtil.data(brand);
        }
        throw new ServiceException(ResultCode.BRAND_UPDATE_ERROR);
    }

    @ApiOperation(value = "后台禁用品牌")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "brandId", value = "品牌ID", required = true, dataType = "String", paramType = "path"),
            @ApiImplicitParam(name = "disable", value = "是否可用", required = true, dataType = "boolean", paramType = "query")
    })
    @PutMapping(value = "/disable/{brandId}")
    public ResultMessage<Object> disable(@PathVariable String brandId, @RequestParam Boolean disable) {
        if (brandService.brandDisable(brandId, disable)) {
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.BRAND_DISABLE_ERROR);
    }

    @ApiOperation(value = "批量删除")
    @ApiImplicitParam(name = "ids", value = "品牌ID", required = true, dataType = "String", allowMultiple = true, paramType = "path")
    @DeleteMapping(value = "/delByIds/{ids}")
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        brandService.deleteBrands(ids);
        return ResultUtil.success(ResultCode.SUCCESS);
    }

}
