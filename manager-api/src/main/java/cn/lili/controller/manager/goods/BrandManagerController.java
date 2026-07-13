package cn.lili.controller.manager.goods;


import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Brand;
import cn.lili.modules.goods.entity.dto.BrandPageDTO;
import cn.lili.modules.goods.entity.vos.BrandVO;
import cn.lili.modules.goods.service.BrandService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;
import java.util.List;


/**
 * 管理端,品牌接口
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
@RestController
@Tag(name = "管理端,品牌接口")
@RequestMapping("/manager/goods/brand")
public class BrandManagerController {

    /**
     * 品牌
     */
    @Autowired
    private BrandService brandService;

    @Operation(summary = "通过id获取")
    @Parameter(name = "id", description = "品牌ID", required = true)
    @GetMapping("/get/{id}")
    public ResultMessage<Brand> get(@NotNull @PathVariable String id) {
        return ResultUtil.data(brandService.getById(id));
    }

    @GetMapping("/all")
    @Operation(summary = "获取所有可用品牌")
    public List<Brand> getAll() {
        return brandService.listNotDeleted();
    }

    @Operation(summary = "分页获取")
    @GetMapping("/getByPage")
    public ResultMessage<IPage<Brand>> getByPage(BrandPageDTO page) {
        return ResultUtil.data(brandService.getBrandsByPage(page));
    }

    @Operation(summary = "新增品牌")
    @PostMapping
    public ResultMessage<BrandVO> save(@Valid BrandVO brand) {
        if (brandService.addBrand(brand)) {
            return ResultUtil.data(brand);
        }
        throw new ServiceException(ResultCode.BRAND_SAVE_ERROR);
    }

    @Operation(summary = "更新数据")
    @Parameter(name = "id", description = "品牌ID", required = true)
    @PutMapping("/{id}")
    public ResultMessage<BrandVO> update(@PathVariable String id, @Valid BrandVO brand) {
        brand.setId(id);
        if (brandService.updateBrand(brand)) {
            return ResultUtil.data(brand);
        }
        throw new ServiceException(ResultCode.BRAND_UPDATE_ERROR);
    }

    @Operation(summary = "后台禁用品牌")
    @Parameter(name = "brandId", description = "品牌ID", required = true)
    @Parameter(name = "disable", description = "是否可用", required = true)
    @PutMapping("/disable/{brandId}")
    public ResultMessage<Object> disable(@PathVariable String brandId, @RequestParam Boolean disable) {
        if (brandService.brandDisable(brandId, disable)) {
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.BRAND_DISABLE_ERROR);
    }

    @Operation(summary = "批量删除")
    @Parameter(name = "ids", description = "品牌ID", required = true)
    @DeleteMapping("/delByIds/{ids}")
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        brandService.deleteBrands(ids);
        return ResultUtil.success(ResultCode.SUCCESS);
    }

}
