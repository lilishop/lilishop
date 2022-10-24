package cn.lili.controller.store;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.dto.AdminStoreApplyDTO;
import cn.lili.modules.store.entity.dto.StoreEditDTO;
import cn.lili.modules.store.entity.vos.StoreDetailVO;
import cn.lili.modules.store.entity.vos.StoreManagementCategoryVO;
import cn.lili.modules.store.entity.vos.StoreSearchParams;
import cn.lili.modules.store.entity.vos.StoreVO;
import cn.lili.modules.store.service.StoreDetailService;
import cn.lili.modules.store.service.StoreService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * 管理端,店铺管理接口
 *
 * @author Bulbasaur
 * @since 2020/12/6 16:09
 */
@Api(tags = "管理端,店铺管理接口")
@RestController
@RequestMapping("/manager/store/store")
public class StoreManagerController {

    /**
     * 店铺
     */
    @Autowired
    private StoreService storeService;
    /**
     * 店铺详情
     */
    @Autowired
    private StoreDetailService storeDetailService;

    @ApiOperation(value = "获取店铺分页列表")
    @GetMapping("/all")
    public ResultMessage<List<Store>> getAll() {
        return ResultUtil.data(storeService.list(new QueryWrapper<Store>().eq("store_disable", "OPEN")));
    }

    @ApiOperation(value = "获取店铺分页列表")
    @GetMapping
    public ResultMessage<IPage<StoreVO>> getByPage(StoreSearchParams entity, PageVO page) {
        return ResultUtil.data(storeService.findByConditionPage(entity, page));
    }

    @ApiOperation(value = "获取店铺详情")
    @ApiImplicitParam(name = "storeId", value = "店铺ID", required = true, paramType = "path", dataType = "String")
    @GetMapping(value = "/get/detail/{storeId}")
    public ResultMessage<StoreDetailVO> detail(@PathVariable String storeId) {
        return ResultUtil.data(storeDetailService.getStoreDetailVO(storeId));
    }

    @ApiOperation(value = "添加店铺")
    @PostMapping(value = "/add")
    public ResultMessage<Store> add(@Valid AdminStoreApplyDTO adminStoreApplyDTO) {
        return ResultUtil.data(storeService.add(adminStoreApplyDTO));
    }

    @ApiOperation(value = "编辑店铺")
    @ApiImplicitParam(name = "storeId", value = "店铺ID", required = true, paramType = "path", dataType = "String")
    @PutMapping(value = "/edit/{id}")
    public ResultMessage<Store> edit(@PathVariable String id, @Valid StoreEditDTO storeEditDTO) {
        storeEditDTO.setStoreId(id);
        return ResultUtil.data(storeService.edit(storeEditDTO));
    }

    @ApiOperation(value = "审核店铺申请")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "passed", value = "是否通过审核 0 通过 1 拒绝 编辑操作则不需传递", paramType = "query", dataType = "int"),
            @ApiImplicitParam(name = "id", value = "店铺id", required = true, paramType = "path", dataType = "String")
    })
    @PutMapping(value = "/audit/{id}/{passed}")
    public ResultMessage<Object> audit(@PathVariable String id, @PathVariable Integer passed) {
        storeService.audit(id, passed);
        return ResultUtil.success();
    }


    @DemoSite
    @ApiOperation(value = "关闭店铺")
    @ApiImplicitParam(name = "id", value = "店铺id", required = true, dataType = "String", paramType = "path")
    @PutMapping(value = "/disable/{id}")
    public ResultMessage<Store> disable(@PathVariable String id) {
        storeService.disable(id);
        return ResultUtil.success();
    }

    @ApiOperation(value = "开启店铺")
    @ApiImplicitParam(name = "id", value = "店铺id", required = true, dataType = "String", paramType = "path")
    @PutMapping(value = "/enable/{id}")
    public ResultMessage<Store> enable(@PathVariable String id) {
        storeService.enable(id);
        return ResultUtil.success();
    }

    @ApiOperation(value = "查询一级分类列表")
    @ApiImplicitParam(name = "storeId", value = "店铺id", required = true, dataType = "String", paramType = "path")
    @GetMapping(value = "/managementCategory/{storeId}")
    public ResultMessage<List<StoreManagementCategoryVO>> firstCategory(@PathVariable String storeId) {
        return ResultUtil.data(this.storeDetailService.goodsManagementCategory(storeId));
    }


    @ApiOperation(value = "根据会员id查询店铺信息")
    @GetMapping("/{memberId}/member")
    public ResultMessage<Store> getByMemberId(@Valid @PathVariable String memberId) {
        List<Store> list = storeService.list(new QueryWrapper<Store>().eq("member_id", memberId));
        if (list.size() > 0) {
            return ResultUtil.data(list.get(0));
        }
        return ResultUtil.data(null);
    }

    @ApiOperation(value = "将所有店铺导入店员表")
    @PostMapping("store/to/clerk")
    public ResultMessage<Object> storeToClerk(){
        this.storeService.storeToClerk();
        return ResultUtil.success();
    }
}
