package cn.lili.controller.manager.store;

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
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;

/**
 * 管理端,店铺管理接口
 *
 * @author Bulbasaur
 * @since 2020/12/6 16:09
 */
@Tag(name = "管理端,店铺管理接口")
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

    @Operation(summary = "获取店铺分页列表")
    @GetMapping("/all")
    public ResultMessage<List<Store>> getAll() {
        return ResultUtil.data(storeService.listOpenStores());
    }

    @Operation(summary = "获取店铺分页列表")
    @Parameter(name = "entity", description = "店铺查询参数", required = true)
    @Parameter(name = "page", description = "分页参数", required = true)
    @GetMapping
    public ResultMessage<IPage<StoreVO>> getByPage(StoreSearchParams entity, PageVO page) {
        return ResultUtil.data(storeService.findByConditionPage(entity, page));
    }

    @Operation(summary = "获取店铺详情")
    @Parameter(name = "storeId", description = "店铺ID", required = true)   
    @GetMapping("/get/detail/{storeId}")
    public ResultMessage<StoreDetailVO> detail(@PathVariable String storeId) {
        // todo 对于刚提交审核的信息需要等待缓存失效后才能操作,否则缓存信息还在
        return ResultUtil.data(storeDetailService.getStoreDetailVO(storeId));
    }

    @Operation(summary = "添加店铺")
    @Parameter(name = "adminStoreApplyDTO", description = "店铺申请参数", required = true)
    @PostMapping("/add")
    public ResultMessage<Store> add(@Valid AdminStoreApplyDTO adminStoreApplyDTO) {
        return ResultUtil.data(storeService.add(adminStoreApplyDTO));
    }

    @Operation(summary = "编辑店铺")
    @Parameter(name = "storeId", description = "店铺ID", required = true)
    @Parameter(name = "storeEditDTO", description = "店铺编辑参数", required = true)
    @PutMapping("/edit/{id}")
    public ResultMessage<Store> edit(@PathVariable String id, @Valid StoreEditDTO storeEditDTO) {
        storeEditDTO.setStoreId(id);
        return ResultUtil.data(storeService.edit(storeEditDTO));
    }

    @Operation(summary = "审核店铺申请")
    @Parameter(name = "passed", description = "是否通过审核 0 通过 1 拒绝 编辑操作则不需传递", required = true)
    @Parameter(name = "id", description = "店铺id", required = true)
    @PutMapping("/audit/{id}/{passed}")
    public ResultMessage<Object> audit(@PathVariable String id, @PathVariable Integer passed) {
        storeService.audit(id, passed);
        return ResultUtil.success();
    }

    @DemoSite
    @Operation(summary = "关闭店铺")
    @Parameter(name = "id", description = "店铺id", required = true)
    @PutMapping("/disable/{id}")
    public ResultMessage<Store> disable(@PathVariable String id) {
        storeService.disable(id);
        return ResultUtil.success();
    }

    @Operation(summary = "开启店铺")
    @Parameter(name = "id", description = "店铺id", required = true)
    @PutMapping("/enable/{id}")
    public ResultMessage<Store> enable(@PathVariable String id) {
        storeService.enable(id);
        return ResultUtil.success();
    }

    @Operation(summary = "查询一级分类列表")
    @Parameter(name = "storeId", description = "店铺id", required = true)
    @GetMapping("/managementCategory/{storeId}")
    public ResultMessage<List<StoreManagementCategoryVO>> firstCategory(@PathVariable String storeId) {
        return ResultUtil.data(this.storeDetailService.goodsManagementCategory(storeId));
    }


    @Operation(summary = "根据客户id查询店铺信息")
    @Parameter(name = "memberId", description = "客户id", required = true)
    @GetMapping("/{memberId}/member")
    public ResultMessage<Store> getByMemberId(@Valid @PathVariable String memberId) {
        return ResultUtil.data(storeService.getStoreByMemberId(memberId));
    }

    @Operation(summary = "将所有店铺导入店员表")
    @PostMapping("store/to/clerk")
    public ResultMessage<Object> storeToClerk(){
        this.storeService.storeToClerk();
        return ResultUtil.success();
    }
}
