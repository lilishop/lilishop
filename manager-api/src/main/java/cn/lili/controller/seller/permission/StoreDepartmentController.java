package cn.lili.controller.seller.permission;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.member.entity.dos.StoreDepartment;
import cn.lili.modules.member.entity.vo.StoreDepartmentVO;
import cn.lili.modules.member.service.StoreDepartmentService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 管理端,部门管理接口
 *
 * @author Chopper
 * @since 2020/11/22 12:06
 */
@RestController
@Tag(name = "店铺端,部门管理接口")
@RequestMapping("/store/department")
public class StoreDepartmentController {
    @Autowired
    private StoreDepartmentService storeDepartmentService;

    @GetMapping("/{id}")
    @Operation(description = "查看部门详情")
    @Parameter(name = "id", description = "部门ID", required = true)
    public ResultMessage<StoreDepartment> get(@PathVariable String id) {
        StoreDepartment storeDepartment = storeDepartmentService.getById(id);
        return ResultUtil.data(storeDepartment);
    }

    @GetMapping
    @Operation(description = "获取树状结构")
    public ResultMessage<List<StoreDepartmentVO>> getByPage(StoreDepartment entity,
                                                            SearchVO searchVo) {
        entity.setStoreId(UserContext.getCurrentUser().getStoreId());
        return ResultUtil.data(storeDepartmentService.tree(entity, searchVo));

    }

    @PostMapping
    @Operation(description = "新增部门")
    @Parameter(name = "storeDepartment", description = "部门信息", required = true)
    public ResultMessage<StoreDepartment> save(@RequestBody StoreDepartment storeDepartment) {
        storeDepartment.setStoreId(UserContext.getCurrentUser().getStoreId());
        storeDepartmentService.save(storeDepartment);
        return ResultUtil.data(storeDepartment);
    }

    @PutMapping("/{id}")
    @Operation(description = "更新部门")
    @Parameter(name = "id", description = "部门ID", required = true)
    public ResultMessage<StoreDepartment> update(@PathVariable String id, StoreDepartment storeDepartment) {
        storeDepartment.setId(id);
        storeDepartmentService.update(storeDepartment);
        return ResultUtil.data(storeDepartment);
    }

    @DeleteMapping("/{ids}")
    @Operation(description = "删除部门")
    @Parameter(name = "ids", description = "部门ID列表", required = true)
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        storeDepartmentService.deleteByIds(ids);
        return ResultUtil.success();
    }
}
