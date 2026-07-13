package cn.lili.controller.manager.other;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.page.entity.dos.PageData;
import cn.lili.modules.page.entity.dto.PageDataDTO;
import cn.lili.modules.page.entity.vos.PageDataListVO;
import cn.lili.modules.page.service.PageDataService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;

/**
 * 管理端,页面设置管理接口
 *
 * @author paulGao
 * @since 2020-05-06 15:18:56
 */
@RestController
@Tag(name = "管理端,页面设置管理接口")
@RequestMapping("/manager/other/pageData")
public class PageDataManagerController {

    @Autowired
    private PageDataService pageDataService;

    @Operation(summary = "获取页面信息")
    @GetMapping("/{id}")
    public ResultMessage<PageData> getPageData(
            @Parameter(description = "页面ID", required = true) @PathVariable String id) {
        return ResultUtil.data(pageDataService.getById(id));
    }

    @Operation(summary = "添加页面")
    @PostMapping("/add")
    public ResultMessage<PageData> addPageData(@Valid PageData pageData) {
        return ResultUtil.data(pageDataService.addPageData(pageData));
    }

    @Operation(summary = "修改页面")
    @DemoSite
    @PutMapping("/update/{id}")
    public ResultMessage<PageData> updatePageData(@Valid PageData pageData, 
            @Parameter(description = "页面ID", required = true) @NotNull @PathVariable String id) {
        pageData.setId(id);
        return ResultUtil.data(pageDataService.updatePageData(pageData));
    }

    @Operation(summary = "页面列表")
    @GetMapping("/pageDataList")
    public ResultMessage<IPage<PageDataListVO>> pageDataList(PageVO pageVO, PageDataDTO pageDataDTO) {
        return ResultUtil.data(pageDataService.getPageDataList(pageVO, pageDataDTO));
    }

    @Operation(summary = "发布页面")
    @PutMapping("/release/{id}")
    @DemoSite
    public ResultMessage<PageData> release(
            @Parameter(description = "页面ID", required = true) @PathVariable String id) {
        return ResultUtil.data(pageDataService.releasePageData(id));
    }

    @Operation(summary = "删除页面")
    @DemoSite
    @DeleteMapping("/remove/{id}")
    public ResultMessage<Object> remove(
            @Parameter(description = "页面ID", required = true) @PathVariable String id) {
        return ResultUtil.data(pageDataService.removePageData(id));
    }
}
