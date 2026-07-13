package cn.lili.controller.seller.settings;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.page.entity.dos.PageData;
import cn.lili.modules.page.entity.dto.PageDataDTO;
import cn.lili.modules.page.entity.enums.PageEnum;
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
import java.util.Objects;

/**
 * 店铺端,页面设置管理接口
 *
 * @author paulGao
 * @since 2020-05-06 15:18:56
 */
@RestController
@Tag(name = "店铺端,页面设置管理接口")
@RequestMapping("/store/settings/pageData")
public class PageDataStoreController {

    @Autowired
    private PageDataService pageDataService;

    @Operation(description = "页面列表")
    @Parameter(name = "pageClientType", description = "客户端类型", required = true)
    @Parameter(name = "pageVO", description = "分页VO")
    @GetMapping("/{pageClientType}/pageDataList")
    public ResultMessage<IPage<PageDataListVO>> pageDataList(@PathVariable String pageClientType, @Valid PageVO pageVO) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        PageDataDTO pageDataDTO = new PageDataDTO();
        pageDataDTO.setPageType(PageEnum.STORE.name());
        pageDataDTO.setPageClientType(pageClientType);
        pageDataDTO.setNum(storeId);
        return ResultUtil.data(pageDataService.getPageDataList(pageVO, pageDataDTO));
    }

    @Operation(description = "获取页面信息")
    @Parameter(name = "id", description = "页面ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<PageData> getPageData(@PathVariable String id) {
        //查询当前店铺下的页面数据
        PageData pageData = pageDataService.getStorePageData(UserContext.getCurrentUser().getStoreId(), id);
        return ResultUtil.data(pageData);
    }

    @Operation(description = "添加页面")
    @PostMapping("/save")
    public ResultMessage<PageData> addPageData(@Valid PageData pageData) {
        //添加店铺类型，填写店铺ID
        pageData.setPageType(PageEnum.STORE.name());
        pageData.setNum(UserContext.getCurrentUser().getStoreId());
        return ResultUtil.data(pageDataService.addPageData(pageData));
    }

    @Operation(description = "修改页面")
    @Parameter(name = "id", description = "页面ID", required = true)
    @Parameter(name = "pageData", description = "页面数据")
    @PutMapping("/update/{id}")
    public ResultMessage<PageData> updatePageData(@Valid PageData pageData, @NotNull @PathVariable String id) {
        this.checkAuthority(id);
        pageData.setId(id);
        //添加店铺类型，填写店铺ID
        pageData.setPageType(PageEnum.STORE.name());
        pageData.setNum(UserContext.getCurrentUser().getStoreId());
        return ResultUtil.data(pageDataService.updatePageData(pageData));
    }


    @Operation(description = "发布页面")
    @Parameter(name = "id", description = "页面ID", required = true)
    @PutMapping("/release/{id}")
    public ResultMessage<PageData> release(@PathVariable String id) {
        this.checkAuthority(id);
        return ResultUtil.data(pageDataService.releasePageData(id));
    }

    @Operation(description = "删除页面")
    @Parameter(name = "id", description = "页面ID", required = true)
    @DeleteMapping("/removePageData/{id}")
    public ResultMessage<Object> remove(@PathVariable String id) {
        this.checkAuthority(id);
        return ResultUtil.data(pageDataService.removePageData(id));
    }


    /**
     * 店铺权限判定
     *
     * @param id
     */
    private void checkAuthority(String id) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        pageDataService.checkStoreAuthority(storeId, id);
    }
}
