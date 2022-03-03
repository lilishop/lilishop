package cn.lili.controller.other;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.page.entity.dos.PageData;
import cn.lili.modules.page.entity.dto.PageDataDTO;
import cn.lili.modules.page.entity.enums.PageEnum;
import cn.lili.modules.page.entity.vos.PageDataListVO;
import cn.lili.modules.page.service.PageDataService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/**
 * 店铺端,页面设置管理接口
 *
 * @author paulGao
 * @since 2020-05-06 15:18:56
 */
@RestController
@Api(tags = "店铺端,页面设置管理接口")
@RequestMapping("/store/other/pageData")
public class PageDataStoreController {

    @Autowired
    private PageDataService pageDataService;

    @ApiOperation(value = "获取页面信息")
    @ApiImplicitParam(name = "id", value = "页面ID", required = true, dataType = "String", paramType = "path")
    @GetMapping(value = "/{id}")
    public ResultMessage<PageData> getPageData(@PathVariable String id) {
        //查询当前店铺下的页面数据
        PageData pageData = pageDataService.getOne(
                new LambdaQueryWrapper<PageData>()
                        .eq(PageData::getPageType, PageEnum.STORE.name())
                        .eq(PageData::getNum, UserContext.getCurrentUser().getStoreId())
                        .eq(PageData::getId, id));
        return ResultUtil.data(pageData);
    }

    @ApiOperation(value = "添加页面")
    @PostMapping("/add")
    public ResultMessage<PageData> addPageData(@Valid PageData pageData) {
        //添加店铺类型，填写店铺ID
        pageData.setPageType(PageEnum.STORE.name());
        pageData.setNum(UserContext.getCurrentUser().getStoreId());
        return ResultUtil.data(pageDataService.addPageData(pageData));
    }

    @ApiOperation(value = "修改页面")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "页面ID", required = true, dataType = "String", paramType = "path")
    })
    @DemoSite
    @PutMapping("/update/{id}")
    public ResultMessage<PageData> updatePageData(@Valid PageData pageData, @NotNull @PathVariable String id) {
        pageData.setId(id);
        //添加店铺类型，填写店铺ID
        pageData.setPageType(PageEnum.STORE.name());
        pageData.setNum(UserContext.getCurrentUser().getStoreId());
        return ResultUtil.data(pageDataService.updatePageData(pageData));
    }

    @ApiOperation(value = "页面列表")
    @GetMapping("/pageDataList")
    public ResultMessage<IPage<PageDataListVO>> pageDataList(PageVO pageVO, PageDataDTO pageDataDTO) {
        pageDataDTO.setPageType(PageEnum.STORE.name());
        pageDataDTO.setNum(UserContext.getCurrentUser().getStoreId());
        return ResultUtil.data(pageDataService.getPageDataList(pageVO, pageDataDTO));
    }

    @ApiOperation(value = "发布页面")
    @ApiImplicitParam(name = "id", value = "页面ID", required = true, dataType = "String", paramType = "path")
    @PutMapping("/release/{id}")
    @DemoSite
    public ResultMessage<PageData> release(@PathVariable String id) {
        return ResultUtil.data(pageDataService.releasePageData(id));
    }

    @ApiOperation(value = "删除页面")
    @DemoSite
    @ApiImplicitParam(name = "id", value = "页面ID", required = true, dataType = "String", paramType = "path")
    @DeleteMapping("/remove/{id}")
    public ResultMessage<Object> remove(@PathVariable String id) {
        return ResultUtil.data(pageDataService.removePageData(id));
    }
}
