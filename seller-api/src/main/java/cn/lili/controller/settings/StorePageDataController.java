package cn.lili.controller.settings;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
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
import java.util.Objects;

/**
 * 店铺端,页面接口
 *
 * @author paulGao
 * @since 2020/11/22 14:23
 */
@RestController
@Api(tags = "店铺端,页面接口")
@RequestMapping("/store/settings/pageData")
public class StorePageDataController {
    @Autowired
    private PageDataService pageDataService;

    @ApiOperation(value = "页面列表")
    @ApiImplicitParam(name = "pageClientType", value = "客户端类型", required = true, dataType = "String", paramType = "path")
    @GetMapping("/{pageClientType}/pageDataList")
    public ResultMessage<IPage<PageDataListVO>> pageDataList(@PathVariable String pageClientType, PageVO pageVO) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        PageDataDTO pageDataDTO = new PageDataDTO();
        pageDataDTO.setPageType(PageEnum.STORE.name());
        pageDataDTO.setPageClientType(pageClientType);
        pageDataDTO.setNum(storeId);
        return ResultUtil.data(pageDataService.getPageDataList(pageVO, pageDataDTO));
    }

    @ApiOperation(value = "获取页面信息")
    @ApiImplicitParam(name = "id", value = "页面ID", required = true, dataType = "String", paramType = "path")
    @GetMapping(value = "/{id}")
    public ResultMessage<PageData> getPageData(@PathVariable String id) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        LambdaQueryWrapper<PageData> queryWrapper = new LambdaQueryWrapper<PageData>().eq(PageData::getId, id).eq(PageData::getPageType, PageEnum.STORE.name()).eq(PageData::getNum, storeId);
        return ResultUtil.data(pageDataService.getOne(queryWrapper));
    }

    @ApiOperation(value = "添加店铺首页")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "name", value = "页面名称", required = true, dataType = "String", paramType = "query"),
            @ApiImplicitParam(name = "pageClientType", value = "客户端类型", required = true, dataType = "String", paramType = "query"),
            @ApiImplicitParam(name = "pageData", value = "页面数据", required = true, dataType = "String", paramType = "query")
    })
    @PostMapping("/save")
    public ResultMessage<PageData> savePageData(@RequestParam String name, @RequestParam String pageClientType, @RequestParam String pageData) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(pageDataService.addPageData(new PageData(name, pageClientType, pageData, storeId)));
    }

    @ApiOperation(value = "修改首页")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "页面ID", required = true, dataType = "String", paramType = "path")
    })
    @PutMapping("/update/{id}")
    public ResultMessage<PageData> updatePageData(@Valid PageData pageData, @PathVariable String id) {
        this.checkAuthority(id);
        pageData.setId(id);
        return ResultUtil.data(pageDataService.updatePageData(pageData));
    }

    @ApiOperation(value = "发布页面")
    @ApiImplicitParam(name = "id", value = "页面ID", required = true, dataType = "String", paramType = "path")
    @PutMapping("/release/{id}")
    public ResultMessage<PageData> releasePageData(@PathVariable String id) {
        this.checkAuthority(id);
        return ResultUtil.data(pageDataService.releasePageData(id));
    }

    @ApiOperation(value = "删除页面")
    @ApiImplicitParam(name = "id", value = "页面ID", required = true, dataType = "String", paramType = "path")
    @DeleteMapping("/removePageData/{id}")
    public ResultMessage<Object> removePageData(@PathVariable String id) {
        this.checkAuthority(id);
        return ResultUtil.data(pageDataService.removePageData(id));
    }


    private void checkAuthority(String id) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        LambdaQueryWrapper<PageData> queryWrapper = new LambdaQueryWrapper<PageData>().eq(PageData::getId, id).eq(PageData::getPageType, PageEnum.STORE.name()).eq(PageData::getNum, storeId);
        PageData data = pageDataService.getOne(queryWrapper);
        if (data == null) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
    }
}
