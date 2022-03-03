package cn.lili.controller.other;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.page.entity.dto.PageDataDTO;
import cn.lili.modules.page.entity.enums.PageEnum;
import cn.lili.modules.page.entity.vos.PageDataVO;
import cn.lili.modules.page.service.PageDataService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * 买家端,页面接口
 *
 * @author Chopper
 * @since 2020/11/16 10:08 下午
 */
@RestController
@Api(tags = "买家端,页面接口")
@RequestMapping("/buyer/other/pageData")
public class PageBuyerController {

    /**
     * 页面管理
     */
    @Autowired
    private PageDataService pageService;

    @ApiOperation(value = "获取首页数据")
    @GetMapping("/getIndex")
    public ResultMessage<PageDataVO> getIndex(@RequestParam String clientType) {
        PageDataDTO pageDataDTO = new PageDataDTO(PageEnum.INDEX.name());
        pageDataDTO.setPageClientType(clientType);
        return ResultUtil.data(pageService.getPageData(pageDataDTO));
    }

    @ApiOperation(value = "获取页面数据")
    @GetMapping
    public ResultMessage<PageDataVO> get(PageDataDTO pageDataDTO) {
        return ResultUtil.data(pageService.getPageData(pageDataDTO));
    }
}
