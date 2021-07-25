package cn.lili.controller.setting;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.search.service.EsGoodsSearchService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端,app版本控制器
 *
 * @author Chopper
 * @since 2018-07-04 21:50:52
 */
@RestController
@Api(tags = "管理端,系统设置扩展接口")
@RequestMapping("/manager/hotwords")
public class HotWordsManagerController {

    @Autowired
    private EsGoodsSearchService esGoodsSearchService;

    @ApiOperation(value = "获取热词")
    @GetMapping
    public ResultMessage getHotWords() {
        return ResultUtil.data(esGoodsSearchService.getHotWords(0, 99));
    }

    @ApiOperation(value = "设置热词")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keywords", value = "关键字"),
            @ApiImplicitParam(name = "point", value = "权重值")
    })
    @PostMapping
    public ResultMessage paymentForm(String keywords, Integer point) {
        esGoodsSearchService.setHotWords(keywords, point);
        return ResultUtil.success();
    }

}
