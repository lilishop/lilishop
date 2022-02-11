package cn.lili.controller.goods;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.search.entity.dto.HotWordsDTO;
import cn.lili.modules.search.service.EsGoodsSearchService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

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
    public ResultMessage<Object> getHotWords() {
        return ResultUtil.data(esGoodsSearchService.getHotWords(100));
    }

    @ApiOperation(value = "设置热词")
    @PostMapping
    public ResultMessage<Object> paymentForm(@Validated HotWordsDTO hotWords) {
        esGoodsSearchService.setHotWords(hotWords);
        return ResultUtil.success();
    }

    @ApiOperation(value = "设置热词")
    @DeleteMapping("/{words}")
    public ResultMessage<Object> deleteWords(@PathVariable String words) {
        esGoodsSearchService.deleteHotWords(words);
        return ResultUtil.success();
    }

}
