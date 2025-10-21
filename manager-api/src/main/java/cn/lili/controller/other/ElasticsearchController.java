package cn.lili.controller.other;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.search.service.EsGoodsIndexService;
import cn.lili.modules.system.aspect.annotation.SystemLogPoint;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

/**
 * ElasticsearchController
 *
 * @author Chopper
 * @version v1.0
 * 2021-03-24 18:32
 */
@RestController
@Api(tags = "ES初始化接口")
@RequestMapping("/manager/other/elasticsearch")
public class ElasticsearchController {

    @Autowired
    private EsGoodsIndexService esGoodsIndexService;

    @GetMapping
    public ResultMessage<String> init() {
        esGoodsIndexService.init();
        return ResultUtil.success();
    }

    @GetMapping("/progress")
    public ResultMessage<Map<String, Long>> getProgress() {
        return ResultUtil.data(esGoodsIndexService.getProgress());
    }


    @ApiOperation(value = "ES删除下架的商品")
    @GetMapping(value = "/deleteGoodsDown")
    @SystemLogPoint(description = "ES删除下架的商品", customerLog = "")
    public ResultMessage<Object> deleteGoodsDown() {

        esGoodsIndexService.deleteGoodsDown();
        return ResultUtil.success();
    }

    @ApiOperation(value = "删除不存在的索引")
    @GetMapping(value = "/delSkuIndex")
    @SystemLogPoint(description = "删除不存在的索引", customerLog = "")
    public ResultMessage<Object> delSkuIndex() {
        esGoodsIndexService.delSkuIndex();
        return ResultUtil.success();
    }

    @ApiOperation(value = "生成所有商品的缓存")
    @GetMapping(value = "/cache")
    @SystemLogPoint(description = "生成所有商品的缓存")
    public ResultMessage<Object> cache() {
        esGoodsIndexService.goodsCache();
        return ResultUtil.success();
    }
}
