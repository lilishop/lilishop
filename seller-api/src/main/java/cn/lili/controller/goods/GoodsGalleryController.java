package cn.lili.controller.goods;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.goods.entity.dos.GoodsGallery;
import cn.lili.modules.goods.service.GoodsGalleryService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 店铺端,商品相册接口
 *
 * @author pikachu
 * @date 2020-03-13 11:18:56
 */
@RestController
@Api(tags = "店铺端,商品相册接口")
@RequestMapping("/store/goodsGallery")
public class GoodsGalleryController {
    @Autowired
    private GoodsGalleryService goodsGalleryService;

    @ApiOperation(value = "通过id获取")
    @GetMapping(value = "/get/{id}")
    public ResultMessage<GoodsGallery> get(@PathVariable String id) {

        GoodsGallery goodsGallery = goodsGalleryService.getById(id);
        return ResultUtil.data(goodsGallery);
    }

    @ApiOperation(value = "获取全部数据")
    @GetMapping(value = "/getAll")
    public ResultMessage<List<GoodsGallery>> getAll() {

        List<GoodsGallery> list = goodsGalleryService.list();
        return ResultUtil.data(list);
    }

    @ApiOperation(value = "分页获取")
    @GetMapping(value = "/getByPage")
    public ResultMessage<IPage<GoodsGallery>> getByPage(GoodsGallery entity,
                                                        SearchVO searchVo,
                                                        PageVO page) {
        IPage<GoodsGallery> data = goodsGalleryService.page(PageUtil.initPage(page), PageUtil.initWrapper(entity, searchVo));
        return ResultUtil.data(data);
    }

    @ApiOperation(value = "添加商品相册")
    @PostMapping(value = "/save")
    public ResultMessage<GoodsGallery> save(GoodsGallery goodsGallery) {

        goodsGalleryService.save(goodsGallery);
        return ResultUtil.data(goodsGallery);
    }

    @ApiOperation(value = "修改商品相册")
    @PostMapping(value = "/update")
    public ResultMessage<GoodsGallery> update(GoodsGallery goodsGallery) {

        goodsGalleryService.updateById(goodsGallery);
        return ResultUtil.data(goodsGallery);
    }

    @ApiOperation(value = "批量删除")
    @DeleteMapping(value = "/delByIds/{ids}")
    public ResultMessage<Object> delAllByIds(@PathVariable List ids) {

        goodsGalleryService.removeByIds(ids);
        return ResultUtil.success();
    }
}
