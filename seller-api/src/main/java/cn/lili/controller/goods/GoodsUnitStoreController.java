package cn.lili.controller.goods;


import cn.lili.mybatis.util.PageUtil;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.GoodsUnit;
import cn.lili.modules.goods.service.GoodsUnitService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 店铺端,商品计量单位接口
 *
 * @author Bulbasaur
 * @since 2020/11/26 16:15
 */
@RestController
@Api(tags = "店铺端,商品计量单位接口")
@RequestMapping("/store/goods/unit")
public class GoodsUnitStoreController {
    @Autowired
    private GoodsUnitService goodsUnitService;


    @ApiOperation(value = "分页获取商品计量单位")
    @GetMapping
    public ResultMessage<IPage<GoodsUnit>> getByPage(PageVO pageVO) {
        return ResultUtil.data(goodsUnitService.page(PageUtil.initPage(pageVO)));
    }


}
