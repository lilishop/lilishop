package cn.lili.controller.promotion;


import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.KanjiaActivityGoods;
import cn.lili.modules.promotion.entity.dto.KanjiaActivityGoodsDTO;
import cn.lili.modules.promotion.entity.dto.KanjiaActivityGoodsOperationDTO;
import cn.lili.modules.promotion.entity.vos.kanjia.KanjiaActivityGoodsParams;
import cn.lili.modules.promotion.service.KanjiaActivityGoodsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;


/**
 * 管理端,促销接口
 *
 * @author qiuqiu
 * @date 2021/7/2
 **/
@RestController
@Api(tags = "管理端,砍价促销接口")
@RequestMapping("/manager/promotion/kan-jia-goods")
public class KanJiaActivityGoodsManagerController {

    @Autowired
    private KanjiaActivityGoodsService kanJiaActivityGoodsService;

    @PostMapping
    @ApiOperation(value = "添加砍价活动")
    public ResultMessage<Object> add(@RequestBody KanjiaActivityGoodsOperationDTO kanJiaActivityGoodsOperationDTO) {
        kanJiaActivityGoodsService.add(kanJiaActivityGoodsOperationDTO);
        return ResultUtil.success();
    }


    @ApiOperation(value = "获取砍价活动分页")
    @GetMapping
    public ResultMessage<IPage<KanjiaActivityGoods>> getKanJiaActivityPage(KanjiaActivityGoodsParams KanJiaActivityParams, PageVO page) {
        return ResultUtil.data(kanJiaActivityGoodsService.getForPage(KanJiaActivityParams, page));
    }


    @GetMapping("/{id}")
    @ApiOperation(value = "获取积分商品详情")
    public ResultMessage<Object> getPointsGoodsDetail(@PathVariable("id") String goodsId) {
        KanjiaActivityGoodsDTO kanJiaActivityGoodsDTO = kanJiaActivityGoodsService.getKanjiaGoodsDetail(goodsId);
        return ResultUtil.data(kanJiaActivityGoodsDTO);
    }


    @PutMapping
    @ApiOperation(value = "修改砍价商品")
    public ResultMessage<Object> updatePointsGoods(@RequestBody KanjiaActivityGoodsDTO kanJiaActivityGoodsDTO) {
        kanJiaActivityGoodsService.updateKanjiaActivityGoods(kanJiaActivityGoodsDTO);
        return ResultUtil.success();
    }


    @DeleteMapping("/{ids}")
    @ApiOperation(value = "删除砍价商品")
    public ResultMessage<Object> delete(@PathVariable String ids) {
        if (kanJiaActivityGoodsService.deleteKanJiaGoods(Arrays.asList(ids.split(",")))) {
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.KANJIA_GOODS_DELETE_ERROR);
    }


}
