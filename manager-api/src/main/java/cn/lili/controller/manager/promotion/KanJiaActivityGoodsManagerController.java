package cn.lili.controller.manager.promotion;


import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.KanjiaActivityGoods;
import cn.lili.modules.promotion.entity.dto.KanjiaActivityGoodsDTO;
import cn.lili.modules.promotion.entity.dto.KanjiaActivityGoodsOperationDTO;
import cn.lili.modules.promotion.entity.dto.search.KanjiaActivityGoodsParams;
import cn.lili.modules.promotion.service.KanjiaActivityGoodsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;


/**
 * 管理端,促销接口
 *
 * @author qiuqiu
 * @since 2021/7/2
 **/
@RestController
@Tag(name = "管理端,砍价促销接口")
@RequestMapping("/manager/promotion/kanJiaGoods")
public class KanJiaActivityGoodsManagerController {

    @Autowired
    private KanjiaActivityGoodsService kanJiaActivityGoodsService;

    @Operation(description = "添加砍价活动")
    @Parameter(name = "kanJiaActivityGoodsOperationDTO", description = "砍价活动商品操作DTO", required = true)
    @PostMapping
    public ResultMessage<Object> add(@RequestBody KanjiaActivityGoodsOperationDTO kanJiaActivityGoodsOperationDTO) {
        kanJiaActivityGoodsService.add(kanJiaActivityGoodsOperationDTO);
        return ResultUtil.success();
    }


    @Operation(description = "获取砍价活动分页")
    @Parameter(name = "kanJiaParams", description = "砍价活动查询参数")
    @Parameter(name = "page", description = "分页参数")
    @GetMapping
    public ResultMessage<IPage<KanjiaActivityGoods>> getKanJiaActivityPage(KanjiaActivityGoodsParams kanJiaParams, PageVO page) {
        return ResultUtil.data(kanJiaActivityGoodsService.pageFindAll(kanJiaParams, page));
    }


    @Operation(description = "获取砍价活动商品详情")
    @Parameter(name = "id", description = "砍价活动商品ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<Object> getPointsGoodsDetail(@PathVariable("id") String goodsId) {
        KanjiaActivityGoodsDTO kanJiaActivityGoodsDTO = kanJiaActivityGoodsService.getKanjiaGoodsDetail(goodsId);
        return ResultUtil.data(kanJiaActivityGoodsDTO);
    }


    @Operation(description = "修改砍价商品")
    @Parameter(name = "kanJiaActivityGoodsDTO", description = "砍价活动商品DTO", required = true)
    @PutMapping
    public ResultMessage<Object> updatePointsGoods(@RequestBody KanjiaActivityGoodsDTO kanJiaActivityGoodsDTO) {
        if (!kanJiaActivityGoodsService.updateKanjiaActivityGoods(kanJiaActivityGoodsDTO)) {
            return ResultUtil.error(ResultCode.KANJIA_GOODS_UPDATE_ERROR);
        }
        return ResultUtil.success();
    }


    @Operation(description = "删除砍价商品")
    @Parameter(name = "ids", description = "砍价活动商品ID列表，多个用逗号分隔", required = true)
    @DeleteMapping("/{ids}")
    public ResultMessage<Object> delete(@PathVariable String ids) {
        if (kanJiaActivityGoodsService.removePromotions(Arrays.asList(ids.split(",")))) {
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.KANJIA_GOODS_DELETE_ERROR);
    }


}
