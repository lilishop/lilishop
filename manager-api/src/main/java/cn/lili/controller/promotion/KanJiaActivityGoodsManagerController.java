package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.KanJiaActivityGoods;
import cn.lili.modules.promotion.entity.dto.KanJiaActivityGoodsDTO;
import cn.lili.modules.promotion.entity.vos.KanJiaActivityGoodsParams;
import cn.lili.modules.promotion.service.KanJiaActivityGoodsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


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
    private KanJiaActivityGoodsService kanJiaActivityGoodsService;

    @PostMapping
    @ApiOperation(value = "添加砍价活动")
    public ResultMessage<Object> add(@RequestBody List<KanJiaActivityGoodsDTO> kanJiaActivityGoodsDTOS) {
        kanJiaActivityGoodsService.add(kanJiaActivityGoodsDTOS);
        return ResultUtil.success();
    }


    @ApiOperation(value = "获取砍价活动分页")
    @GetMapping
    public ResultMessage<IPage<KanJiaActivityGoods>> getKanjiaActivityPage(KanJiaActivityGoodsParams KanJiaActivityParams, PageVO page) {
        return ResultUtil.data(kanJiaActivityGoodsService.getForPage(KanJiaActivityParams, page));
    }


}
