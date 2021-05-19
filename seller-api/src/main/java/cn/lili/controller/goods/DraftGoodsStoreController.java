package cn.lili.controller.goods;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.DraftGoods;
import cn.lili.modules.goods.entity.dto.DraftGoodsDTO;
import cn.lili.modules.goods.entity.dto.DraftGoodsSearchParams;
import cn.lili.modules.goods.entity.vos.DraftGoodsVO;
import cn.lili.modules.goods.service.DraftGoodsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 店铺端,草稿商品接口
 *
 * @author paulG
 * @date 2021/2/20 2:26 下午
 *
 */
@RestController
@Api(tags = "店铺端,草稿商品接口")
@RequestMapping("/store/draft/goods")
public class DraftGoodsStoreController {
    @Autowired
    private DraftGoodsService draftGoodsService;


    @ApiOperation(value = "分页获取草稿商品列表")
    @GetMapping(value = "/page")
    public ResultMessage<IPage<DraftGoods>> getDraftGoodsByPage(DraftGoodsSearchParams searchParams) {
        searchParams.setStoreId(UserContext.getCurrentUser().getStoreId());
        return ResultUtil.data(draftGoodsService.getDraftGoods(searchParams));
    }

    @ApiOperation(value = "获取草稿商品")
    @GetMapping(value = "/{id}")
    public ResultMessage<DraftGoodsVO> getDraftGoods(@PathVariable String id) {
        DraftGoodsVO draftGoods = draftGoodsService.getDraftGoods(id);
        if (!UserContext.getCurrentUser().getStoreId().equals(draftGoods.getStoreId())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        return ResultUtil.data(draftGoods);
    }

    @ApiOperation(value = "保存草稿商品")
    @PostMapping(value = "/save", consumes = "application/json", produces = "application/json")
    public ResultMessage<String> saveDraftGoods(@RequestBody DraftGoodsDTO draftGoodsVO) {
        if (draftGoodsVO.getStoreId() == null) {
            AuthUser currentUser = UserContext.getCurrentUser();
            draftGoodsVO.setStoreId(currentUser.getStoreId());
        } else if (draftGoodsVO.getStoreId() != null && !UserContext.getCurrentUser().getStoreId().equals(draftGoodsVO.getStoreId())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        draftGoodsService.saveGoodsDraft(draftGoodsVO);
        return ResultUtil.success();
    }

    @ApiOperation(value = "删除草稿商品")
    @DeleteMapping(value = "/{id}")
    public ResultMessage<String> deleteDraftGoods(@PathVariable String id) {
        DraftGoods draftGoods = draftGoodsService.getById(id);
        if (!draftGoods.getStoreId().equals(UserContext.getCurrentUser().getStoreId())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        draftGoodsService.deleteGoodsDraft(id);
        return ResultUtil.success();
    }

}
