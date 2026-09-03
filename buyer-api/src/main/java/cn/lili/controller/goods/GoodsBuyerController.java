package cn.lili.controller.goods;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import cn.lili.modules.search.entity.dos.GoodsRelatedInfo;
import cn.lili.modules.search.entity.dto.GoodsSearchDTO;
import cn.lili.modules.search.service.GoodsSearchService;
import cn.lili.modules.search.service.HotWordsService;
import cn.lili.modules.statistics.aop.PageViewPoint;
import cn.lili.modules.statistics.aop.enums.PageViewEnum;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import jakarta.validation.constraints.NotNull;
import java.util.List;
import java.util.Map;

/**
 * 买家端,商品接口
 *
 * @author Chopper
 * @since 2020/11/16 10:06 下午
 */
@Slf4j
@Tag(name = "买家端,商品接口")
@RestController
@RequestMapping("/buyer/goods/goods")
public class GoodsBuyerController {

    /**
     * 商品
     */
    @Autowired
    private GoodsService goodsService;
    /**
     * 商品SKU
     */
    @Autowired
    private GoodsSkuService goodsSkuService;
    /**
     * 商品搜索
     */
    @Autowired
    private GoodsSearchService goodsSearchService;

    @Autowired
    private HotWordsService hotWordsService;

    @Operation(summary = "通过id获取商品信息")
    @Parameter(name = "goodsId", description = "商品ID", required = true)
    @GetMapping("/get/{goodsId}")
    public ResultMessage<GoodsVO> get(@NotNull(message = "商品ID不能为空") @PathVariable("goodsId") String id) {
        return ResultUtil.data(goodsService.getGoodsVO(id));
    }

    @Operation(summary = "通过id获取商品信息")
    @Parameters({
            @Parameter(name = "goodsId", description = "商品ID", required = true),
            @Parameter(name = "skuId", description = "skuId", required = true)
    })
    @GetMapping("/sku/{goodsId}/{skuId}")
    @PageViewPoint(type = PageViewEnum.SKU, id = "#id")
    public ResultMessage<Map<String, Object>> getSku(@NotNull(message = "商品ID不能为空") @PathVariable("goodsId") String goodsId,
                                                     @NotNull(message = "SKU ID不能为空") @PathVariable("skuId") String skuId) {
        try {
            // 读取选中的列表
            Map<String, Object> map = goodsSkuService.getGoodsSkuDetail(goodsId, skuId);
            return ResultUtil.data(map);
        } catch (ServiceException se) {
            log.info("商品详情不可用 goodsId={}, skuId={}, code={}, msg={}",
                    goodsId, skuId,
                    se.getResultCode() != null ? se.getResultCode() : ResultCode.ERROR,
                    se.getMsg());
            throw se;
        } catch (Exception e) {
            log.error(ResultCode.GOODS_ERROR.message(), e);
            return ResultUtil.error(ResultCode.GOODS_ERROR);
        }

    }

    @Operation(summary = "获取商品分页列表")
    @GetMapping
    public ResultMessage<IPage<Goods>> getByPage(GoodsSearchParams goodsSearchParams) {
        return ResultUtil.data(goodsService.queryByParams(goodsSearchParams));
    }

    @Operation(summary = "获取商品sku列表")
    @GetMapping("/sku")
    public ResultMessage<List<GoodsSku>> getSkuByPage(GoodsSearchParams goodsSearchParams) {
        return ResultUtil.data(goodsSkuService.getGoodsSkuByList(goodsSearchParams));
    }

    @Operation(summary = "商品搜索分页",
            description = "pageSize 上限 100，超出被静默压到 100。"
                    + "promotionType、promotionsId、currentGoodsId、nameIds 当前 MySQL 搜索未实现。")
    @GetMapping("/search")
    public ResultMessage<Page<GoodsIndex>> searchGoods(GoodsSearchDTO goodsSearchParams, PageVO pageVO) {
        // 保持前端传入的驼峰 sort（price/buyCount 等），交由服务端白名单归一
        pageVO.setNotConvert(true);
        return ResultUtil.data(goodsSearchService.searchGoodsByPage(goodsSearchParams, pageVO));
    }

    @Operation(summary = "商品搜索筛选器",
            description = "promotionType、promotionsId、currentGoodsId、nameIds 当前 MySQL 搜索未实现。")
    @GetMapping("/search/related")
    public ResultMessage<GoodsRelatedInfo> searchGoodsRelated(GoodsSearchDTO goodsSearchParams, PageVO pageVO) {
        pageVO.setNotConvert(true);
        // 品牌 / 分类 / 参数选项，与列表共用 WHERE
        GoodsRelatedInfo selector = goodsSearchService.getSelector(goodsSearchParams, pageVO);
        return ResultUtil.data(selector);
    }

    @Operation(summary = "获取搜索热词")
    @GetMapping("/hot-words")
    public ResultMessage<List<String>> getGoodsHotWords(Integer count) {
        List<String> hotWords = hotWordsService.getHotWords(count);
        return ResultUtil.data(hotWords);
    }

}
