package cn.lili.controller.goods;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.fulu.core.utils.Test;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.dto.GoodsOperationFuLuDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.dto.GoodsSkuStockDTO;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.vos.GoodsSkuVO;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.entity.vos.StockWarningVO;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.store.entity.dos.StoreDetail;
import cn.lili.modules.store.entity.dto.FuLuConfigureDTO;
import cn.lili.modules.store.service.StoreDetailService;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 店铺端,商品接口
 *
 * @author pikachu
 * @since 2020-02-23 15:18:56
 */
@RestController
@Api(tags = "店铺端,商品接口")
@RequestMapping("/store/goods/goods")
public class GoodsStoreController {

    /**
     * 商品
     */
    @Autowired
    private GoodsService goodsService;
    /**
     * 商品sku
     */
    @Autowired
    private GoodsSkuService goodsSkuService;
    /**
     * 店铺详情
     */
    @Autowired
    private StoreDetailService storeDetailService;

    @ApiOperation(value = "分页获取商品列表")
    @GetMapping(value = "/list")
    public ResultMessage<IPage<Goods>> getByPage(GoodsSearchParams goodsSearchParams) {
        //获取当前登录商家账号
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        goodsSearchParams.setStoreId(storeId);
        return ResultUtil.data(goodsService.queryByParams(goodsSearchParams));
    }

    @ApiOperation(value = "分页获取商品Sku列表")
    @GetMapping(value = "/sku/list")
    public ResultMessage<IPage<GoodsSku>> getSkuByPage(GoodsSearchParams goodsSearchParams) {
        //获取当前登录商家账号
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        goodsSearchParams.setStoreId(storeId);
        return ResultUtil.data(goodsSkuService.getGoodsSkuByPage(goodsSearchParams));
    }

    @ApiOperation(value = "分页获取库存告警商品列表")
    @GetMapping(value = "/list/stock")
    public ResultMessage<StockWarningVO> getWarningStockByPage(GoodsSearchParams goodsSearchParams) {
        //获取当前登录商家账号
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        StoreDetail storeDetail = OperationalJudgment.judgment(storeDetailService.getStoreDetail(storeId));
        Integer stockWarnNum = storeDetail.getStockWarning();
        goodsSearchParams.setStoreId(storeId);
        goodsSearchParams.setLeQuantity(stockWarnNum);
        goodsSearchParams.setMarketEnable(GoodsStatusEnum.UPPER.name());
        IPage<GoodsSku> goodsSku = goodsSkuService.getGoodsSkuByPage(goodsSearchParams);
        StockWarningVO stockWarning = new StockWarningVO(stockWarnNum, goodsSku);
        return ResultUtil.data(stockWarning);
    }


    @ApiOperation(value = "通过id获取")
    @GetMapping(value = "/get/{id}")
    public ResultMessage<GoodsVO> get(@PathVariable String id) {
        GoodsVO goods = OperationalJudgment.judgment(goodsService.getGoodsVO(id));
        return ResultUtil.data(goods);
    }

    @ApiOperation(value = "新增商品")
    @PostMapping(value = "/create", consumes = "application/json", produces = "application/json")
    public ResultMessage<GoodsOperationDTO> save(@Valid @RequestBody GoodsOperationDTO goodsOperationDTO) {
        goodsService.addGoods(goodsOperationDTO);
        return ResultUtil.success();
    }

    @ApiOperation(value = "修改商品")
    @PutMapping(value = "/update/{goodsId}", consumes = "application/json", produces = "application/json")
    public ResultMessage<GoodsOperationDTO> update(@RequestBody GoodsOperationDTO goodsOperationDTO, @PathVariable String goodsId) {
        goodsService.editGoods(goodsOperationDTO, goodsId);
        return ResultUtil.success();
    }

    @ApiOperation(value = "下架商品", notes = "下架商品时使用")
    @ApiImplicitParam(name = "goodsId", value = "商品ID", required = true, paramType = "query", allowMultiple = true)
    @PutMapping(value = "/under")
    public ResultMessage<Object> underGoods(@RequestParam List<String> goodsId) {

        goodsService.updateGoodsMarketAble(goodsId, GoodsStatusEnum.DOWN, "商家下架");
        return ResultUtil.success();
    }

    @ApiOperation(value = "上架商品", notes = "上架商品时使用")
    @PutMapping(value = "/up")
    @ApiImplicitParam(name = "goodsId", value = "商品ID", required = true, paramType = "query", allowMultiple = true)
    public ResultMessage<Object> unpGoods(@RequestParam List<String> goodsId) {
        goodsService.updateGoodsMarketAble(goodsId, GoodsStatusEnum.UPPER, "");
        return ResultUtil.success();
    }

    @ApiOperation(value = "删除商品")
    @PutMapping(value = "/delete")
    @ApiImplicitParam(name = "goodsId", value = "商品ID", required = true, paramType = "query", allowMultiple = true)
    public ResultMessage<Object> deleteGoods(@RequestParam List<String> goodsId) {
        goodsService.deleteGoods(goodsId);
        return ResultUtil.success();
    }

    @ApiOperation(value = "设置商品运费模板")
    @PutMapping(value = "/freight")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "goodsId", value = "商品ID", required = true, paramType = "query", allowMultiple = true),
            @ApiImplicitParam(name = "templateId", value = "运费模板ID", required = true, paramType = "query")
    })
    public ResultMessage<Object> freight(@RequestParam List<String> goodsId, @RequestParam String templateId) {
        goodsService.freight(goodsId, templateId);
        return ResultUtil.success();
    }

    @ApiOperation(value = "根据goodsId分页获取商品规格列表")
    @GetMapping(value = "/sku/{goodsId}/list")
    public ResultMessage<List<GoodsSkuVO>> getSkuByList(@PathVariable String goodsId) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(goodsSkuService.getGoodsSkuVOList(goodsSkuService.list(new LambdaQueryWrapper<GoodsSku>().eq(GoodsSku::getGoodsId, goodsId).eq(GoodsSku::getStoreId, storeId))));
    }

    @ApiOperation(value = "修改商品库存")
    @PutMapping(value = "/update/stocks", consumes = "application/json")
    public ResultMessage<Object> updateStocks(@RequestBody List<GoodsSkuStockDTO> updateStockList) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        // 获取商品skuId集合
        List<String> goodsSkuIds = updateStockList.stream().map(GoodsSkuStockDTO::getSkuId).collect(Collectors.toList());
        // 根据skuId集合查询商品信息
        List<GoodsSku> goodsSkuList = goodsSkuService.list(new LambdaQueryWrapper<GoodsSku>().in(GoodsSku::getId, goodsSkuIds).eq(GoodsSku::getStoreId, storeId));
        // 过滤不符合当前店铺的商品
        List<String> filterGoodsSkuIds = goodsSkuList.stream().map(GoodsSku::getId).collect(Collectors.toList());
        List<GoodsSkuStockDTO> collect = updateStockList.stream().filter(i -> filterGoodsSkuIds.contains(i.getSkuId())).collect(Collectors.toList());
        goodsSkuService.updateStocks(collect);
        return ResultUtil.success();
    }


    @ApiOperation(value = "新增商品(minShengLotteryRecordService商品迁移）")
    @PostMapping(value = "/fuluCreate")
    public ResultMessage<GoodsOperationDTO> fuluSave() throws Exception {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        GoodsOperationFuLuDTO goodsOperationDTO = new GoodsOperationFuLuDTO();

        FuLuConfigureDTO fuLuConfigureDTO = storeDetailService.getFuLuConfigureDTO();
        //获取并解析福禄商品数据
        Map maps = (Map) JSON.parse(Test.getGoodsInfoList(fuLuConfigureDTO));
        if (!maps.get("code").toString().equals("0")) {
            return ResultUtil.error(201, "商品不存在或配置参数错误");
        }
        int i = 0;
        List<Map<String, String>> listObjectSec = JSONArray.parseObject(maps.get("result").toString(), List.class);
        for (Map<String, String> mapList : listObjectSec) {
            if (listObjectSec.size() <= 0) {
                break;
            }
            for (Map.Entry entry : mapList.entrySet()) {
                if (entry.getKey().equals("product_id")) {
                    Map map1 = (Map) JSON.parse(Test.productInfoGetTest(fuLuConfigureDTO, entry.getValue().toString()).get("result").toString());
                    if (map1.get("product_type").toString().equals("直充")) {
                        goodsOperationDTO.setCategoryPath("1496301573515636737");//分类path
                    } else if (map1.get("product_type").toString().equals("卡密")) {
                        goodsOperationDTO.setCategoryPath("1496301573515636737");//分类path
                    } else {
                        //不是直充也不是卡密需要修改代码
                        continue;
                    }

                    List<Goods> goodsList = goodsService.list(
                            new LambdaQueryWrapper<Goods>()
                                    .eq(Goods::getStoreId, storeId)
                                    .eq(Goods::getSn, map1.get("product_id").toString())
                                    .eq(Goods::getDeleteFlag, false)
                    );

                    goodsOperationDTO.setStoreCategoryPath("");//店铺分类id
                    goodsOperationDTO.setBrandId("1496301301183672321");//品牌ID
                    goodsOperationDTO.setGoodsName(map1.get("product_name").toString());//商品名称
                    goodsOperationDTO.setSn(map1.get("product_id").toString());//商品编号
                    goodsOperationDTO.setPrice(Double.valueOf(map1.get("purchase_price").toString()));//商品价格
                    goodsOperationDTO.setCost(Double.valueOf(map1.get("purchase_price").toString()));//市场价格
                    goodsOperationDTO.setWeight(Double.valueOf("0"));//重量
                    goodsOperationDTO.setIntro(map1.get("product_name").toString());//详情
                    goodsOperationDTO.setMobileIntro(map1.get("product_name").toString());//移动端详情
                    goodsOperationDTO.setQuantity(99999999);//库存
                    goodsOperationDTO.setRelease(true);//是否立即发布
                    goodsOperationDTO.setRecommend(true);//是否是推荐商品
                    goodsOperationDTO.setGoodsParamsDTOList(new ArrayList<>());//商品参数
                    List<String> goodsGalleryList = new ArrayList<>();
                    if (map1.containsKey("four_category_icon") && map1.get("four_category_icon") != null) {
                        goodsGalleryList.add(map1.get("four_category_icon").toString());
                    }
                    goodsOperationDTO.setGoodsGalleryList(goodsGalleryList);
                    goodsOperationDTO.setTemplateId("0");//运费模板id,不需要运费模板时值是0
                    goodsOperationDTO.setSellingPoint(map1.get("product_name").toString());
                    goodsOperationDTO.setSalesModel("RETAIL");//销售模式
//                    goodsOperationDTO.setHaveSpec();//是否有规格
                    goodsOperationDTO.setGoodsUnit("个");//销售模式1471044596808024065
                    goodsOperationDTO.setInfo(map1.get("product_name").toString());//商品描述
                    goodsOperationDTO.setGoodsType("VIRTUAL_GOODS");//商品类型


//                    goodsOperationDTO.setGoodsVideo();//商品视频

                    List<Map<String, Object>> mapArrayList = new ArrayList<>();
                    Map<String, Object> map = new HashMap<>();
                    map.put("sn", goodsOperationDTO.getSn());
                    map.put("price", goodsOperationDTO.getPrice());
                    map.put("cost", goodsOperationDTO.getCost());
                    map.put("weight", goodsOperationDTO.getWeight());
                    map.put("quantity", goodsOperationDTO.getQuantity());
                    map.put("category_path", "1496301573515636737");

                    if (map1.containsKey("four_category_icon") && map1.get("four_category_icon") != null) {
                        List<Map<String, String>> images = new ArrayList<>();
                        Map<String,String> map2 = new HashMap<>();
                        map2.put("url",map1.get("four_category_icon").toString());
                        images.add(map2);
                        map.put("images", images);
                        i += 1;
                    }

                    mapArrayList.add(map);
                    goodsOperationDTO.setSkuList(mapArrayList);//sku列表

                    if (goodsList.size() > 0) {
                        GoodsOperationDTO goodsOperationDTO1 = new GoodsOperationDTO();
                        goodsOperationDTO1.setPrice(Double.valueOf(map1.get("purchase_price").toString()));
                        goodsService.editGoods(goodsOperationDTO1, goodsList.get(0).getId());
                    }else{
                        goodsService.fuLuAddGoods(goodsOperationDTO);
                    }



                    break;
                }
//                if (i == 1) {
//                    break;
//                }
            }
        }
        return ResultUtil.success();
    }


}
