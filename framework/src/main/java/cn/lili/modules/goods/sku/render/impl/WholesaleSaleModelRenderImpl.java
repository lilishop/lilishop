package cn.lili.modules.goods.sku.render.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.lang.Assert;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dos.Wholesale;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.dto.WholesaleDTO;
import cn.lili.modules.goods.entity.enums.GoodsSalesModeEnum;
import cn.lili.modules.goods.service.WholesaleService;
import cn.lili.modules.goods.sku.render.SalesModelRender;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author paulG
 * @since 2022/5/24
 **/
@Component
public class WholesaleSaleModelRenderImpl implements SalesModelRender {
    /**
     * 批发商品
     */
    @Autowired
    private WholesaleService wholesaleService;


    @Override
    @Transactional(rollbackFor = Exception.class)
    public void renderSingle(GoodsSku goodsSku, GoodsOperationDTO goodsOperationDTO) {
        Assert.notEmpty(goodsOperationDTO.getWholesaleList(), "批发规则不能为空");
        this.checkWholesaleList(goodsOperationDTO.getWholesaleList(), goodsSku);
        List<Wholesale> collect = goodsOperationDTO.getWholesaleList().stream().sorted(Comparator.comparing(Wholesale::getPrice)).collect(Collectors.toList());
        wholesaleService.removeByGoodsId(goodsSku.getGoodsId());
        wholesaleService.saveOrUpdateBatch(collect);
        goodsSku.setPrice(collect.get(0).getPrice());
        goodsSku.setCost(collect.get(0).getPrice());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void renderBatch(List<GoodsSku> goodsSkus, GoodsOperationDTO goodsOperationDTO) {
        Assert.notEmpty(goodsOperationDTO.getWholesaleList(), "批发规则不能为空");
        this.checkWholesaleList(goodsOperationDTO.getWholesaleList(), goodsSkus.get(0));
        List<Wholesale> collect = goodsOperationDTO.getWholesaleList().stream().sorted(Comparator.comparing(Wholesale::getPrice)).collect(Collectors.toList());
        for (GoodsSku skus : goodsSkus) {
            skus.setPrice(collect.get(0).getPrice());
            skus.setCost(collect.get(0).getPrice());
        }
        wholesaleService.removeByGoodsId(goodsSkus.get(0).getGoodsId());
        wholesaleService.saveOrUpdateBatch(collect);
    }

    private void checkWholesaleList(List<WholesaleDTO> wholesaleList, GoodsSku goodsSku) {
        if (CollUtil.isEmpty(wholesaleList)) {
            throw new ServiceException(ResultCode.MUST_HAVE_SALES_MODEL);
        }
        for (WholesaleDTO wholesaleDTO : wholesaleList) {
            if (wholesaleDTO.getPrice() == null || wholesaleDTO.getPrice() <= 0 || wholesaleDTO.getNum() == null || wholesaleDTO.getNum() <= 0) {
                throw new ServiceException(ResultCode.HAVE_INVALID_SALES_MODEL);
            }
            if (CharSequenceUtil.isEmpty(wholesaleDTO.getGoodsId())) {
                wholesaleDTO.setGoodsId(goodsSku.getGoodsId());
            }

        }
    }

    @Override
    public String getSalesMode() {
        return GoodsSalesModeEnum.WHOLESALE.name();
    }
}
