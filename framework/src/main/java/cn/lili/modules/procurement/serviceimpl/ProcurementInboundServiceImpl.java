package cn.lili.modules.procurement.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.goods.entity.dto.GoodsSkuStockDTO;
import cn.lili.modules.goods.entity.enums.GoodsStockTypeEnum;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.procurement.entity.dos.ProcurementInbound;
import cn.lili.modules.procurement.entity.dos.ProcurementInboundItem;
import cn.lili.modules.procurement.entity.dos.ProcurementOrder;
import cn.lili.modules.procurement.entity.dos.ProcurementOrderItem;
import cn.lili.modules.procurement.entity.dto.ProcurementInboundCreateDTO;
import cn.lili.modules.procurement.entity.enums.ProcurementStatusEnum;
import cn.lili.modules.procurement.entity.params.ProcurementInboundSearchParams;
import cn.lili.modules.procurement.mapper.ProcurementInboundMapper;
import cn.lili.modules.procurement.mapper.ProcurementInboundItemMapper;
import cn.lili.modules.procurement.service.ProcurementInboundService;
import cn.lili.modules.procurement.service.ProcurementInboundItemService;
import cn.lili.modules.procurement.service.ProcurementOrderItemService;
import cn.lili.modules.procurement.service.ProcurementOrderService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 采购入库业务实现
 * 实现入库单创建、分页与详情，并更新SKU库存与订单状态
 * @author Bulbasaur
 * @since 2025-12-18
 */
@Service
public class ProcurementInboundServiceImpl extends ServiceImpl<ProcurementInboundMapper, ProcurementInbound> implements ProcurementInboundService {

    @Autowired
    private ProcurementInboundItemService inboundItemService;
    @Autowired
    private ProcurementOrderService orderService;
    @Autowired
    private ProcurementOrderItemService orderItemService;
    @Autowired
    private GoodsSkuService goodsSkuService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ProcurementInbound createInbound(ProcurementInboundCreateDTO dto) {
        AuthUser user = UserContext.getCurrentUser();
        if (user == null || CharSequenceUtil.isEmpty(user.getStoreId())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        ProcurementOrder order = orderService.getById(dto.getProcurementOrderId());
        if (order == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        if (!(ProcurementStatusEnum.PENDING_INBOUND.name().equals(order.getStatus()) || ProcurementStatusEnum.PARTIAL_INBOUND.name().equals(order.getStatus()))) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        List<ProcurementOrderItem> orderItems = orderItemService.list(Wrappers.<ProcurementOrderItem>lambdaQuery().eq(ProcurementOrderItem::getProcurementOrderId, order.getId()));
        ProcurementInbound inbound = new ProcurementInbound();
        inbound.setInboundSn(SnowFlake.createStr("PI"));
        inbound.setStoreId(order.getStoreId());
        inbound.setProcurementOrderId(order.getId());
        inbound.setCertificateImages(dto.getCertificateImages());
        inbound.setOperatorId(user.getClerkId());
        inbound.setOperatorName(user.getUsername());
        inbound.setInboundTime(new Date());

        int expected = orderItems.stream().mapToInt(ProcurementOrderItem::getQuantity).sum();
        int confirmedAlready = orderItems.stream().mapToInt(i -> i.getReceivedQuantity() == null ? 0 : i.getReceivedQuantity()).sum();
        int confirmedThis = 0;
        double totalCost = 0d;
        double totalRetail = 0d;

        List<ProcurementInboundItem> inboundItems = new ArrayList<>();
        List<GoodsSkuStockDTO> stockDTOS = new ArrayList<>();
        for (ProcurementInboundCreateDTO.Item i : dto.getItems()) {
            ProcurementOrderItem matched = orderItems.stream().filter(x -> x.getId().equals(i.getProcurementOrderItemId())).findFirst().orElse(null);
            if (matched == null) {
                throw new ServiceException(ResultCode.PARAMS_ERROR);
            }
            int remained = matched.getQuantity() - (matched.getReceivedQuantity() == null ? 0 : matched.getReceivedQuantity());
            if (i.getInboundQuantity() == null || i.getInboundQuantity() < 0 || i.getInboundQuantity() > remained) {
                throw new ServiceException(ResultCode.PARAMS_ERROR);
            }
            ProcurementInboundItem inboundItem = new ProcurementInboundItem();
            inboundItem.setInboundId(inbound.getId());
            inboundItem.setProcurementOrderItemId(matched.getId());
            inboundItem.setGoodsId(i.getGoodsId());
            inboundItem.setSkuId(i.getSkuId());
            inboundItem.setGoodsName(i.getGoodsName());
            inboundItem.setExpectedQuantity(matched.getQuantity());
            inboundItem.setInboundQuantity(i.getInboundQuantity());
            inboundItems.add(inboundItem);

            matched.setReceivedQuantity((matched.getReceivedQuantity() == null ? 0 : matched.getReceivedQuantity()) + i.getInboundQuantity());
            orderItemService.updateById(matched);

            confirmedThis += i.getInboundQuantity();
            totalCost = CurrencyUtil.add(totalCost, CurrencyUtil.mul(matched.getUnitPriceWithTax(), i.getInboundQuantity()));
            totalRetail = CurrencyUtil.add(totalRetail, CurrencyUtil.mul(matched.getRetailPrice(), i.getInboundQuantity()));

            GoodsSkuStockDTO stockDTO = new GoodsSkuStockDTO();
            String goodsId = CharSequenceUtil.isNotEmpty(matched.getGoodsId()) ? matched.getGoodsId() : i.getGoodsId();
            String skuId = CharSequenceUtil.isNotEmpty(matched.getSkuId()) ? matched.getSkuId() : i.getSkuId();
            if (CharSequenceUtil.isEmpty(skuId)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR);
            }
            GoodsSku goodsSku = goodsSkuService.getById(skuId);
            if (goodsSku == null) {
                throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
            }
            if (CharSequenceUtil.isEmpty(goodsId)) {
                goodsId = goodsSku.getGoodsId();
            }
            stockDTO.setGoodsId(goodsId);
            stockDTO.setSkuId(skuId);
            stockDTO.setQuantity(i.getInboundQuantity());
            stockDTO.setType(GoodsStockTypeEnum.ADD.name());
            stockDTOS.add(stockDTO);
        }
        if (stockDTOS.isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        this.save(inbound);
        inboundItems.forEach(item -> item.setInboundId(inbound.getId()));
        inboundItemService.saveBatch(inboundItems);

        inbound.setExpectedQuantity(expected);
        inbound.setConfirmedQuantity(confirmedAlready + confirmedThis);
        inbound.setPendingQuantity(expected - inbound.getConfirmedQuantity());
        inbound.setTotalCost(totalCost);
        inbound.setTotalRetailAmount(totalRetail);
        this.updateById(inbound);

        goodsSkuService.updateStocksByType(stockDTOS);

        boolean allReceived = orderItems.stream().allMatch(x -> (x.getReceivedQuantity() != null && x.getReceivedQuantity() >= x.getQuantity()));
        order.setStatus(allReceived ? ProcurementStatusEnum.COMPLETED.name() : ProcurementStatusEnum.PARTIAL_INBOUND.name());
        orderService.updateById(order);
        return inbound;
    }

    @Override
    public IPage<ProcurementInbound> page(ProcurementInboundSearchParams params) {
        LambdaQueryWrapper<ProcurementInbound> wrapper = Wrappers.lambdaQuery();
        AuthUser user = UserContext.getCurrentUser();
        if (user != null && CharSequenceUtil.isNotEmpty(user.getStoreId())) {
            wrapper.eq(ProcurementInbound::getStoreId, user.getStoreId());
        }
        if (CharSequenceUtil.isNotEmpty(params.getInboundSn())) {
            wrapper.eq(ProcurementInbound::getInboundSn, params.getInboundSn());
        }
        if (CharSequenceUtil.isNotEmpty(params.getProcurementOrderId())) {
            wrapper.eq(ProcurementInbound::getProcurementOrderId, params.getProcurementOrderId());
        }
        wrapper.orderByDesc(ProcurementInbound::getCreateTime);
        return this.page(PageUtil.initPage(params), wrapper);
    }

    @Override
    public ProcurementInbound getDetail(String id) {
        ProcurementInbound inbound = this.getById(id);
        if (inbound == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        return inbound;
    }
}
