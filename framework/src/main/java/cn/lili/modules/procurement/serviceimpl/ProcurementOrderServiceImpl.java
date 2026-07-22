package cn.lili.modules.procurement.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.procurement.entity.dos.ProcurementOrder;
import cn.lili.modules.procurement.entity.dos.ProcurementOrderItem;
import cn.lili.modules.procurement.entity.dto.AuditActionDTO;
import cn.lili.modules.procurement.entity.dto.ProcurementOrderCreateDTO;
import cn.lili.modules.procurement.entity.enums.ProcurementStatusEnum;
import cn.lili.modules.procurement.entity.params.ProcurementOrderSearchParams;
import cn.lili.modules.procurement.entity.vos.ProcurementOrderVO;
import cn.lili.modules.procurement.mapper.ProcurementOrderMapper;
import cn.lili.modules.procurement.service.ProcurementOrderItemService;
import cn.lili.modules.procurement.service.ProcurementOrderService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 采购单业务实现
 * 实现采购单的创建、提交、审核、关闭、详情与分页查询
 * @author Bulbasaur
 * @since 2025-12-18
 */
@Service
public class ProcurementOrderServiceImpl extends ServiceImpl<ProcurementOrderMapper, ProcurementOrder> implements ProcurementOrderService {

    @Autowired
    private ProcurementOrderItemService orderItemService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ProcurementOrderVO create(ProcurementOrderCreateDTO dto) {
        AuthUser user = UserContext.getCurrentUser();
        if (user == null || user.getStoreId() == null) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        if (CharSequenceUtil.isEmpty(dto.getStockReasonId())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        ProcurementOrder order = new ProcurementOrder();
        order.setOrderSn(SnowFlake.createStr("PO"));
        order.setStoreId(user.getStoreId());
        order.setStoreName(user.getStoreName());
        order.setMakerId(user.getClerkId());
        order.setMakerName(user.getUsername());
        order.setRemark(dto.getRemark());
        order.setStockReasonId(dto.getStockReasonId());
        order.setStatus(ProcurementStatusEnum.DRAFT.name());

        List<ProcurementOrderItem> items = new ArrayList<>();
        int totalQuantity = 0;
        double totalAmount = 0d;
        for (ProcurementOrderCreateDTO.Item i : dto.getItems()) {
            ProcurementOrderItem item = new ProcurementOrderItem();
            item.setGoodsId(i.getGoodsId());
            item.setSkuId(i.getSkuId());
            item.setGoodsName(i.getGoodsName());
            item.setRetailPrice(i.getRetailPrice());
            item.setQuantity(i.getQuantity());
            item.setTaxRate(i.getTaxRate());
            item.setUnitPriceWithTax(i.getUnitPriceWithTax());
            double unitWithoutTax = CurrencyUtil.div(i.getUnitPriceWithTax(), (1 + i.getTaxRate() / 100.0));
            item.setUnitPriceWithoutTax(unitWithoutTax);
            double subtotalWithTax = CurrencyUtil.mul(i.getUnitPriceWithTax(), i.getQuantity());
            double subtotalWithoutTax = CurrencyUtil.mul(unitWithoutTax, i.getQuantity());
            item.setSubtotalWithTax(subtotalWithTax);
            item.setSubtotalWithoutTax(subtotalWithoutTax);
            item.setReceivedQuantity(0);
            items.add(item);
            totalQuantity += i.getQuantity();
            totalAmount = CurrencyUtil.add(totalAmount, subtotalWithTax);
        }
        order.setTotalQuantity(totalQuantity);
        order.setTotalAmount(totalAmount);

        this.save(order);
        orderItemService.saveItems(order.getId(), items);

        ProcurementOrderVO vo = new ProcurementOrderVO();
        BeanUtil.copyProperties(order, vo);
        vo.setItems(items.stream().peek(x -> x.setProcurementOrderId(order.getId())).collect(Collectors.toList()));
        return vo;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean submit(String id) {
        ProcurementOrder order = this.getById(id);
        if (order == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        if (!ProcurementStatusEnum.DRAFT.name().equals(order.getStatus())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        order.setStatus(ProcurementStatusEnum.SUBMITTED.name());
        return this.updateById(order);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean audit(String id, AuditActionDTO action) {
        ProcurementOrder order = this.getById(id);
        if (order == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        AuthUser user = UserContext.getCurrentUser();
        if (user == null || user.getStoreId() == null) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        if (!user.getStoreId().equals(order.getStoreId())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        if (!ProcurementStatusEnum.SUBMITTED.name().equals(order.getStatus())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        order.setAuditTime(new Date());
        order.setAuditorId(user.getClerkId());
        order.setAuditorName(user.getUsername());
        if (Boolean.TRUE.equals(action.getPass())) {
            order.setStatus(ProcurementStatusEnum.PENDING_INBOUND.name());
        } else {
            order.setStatus(ProcurementStatusEnum.REJECTED.name());
        }
        if (CharSequenceUtil.isNotEmpty(action.getRemark())) {
            order.setRemark(action.getRemark());
        }
        return this.updateById(order);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean close(String id) {
        ProcurementOrder order = this.getById(id);
        if (order == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        order.setStatus(ProcurementStatusEnum.CLOSED.name());
        return this.updateById(order);
    }

    @Override
    public ProcurementOrderVO getDetail(String id) {
        ProcurementOrder order = this.getById(id);
        if (order == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        ProcurementOrderVO vo = new ProcurementOrderVO();
        BeanUtil.copyProperties(order, vo);
        List<ProcurementOrderItem> items = orderItemService.list(new LambdaQueryWrapper<ProcurementOrderItem>().eq(ProcurementOrderItem::getProcurementOrderId, id));
        vo.setItems(items);
        return vo;
    }

    @Override
    public IPage<ProcurementOrder> page(ProcurementOrderSearchParams params) {
        QueryWrapper<ProcurementOrder> wrapper = buildQueryWrapper(params, true);
        wrapper.orderByDesc("create_time");
        return this.page(PageUtil.initPage(params), wrapper);
    }

    @Override
    public Map<String, Long> statusCount(ProcurementOrderSearchParams params) {
        QueryWrapper<ProcurementOrder> wrapper = buildQueryWrapper(params, false);
        wrapper.select("status", "COUNT(1) AS cnt");
        wrapper.groupBy("status");
        List<Map<String, Object>> rows = this.listMaps(wrapper);
        Map<String, Long> result = new HashMap<>();
        for (Map<String, Object> row : rows) {
            Object status = row.get("status");
            Object cnt = row.get("cnt");
            if (status != null && cnt != null) {
                result.put(status.toString(), ((Number) cnt).longValue());
            }
        }
        return result;
    }

    private QueryWrapper<ProcurementOrder> buildQueryWrapper(ProcurementOrderSearchParams params, boolean includeStatus) {
        QueryWrapper<ProcurementOrder> wrapper = Wrappers.query();
        AuthUser user = UserContext.getCurrentUser();
        if (user != null && user.getStoreId() != null) {
            wrapper.eq("store_id", user.getStoreId());
        }
        if (CharSequenceUtil.isNotEmpty(params.getOrderSn())) {
            wrapper.eq("order_sn", params.getOrderSn());
        }
        if (includeStatus && CharSequenceUtil.isNotEmpty(params.getStatus())) {
            wrapper.eq("status", params.getStatus());
        }
        if (params.getStartCreateTime() != null) {
            wrapper.ge("create_time", params.getStartCreateTime());
        }
        if (params.getEndCreateTime() != null) {
            wrapper.le("create_time", params.getEndCreateTime());
        }
        if (params.getStartAuditTime() != null) {
            wrapper.ge("audit_time", params.getStartAuditTime());
        }
        if (params.getEndAuditTime() != null) {
            wrapper.le("audit_time", params.getEndAuditTime());
        }
        if (CharSequenceUtil.isNotEmpty(params.getGoodsName())) {
            List<String> orderIds = orderItemService.list(Wrappers.<ProcurementOrderItem>lambdaQuery()
                    .like(ProcurementOrderItem::getGoodsName, params.getGoodsName()))
                    .stream().map(ProcurementOrderItem::getProcurementOrderId).distinct().collect(Collectors.toList());
            if (!orderIds.isEmpty()) {
                wrapper.in("id", orderIds);
            } else {
                wrapper.eq("id", "0");
            }
        }
        return wrapper;
    }
}
