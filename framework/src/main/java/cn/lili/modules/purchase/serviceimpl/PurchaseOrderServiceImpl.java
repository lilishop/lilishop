package cn.lili.modules.purchase.serviceimpl;

import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.purchase.entity.dos.PurchaseOrder;
import cn.lili.modules.purchase.entity.params.PurchaseOrderSearchParams;
import cn.lili.modules.purchase.entity.vos.PurchaseOrderVO;
import cn.lili.modules.purchase.mapper.PurchaseOrderMapper;
import cn.lili.modules.purchase.service.PurchaseOrderItemService;
import cn.lili.modules.purchase.service.PurchaseOrderService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.Map;


/**
 * 采购单业务层实现
 *
 * @author Bulbasaur
 * @date 2020/11/26 16:13
 */
@Service
public class PurchaseOrderServiceImpl extends ServiceImpl<PurchaseOrderMapper, PurchaseOrder> implements PurchaseOrderService {
    @Autowired
    private PurchaseOrderItemService purchaseOrderItemService;

    @Override
    public PurchaseOrderVO addPurchaseOrder(PurchaseOrderVO purchaseOrderVO) {
        PurchaseOrder purchaseOrder = new PurchaseOrder();
        BeanUtil.copyProperties(purchaseOrderVO, purchaseOrder);
        //添加采购单
        purchaseOrder.setStatus("OPEN");
        this.save(purchaseOrder);
        //添加采购单子内容
        purchaseOrderItemService.addPurchaseOrderItem(purchaseOrder.getId(), purchaseOrderVO.getPurchaseOrderItems());
        return purchaseOrderVO;
    }

    @Override
    public PurchaseOrderVO getPurchaseOrder(String id) {
        PurchaseOrderVO purchaseOrderVO = new PurchaseOrderVO();
        //获取采购单内容
        PurchaseOrder purchaseOrder = this.getById(id);
        BeanUtil.copyProperties(purchaseOrder, purchaseOrderVO);

        //获取采购单子内容
        Map<String, Object> map = new HashMap<>(2);
        map.put("purchaseOrderId", id);
        purchaseOrderVO.setPurchaseOrderItems(purchaseOrderItemService.listByMap(map));
        return purchaseOrderVO;
    }

    @Override
    public IPage<PurchaseOrder> page(PurchaseOrderSearchParams purchaseOrderSearchParams) {

        LambdaQueryWrapper<PurchaseOrder> lambdaQueryWrapper = Wrappers.lambdaQuery();
        if (purchaseOrderSearchParams.getMemberId() != null) {
            lambdaQueryWrapper.eq(PurchaseOrder::getMemberId, purchaseOrderSearchParams.getMemberId());
        }
        if (purchaseOrderSearchParams.getCategoryId() != null) {
            lambdaQueryWrapper.eq(PurchaseOrder::getCategoryId, purchaseOrderSearchParams.getCategoryId());
        }
        if (purchaseOrderSearchParams.getStatus() != null) {
            lambdaQueryWrapper.eq(PurchaseOrder::getStatus, purchaseOrderSearchParams.getStatus());
        }

        Page page = new Page();
        page.setSize(purchaseOrderSearchParams.getPageSize());
        page.setPages(purchaseOrderSearchParams.getPageNumber());
        IPage<PurchaseOrder> purchaseOrders = this.page(page, lambdaQueryWrapper);
        return purchaseOrders;
    }

    @Override
    public boolean close(String id) {
        PurchaseOrder purchaseOrder = this.getById(id);
        purchaseOrder.setStatus("CLOSE");

        UpdateWrapper<PurchaseOrder> updateWrapper = new UpdateWrapper<>();
        updateWrapper.eq("id", id);
        updateWrapper.set("status", "CLOSE");

        return this.update(updateWrapper);
    }
}