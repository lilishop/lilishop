package cn.lili.modules.purchase.serviceimpl;

import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.purchase.entity.dos.PurchaseOrder;
import cn.lili.modules.purchase.entity.dos.PurchaseOrderItem;
import cn.lili.modules.purchase.entity.params.PurchaseOrderSearchParams;
import cn.lili.modules.purchase.entity.vos.PurchaseOrderVO;
import cn.lili.modules.purchase.mapper.PurchaseOrderMapper;
import cn.lili.modules.purchase.service.PurchaseOrderItemService;
import cn.lili.modules.purchase.service.PurchaseOrderService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


/**
 * 采购单业务层实现
 *
 * @author Bulbasaur
 * @since 2020/11/26 16:13
 */
@Service
public class PurchaseOrderServiceImpl extends ServiceImpl<PurchaseOrderMapper, PurchaseOrder> implements PurchaseOrderService {
    @Autowired
    private PurchaseOrderItemService purchaseOrderItemService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public PurchaseOrderVO addPurchaseOrder(PurchaseOrderVO purchaseOrderVO) {
        PurchaseOrder purchaseOrder = new PurchaseOrder();
        BeanUtil.copyProperties(purchaseOrderVO, purchaseOrder);
        //添加采购单
        purchaseOrder.setStatus("OPEN");
        purchaseOrder.setMemberId(UserContext.getCurrentUser().getId());
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
        purchaseOrderVO.setPurchaseOrderItems(purchaseOrderItemService.list(
                new LambdaQueryWrapper<PurchaseOrderItem>().eq(PurchaseOrderItem::getPurchaseOrderId,id)));
        return purchaseOrderVO;
    }

    @Override
    public IPage<PurchaseOrder> page(PurchaseOrderSearchParams purchaseOrderSearchParams) {

        LambdaQueryWrapper<PurchaseOrder> lambdaQueryWrapper = Wrappers.lambdaQuery();

        lambdaQueryWrapper.eq(purchaseOrderSearchParams.getMemberId() != null,
                PurchaseOrder::getMemberId, purchaseOrderSearchParams.getMemberId());
        lambdaQueryWrapper.eq(purchaseOrderSearchParams.getCategoryId() != null,
                PurchaseOrder::getCategoryId, purchaseOrderSearchParams.getCategoryId());
        lambdaQueryWrapper.eq(purchaseOrderSearchParams.getStatus() != null,
                PurchaseOrder::getStatus, purchaseOrderSearchParams.getStatus());
        lambdaQueryWrapper.orderByDesc(PurchaseOrder::getCreateTime);
        return this.page(PageUtil.initPage(purchaseOrderSearchParams), lambdaQueryWrapper);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean close(String id) {
        PurchaseOrder purchaseOrder = this.getById(id);
        purchaseOrder.setStatus("CLOSE");

        UpdateWrapper<PurchaseOrder> updateWrapper = new UpdateWrapper<>();
        updateWrapper.eq("id", id);
        updateWrapper.set("status", "CLOSE");

        return this.update(updateWrapper);
    }
}