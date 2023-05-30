package cn.lili.modules.purchase.serviceimpl;

import cn.lili.modules.purchase.entity.dos.PurchaseOrderItem;
import cn.lili.modules.purchase.mapper.PurchaseOrderItemMapper;
import cn.lili.modules.purchase.service.PurchaseOrderItemService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;


/**
 * 采购单子内容业务层实现
 *
 * @author Bulbasaur
 * @since 2020/11/26 16:13
 */
@Service
public class PurchaseOrderItemServiceImpl extends ServiceImpl<PurchaseOrderItemMapper, PurchaseOrderItem> implements PurchaseOrderItemService {

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean addPurchaseOrderItem(String purchaseOrderId, List<PurchaseOrderItem> purchaseOrderItemList) {
        //添加采购单子内容
        for (PurchaseOrderItem purchaseOrderItem : purchaseOrderItemList) {
            purchaseOrderItem.setPurchaseOrderId(purchaseOrderId);
            this.save(purchaseOrderItem);
        }
        return true;
    }
}